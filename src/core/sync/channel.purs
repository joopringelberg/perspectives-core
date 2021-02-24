-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

-- | This module defines External Core functions for model:Couchdb.

module Perspectives.Sync.Channel where

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Object (fromFoldable)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, MPT, (##=), (##>), (##>>), type (~~>))
import Perspectives.Couchdb (selectOnFieldEqual, selectOnFieldNotEqual)
import Perspectives.Couchdb.Databases (createDatabase, deleteDocument_, endReplication, ensureAuthentication, replicateContinuously)
import Perspectives.Guid (guid)
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter, disjunction)
import Perspectives.Instances.ObjectGetters (bottom, externalRole, isMe)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Persistence.API (getSystemIdentifier)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.SerializableNonEmptyArray (singleton) as SNA
import Perspectives.User (getCouchdbBaseURL, getHost, getPort)
import Prelude (Unit, bind, discard, not, pure, show, unit, void, ($), (<<<), (<>), (==), (>=>), (>>=))

-- | Create a new database for the communication between `me` and another user.
-- | Create an instance of sys:Channel. Bind `me` in the role Initiator. Set the value of ChannelDatabaseName to
-- | that of the new database.
createChannel :: MonadPerspectivesTransaction ContextInstance
createChannel = do
  g <- liftEffect guid
  channelName <- pure ("channel_" <> (show g))
  channel <- createChannelContext channelName
  lift2 $ ensureAuthentication (createDatabase channelName)
  pure channel

createChannelContext :: String -> MonadPerspectivesTransaction ContextInstance
createChannelContext channelName = do
  host <- lift2 getHost
  port <- lift2 getPort
  eChannel <- runExceptT $ constructContext (Just $ ENR $ EnumeratedRoleType "model:System$Invitation$PrivateChannel") $ ContextSerialization
    { id: "model:User$" <> channelName
    , prototype: Nothing
    , ctype: "sys:Channel"
    , rollen: fromFoldable [(Tuple "model:System$Channel$Initiator" $
      SNA.singleton (RolSerialization
        { id: Nothing
        , properties: PropertySerialization $ fromFoldable
          [
            Tuple "model:System$PhysicalContext$UserWithAddress$Host" [host]
          , Tuple "model:System$PhysicalContext$UserWithAddress$Port" [(show port)]
          ]
        , binding: Just "usr:Me" })
       )]
    , externeProperties: PropertySerialization $ fromFoldable [Tuple "model:System$Channel$External$ChannelDatabaseName" [channelName]]
    }
  case eChannel of
    Left e -> throwError (error ("createChannel could not create channel: " <> show e))
    Right (channel :: ContextInstance) -> pure channel

-- | Add the second user to the channel: not the Initiator, but the ConnectedPartner.
addPartnerToChannel :: RoleInstance -> ContextInstance -> Host -> Port -> MonadPerspectivesTransaction Unit
addPartnerToChannel (RoleInstance usr) (ContextInstance channel) host port = void $ createAndAddRoleInstance (EnumeratedRoleType "model:System$Channel$ConnectedPartner")
  channel
  (RolSerialization
    { id: Nothing
    , properties: PropertySerialization $ fromFoldable
      [ Tuple "model:System$PhysicalContext$UserWithAddress$Host" [host]
      , Tuple "model:System$PhysicalContext$UserWithAddress$Port" [show port]
      ],
    binding: Just usr})

type Host = String
type Port = Int
type Port_ = String

-- | Get either the Initiator or the ConnectedPartner - whomever is filled by me.
getMeFromChannel :: MonadPerspectives (ContextInstance ~~> RoleInstance)
getMeFromChannel = do
  getConnectedPartner <- (getRoleFunction "sys:Channel$ConnectedPartner")
  getInitiator <- (getRoleFunction "sys:Channel$Initiator")
  pure $ disjunction
    (filter getConnectedPartner (lift <<< lift <<< isMe))
    (filter getInitiator (lift <<< lift <<< isMe))

-- | Get either the Initiator or the ConnectedPartner - whomever is NOT filled by me.
getYouFromChannel :: MonadPerspectives (ContextInstance ~~> RoleInstance)
getYouFromChannel = do
  getConnectedPartner <- (getRoleFunction "sys:Channel$ConnectedPartner")
  getInitiator <- (getRoleFunction "sys:Channel$Initiator")
  pure $ disjunction
    (filter getConnectedPartner (lift <<< lift <<< isMe >=> pure <<< not))
    (filter getInitiator (lift <<< lift <<< isMe >=> pure <<< not))

setAddress :: Host -> Port -> Array RoleInstance -> MPT Unit
setAddress host port rl = do
  setProperty rl (EnumeratedPropertyType "model:System$PhysicalContext$UserWithAddress$Host") [Value host]
  setProperty rl (EnumeratedPropertyType "model:System$PhysicalContext$UserWithAddress$Port") [Value (show port)]

setRelayAddress :: Host -> Port -> Array RoleInstance -> MPT Unit
setRelayAddress host port rl = do
  setProperty rl (EnumeratedPropertyType "model:System$PhysicalContext$UserWithAddress$RelayHost") [Value host]
  setProperty rl (EnumeratedPropertyType "model:System$PhysicalContext$UserWithAddress$RelayPort") [Value (show port)]

-- | Regardless whether I am the Initiator or the ConnectedPartner, set my host and port value.
setMyAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setMyAddress host port channel = do
  g <- lift $ lift getMeFromChannel
  (lift $ lift (channel ##= g)) >>= setAddress host port

-- | Regardless whether you are the Initiator or the ConnectedPartner, set your host and port value.
setYourAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setYourAddress host port channel = do
  g <- lift $ lift getYouFromChannel
  (lift $ lift (channel ##= g)) >>= setAddress host port

setMyRelayAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setMyRelayAddress host port channel = do
  g <- lift $ lift getMeFromChannel
  (lift $ lift (channel ##= g)) >>= setRelayAddress host port

setYourRelayAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setYourRelayAddress host port channel = do
  g <- lift $ lift getYouFromChannel
  (lift $ lift (channel ##= g)) >>= setRelayAddress host port

-- | For a channel, set the replication of the local copy to the database found at either Host or RelayHost.
-- | If host and port are equal for both partners, do not set replication.
-- | Also set replication for the channel to the post database. This replication filters out just transactions
-- | whose author is the ConnectedPartner (so I do not have to deal with transactions I've created myself).
-- TODO. Zodra MonadPerspectives gestapeld is op ExceptT, gebruik dan throwError in plaats van pure unit.
setChannelReplication :: ContextInstance -> MonadPerspectives Unit
setChannelReplication channel = do
  getChannelId <- getPropertyFunction "model:System$Channel$External$ChannelDatabaseName"
  mchannel <- channel ##> externalRole >=> getChannelId
  post <- postDbName
  case mchannel of
    Nothing -> pure unit
    Just (Value channelId) -> do
      getYou <- getRoleFunction "sys:Channel$You"
      myou <- channel ##> getYou
      case myou of
        Nothing ->
          -- REPLICATE CHANNEL TO POST, ALL TRANSACTIONS.
          -- TODO: repliceer alleen transacties die niet van mijzelf zijn.
          localReplication channelId post Nothing
        Just you -> do
          -- OTHER KNOWN
          -- yourIdentifier
          (RoleInstance userBehindYou) <- you ##>> bottom
          -- REPLICATE CHANNEL TO POST, JUST TRANSACTIONS AUTHORED BY OTHER.
          localReplication channelId post (Just userBehindYou)

          -- REPLICATING CHANNEL TO COPY OF OTHER:
          host <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$Host"
          hostValue <- you ##> host
          case hostValue of
            -- NO HOST
            Nothing -> do
              relayHost <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$RelayHost"
              relayHostValue <- you ##> relayHost
              case relayHostValue of
                Nothing -> pure unit
                -- RELAYHOST
                Just (Value h) -> do
                  -- if h equals the host of this PDR, just stop.
                  myHost <- getHost
                  if myHost == h
                    then pure unit
                    else do
                      relayPort <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$RelayPort"
                      portValue <- you ##> relayPort
                      case portValue of
                        Nothing -> pure unit
                        -- REPLICATE LOCAL CHANNEL TO RELAYHOST, JUST TRANSACTIONS AUTHORED BY ME.
                        -- Push local copy of channel to RelayHost. Author = me
                        Just (Value p) -> do
                          me <- getUserIdentifier
                          setPushAndPullReplication channelId h p me
            Just (Value yourHost) -> do
              -- HOST
              -- if h equals the host of this PDR, just stop.
              myHost <- getHost
              if myHost == yourHost
                then pure unit
                else do
                  port <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$Port"
                  portValue <- you ##> port
                  case portValue of
                    Nothing -> pure unit
                    -- REPLICATE LOCAL CHANNEL TO HOST, JUST TRANSACTIONS AUTHORED BY ME.
                    -- Push local copy of channel to PeerHost. Author = me
                    Just (Value yourPort) -> do
                      me <- getUserIdentifier
                      setPushReplication channelId yourHost yourPort me

postDbName :: MonadPerspectives String
postDbName = do
  systemIdentifier <- getSystemIdentifier
  pure $ systemIdentifier <> "_post/"

-- | Push local channel to remote.
setPushReplication :: String -> String -> String -> String -> MonadPerspectives Unit
setPushReplication channelDatabaseName host port author = do
  base <- getCouchdbBaseURL
  replicateContinuously
    channelDatabaseName
    (base <> channelDatabaseName)
    (host <> ":" <> port <> "/" <> channelDatabaseName)
    (Just $ selectOnFieldEqual "author" author)

-- | Push local channel to remote, pull remote channel on the RelayHost to local.
setPushAndPullReplication :: String -> String -> String -> String -> MonadPerspectives Unit
setPushAndPullReplication channelDatabaseName host port author = do
  base <- getCouchdbBaseURL
  -- Push my Transactions outwards;
  me <- getUserIdentifier
  replicateContinuously
    channelDatabaseName
    (base <> channelDatabaseName)
    (host <> ":" <> port <> "/" <> channelDatabaseName)
    (Just $ selectOnFieldEqual "author" me)
  -- Pull in all Transactions from the partner inwards.
  replicateContinuously
    channelDatabaseName
    (host <> ":" <> port <> "/" <> channelDatabaseName)
    (base <> channelDatabaseName)
    (Just $ selectOnFieldEqual "author" author)

-- | Replicate source to target. Both databases are supposed to be local, so push or pull is unimportant.
-- | If the author is not given, the criterium will be that the "author" field in the transaction must be
-- | *different* from the own user id.
localReplication :: String -> String -> Maybe String -> MonadPerspectives Unit
localReplication source target author = do
  base <- getCouchdbBaseURL
  me <- getUserIdentifier
  replicateContinuously
    (source <> "_" <> target)
    (base <> source)
    (base <> target)
    (maybe
      (Just $ selectOnFieldNotEqual "author" me)
      (\a -> Just $ selectOnFieldEqual "author" a)
      author)

-- | Stop replicating the channel to the post database.
-- | Stop replicating the channel database to the remote version.
endChannelReplication :: ContextInstance -> MonadPerspectives Unit
endChannelReplication channel = do
  getChannelId <- getPropertyFunction "model:System$Channel$External$ChannelDatabaseName"
  mchannel <- channel ##> externalRole >=> getChannelId
  post <- postDbName
  case mchannel of
    Nothing -> pure unit
    Just (Value channelId) -> do
      r <- try do
        void $ endReplication channelId post
        void $ deleteDocument_ "_replicator" channelId
      case r of
        Left e -> log $ show e
        Right _ -> pure unit

-- Not used.
getYourHostAndPort :: ContextInstance -> MonadPerspectives (Maybe (Tuple Host Port_))
getYourHostAndPort channel = do
  getYou <- getRoleFunction "sys:Channel$You"
  myou <- channel ##> getYou
  case myou of
    Nothing -> pure Nothing
    Just you -> do
      host <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$Host"
      hostValue <- you ##> host
      case hostValue of
        Nothing -> do
          relayHost <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$RelayHost"
          relayHostValue <- you ##> relayHost
          case relayHostValue of
            Nothing -> pure Nothing
            Just (Value h) -> do
              relayPort <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$RelayPort"
              portValue <- you ##> relayPort
              case portValue of
                Nothing -> pure Nothing
                Just (Value p) -> pure $ Just $ Tuple h p
        Just (Value yourHost) -> do
          port <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$Port"
          portValue <- you ##> port
          case portValue of
            Nothing -> pure Nothing
            Just (Value yourPort) -> pure $ Just $ Tuple yourHost yourPort
