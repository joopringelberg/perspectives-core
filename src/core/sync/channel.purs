-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Sync.Channel where

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Object (fromFoldable)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, MPT, (##=), (##>), (##>>), type (~~>))
import Perspectives.Couchdb (selectOnFieldEqual, selectOnFieldNotEqual)
import Perspectives.Identifiers (getFirstMatch, getSecondMatch)
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter, orElse)
import Perspectives.Instances.Me (isMe)
import Perspectives.Instances.ObjectGetters (bottom, externalRole)
import Perspectives.ModelDependencies (addressHost, addressPort, addressRelayHost, addressRelayPort, channel, channelDatabase, channelInitiator, channelPartner, privateChannel)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Persistence.API (Url, createDatabase)
import Perspectives.Persistence.CouchdbFunctions (endReplication, replicateContinuously)
import Perspectives.Persistence.State (getCouchdbBaseURL, getSystemIdentifier)
import Perspectives.PerspectivesState (getPerspectivesUser)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), ResourceType(..), RoleType(..))
import Perspectives.ResourceIdentifiers (createResourceIdentifier, guid)
import Perspectives.SerializableNonEmptyArray (singleton) as SNA
import Prelude (Unit, bind, discard, not, pure, show, unit, void, ($), (<<<), (<>), (==), (>=>), (>>=), (<$>))

-- | Create a new database for the communication between `me` and another user.
-- | Create an instance of sys:Channel. Bind `me` in the role Initiator. Set the value of ChannelDatabaseName to
-- | that of the new database.
createChannel :: Url -> MonadPerspectivesTransaction ContextInstance
createChannel couchdbUrl = do
  channelID <- createResourceIdentifier (CType $ ContextType channel)
  channel <- createChannelContext couchdbUrl channelID
  lift (guid channelID >>= createDatabase)
  pure channel

createChannelContext :: Url -> String -> MonadPerspectivesTransaction ContextInstance
createChannelContext couchdbUrl channelName = case splitCouchdbUrl couchdbUrl of
  Nothing -> throwError $ error ("createChannelContext received couchdbUrl that is not well-formed: " <> couchdbUrl)
  Just (Tuple host port) -> do
    eChannel <- runExceptT $ constructContext (Just $ ENR $ EnumeratedRoleType privateChannel) $ ContextSerialization
      { id: Just channelName
      , prototype: Nothing
      , ctype: channel
      , rollen: fromFoldable [(Tuple channelInitiator $
        SNA.singleton (RolSerialization
          { id: Nothing
          , properties: PropertySerialization $ fromFoldable
            [
              Tuple addressHost [host]
            , Tuple addressPort [(show port)]
            ]
          , binding: Just "sys:Me" })
         )]
      , externeProperties: PropertySerialization $ fromFoldable [Tuple channelDatabase [channelName]]
      }
    case eChannel of
      Left e -> throwError (error ("createChannel could not create channel: " <> show e))
      Right (channel :: ContextInstance) -> pure channel

-- | Add the second user to the channel: not the Initiator, but the ConnectedPartner.
addPartnerToChannel :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
addPartnerToChannel (RoleInstance usr) (ContextInstance channel) = do
  couchdbUrl <- lift $ getCouchdbBaseURL
  case splitCouchdbUrl <$> couchdbUrl of
    Just (Just (Tuple host port)) -> do
       void $ createAndAddRoleInstance (EnumeratedRoleType channelPartner)
        channel
        (RolSerialization
          { id: Nothing
          , properties: PropertySerialization $ fromFoldable
            [ Tuple addressHost [host]
            , Tuple addressPort [show port]
            ],
          binding: Just usr})
    _ -> throwError $ error ("addPartnerToChannel received not a well-formed couchdbUrl: " <> (show couchdbUrl))

type Host = String
type Port = Int
type Port_ = String

-- | Get either the Initiator or the ConnectedPartner - whomever is filled by me.
getMeFromChannel :: MonadPerspectives (ContextInstance ~~> RoleInstance)
getMeFromChannel = do
  getConnectedPartner <- (getRoleFunction "sys:Channel$ConnectedPartner")
  getInitiator <- (getRoleFunction "sys:Channel$Initiator")
  pure $ orElse
    (filter getConnectedPartner (lift <<< lift <<< isMe))
    (filter getInitiator (lift <<< lift <<< isMe))

-- | Get either the Initiator or the ConnectedPartner - whomever is NOT filled by me.
getYouFromChannel :: MonadPerspectives (ContextInstance ~~> RoleInstance)
getYouFromChannel = do
  getConnectedPartner <- (getRoleFunction "sys:Channel$ConnectedPartner")
  getInitiator <- (getRoleFunction "sys:Channel$Initiator")
  pure $ orElse
    (filter getConnectedPartner (lift <<< lift <<< isMe >=> pure <<< not))
    (filter getInitiator (lift <<< lift <<< isMe >=> pure <<< not))

setAddress :: Host -> Port -> Array RoleInstance -> MPT Unit
setAddress host port rl = do
  setProperty rl (EnumeratedPropertyType addressHost) Nothing [Value host]
  setProperty rl (EnumeratedPropertyType addressPort) Nothing [Value (show port)]

setRelayAddress :: Host -> Port -> Array RoleInstance -> MPT Unit
setRelayAddress host port rl = do
  setProperty rl (EnumeratedPropertyType addressRelayHost) Nothing [Value host]
  setProperty rl (EnumeratedPropertyType addressRelayPort) Nothing [Value (show port)]

-- | Regardless whether I am the Initiator or the ConnectedPartner, set my host and port value.
setMyAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setMyAddress host port channel = do
  g <- lift getMeFromChannel
  (lift (channel ##= g)) >>= setAddress host port

-- | Regardless whether you are the Initiator or the ConnectedPartner, set your host and port value.
setYourAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setYourAddress host port channel = do
  g <- lift getYouFromChannel
  (lift (channel ##= g)) >>= setAddress host port

setMyRelayAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setMyRelayAddress host port channel = do
  g <- lift getMeFromChannel
  (lift (channel ##= g)) >>= setRelayAddress host port

setYourRelayAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setYourRelayAddress host port channel = do
  g <- lift getYouFromChannel
  (lift (channel ##= g)) >>= setRelayAddress host port

-- | For a channel, set the replication of the local copy to the database found at either Host or RelayHost.
-- | If host and port are equal for both partners, do not set replication.
-- | Also set replication for the channel to the post database. This replication filters out just transactions
-- | whose author is the ConnectedPartner (so I do not have to deal with transactions I've created myself).
-- TODO. Zodra MonadPerspectives gestapeld is op ExceptT, gebruik dan throwError in plaats van pure unit.
setChannelReplication :: Url -> ContextInstance -> MonadPerspectives Unit
setChannelReplication couchdbUrl channel = do
  case splitCouchdbUrl couchdbUrl of
    -- Fails silently
    Nothing -> pure unit
    Just (Tuple myHost port) -> do
      getChannelId <- getPropertyFunction channelDatabase
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
              localReplication couchdbUrl channelId post Nothing
            Just you -> do
              -- OTHER KNOWN
              -- yourIdentifier
              (RoleInstance userBehindYou) <- you ##>> bottom
              -- REPLICATE CHANNEL TO POST, JUST TRANSACTIONS AUTHORED BY OTHER.
              localReplication couchdbUrl channelId post (Just userBehindYou)

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
                              setPushAndPullReplication couchdbUrl channelId h p me
                Just (Value yourHost) -> do
                  -- HOST
                  -- if h equals the host of this PDR, just stop.
                  if myHost == yourHost
                    then pure unit
                    else do
                      getPort <- getPropertyFunction "sys:PhysicalContext$UserWithAddress$Port"
                      portValue <- you ##> getPort
                      case portValue of
                        Nothing -> pure unit
                        -- REPLICATE LOCAL CHANNEL TO HOST, JUST TRANSACTIONS AUTHORED BY ME.
                        -- Push local copy of channel to PeerHost. Author = me
                        Just (Value yourPort) -> do
                          me <- getUserIdentifier
                          setPushReplication couchdbUrl channelId yourHost yourPort me

-- | Split an url of the form host:port into its two constituents.
splitCouchdbUrl :: String -> Maybe (Tuple String String)
splitCouchdbUrl s = case (getFirstMatch couchdbUrlRegex s), (getSecondMatch couchdbUrlRegex s) of
  Just source, Just target -> Just $ Tuple source target
  _, _ -> Nothing
  where
    couchdbUrlRegex :: Regex
    couchdbUrlRegex = unsafeRegex "^(.*):(.*)$" noFlags

postDbName :: MonadPerspectives String
postDbName = do
  systemIdentifier <- getSystemIdentifier
  pure $ systemIdentifier <> "_post/"

-- | Push local channel to remote.
setPushReplication :: Url -> String -> String -> String -> String -> MonadPerspectives Unit
setPushReplication base channelDatabaseName host port author = do
  usr <- getPerspectivesUser
  replicateContinuously
    usr
    base
    channelDatabaseName
    (base <> channelDatabaseName)
    (host <> ":" <> port <> "/" <> channelDatabaseName)
    (Just $ selectOnFieldEqual "author" author)

-- | Push local channel to remote, pull remote channel on the RelayHost to local.
setPushAndPullReplication :: Url -> String -> String -> String -> String -> MonadPerspectives Unit
setPushAndPullReplication base channelDatabaseName host port author = do
  -- Push my Transactions outwards;
  me <- getUserIdentifier
  usr <- getPerspectivesUser
  replicateContinuously
    usr
    base
    channelDatabaseName
    (base <> channelDatabaseName)
    (host <> ":" <> port <> "/" <> channelDatabaseName)
    (Just $ selectOnFieldEqual "author" me)
  -- Pull in all Transactions from the partner inwards.
  replicateContinuously
    usr
    base
    channelDatabaseName
    (host <> ":" <> port <> "/" <> channelDatabaseName)
    (base <> channelDatabaseName)
    (Just $ selectOnFieldEqual "author" author)

-- | Replicate source to target. Both databases are supposed to be local, so push or pull is unimportant.
-- | If the author is not given, the criterium will be that the "author" field in the transaction must be
-- | *different* from the own user id.
localReplication :: Url -> String -> String -> Maybe String -> MonadPerspectives Unit
localReplication base source target author = do
  me <- getUserIdentifier
  usr <- getPerspectivesUser
  replicateContinuously
    usr
    base
    (source <> "_" <> target)
    (base <> source)
    (base <> target)
    (maybe
      (Just $ selectOnFieldNotEqual "author" me)
      (\a -> Just $ selectOnFieldEqual "author" a)
      author)

-- | Stop replicating the channel to the post database.
-- | Stop replicating the channel database to the remote version.
endChannelReplication :: Url -> ContextInstance -> MonadPerspectives Unit
endChannelReplication couchdbUrl channel = do
  getChannelId <- getPropertyFunction channelDatabase
  mchannel <- channel ##> externalRole >=> getChannelId
  post <- postDbName
  case mchannel of
    Nothing -> pure unit
    Just (Value channelId) -> do
      r <- try do
        case splitChannelId channelId of
          Just (Tuple source target) -> do
            void $ endReplication couchdbUrl channelId post
            void $ endReplication couchdbUrl source target
          Nothing -> pure unit
      case r of
        Left e -> log $ show e
        Right _ -> pure unit
  where
    splitChannelId :: String -> Maybe (Tuple String String)
    splitChannelId s = case (getFirstMatch channelRegEx s), (getSecondMatch channelRegEx s) of
      Just source, Just target -> Just $ Tuple source target
      _, _ -> Nothing

    channelRegEx :: Regex
    channelRegEx = unsafeRegex "^(.*)_(.*)$" noFlags


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
