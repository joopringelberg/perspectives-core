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

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (empty, fromFoldable)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectives, (##=), (##>), (##>>))
import Perspectives.Couchdb (selectOnField)
import Perspectives.Couchdb.Databases (createDatabase, ensureAuthentication, replicateContinuously)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (bottom, externalRole, isMe)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Query.Compiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.SerializableNonEmptyArray (singleton) as SNA
import Perspectives.User (getCouchdbBaseURL, getHost, getPort, getSystemIdentifier)
import Prelude (Unit, bind, discard, not, pure, show, unit, void, ($), (<<<), (<>), (==), (>=>))

-- | Create a new database for the communication between `me` and another user.
-- | Create an instance of sys:Channel. Bind `me` in the role ConnectedPartner. Set the value of ChannelDatabaseName to
-- | that of the new database.
-- | Bind the new Channel to MySystem in the role Channels.
createChannel :: MonadPerspectivesTransaction ContextInstance
createChannel = do
  channelName <- pure ("channel_" <> (show $ guid unit))
  channel <- createChannelContext channelName
  lift2 $ ensureAuthentication (createDatabase channelName)
  pure channel

createChannelContext :: String -> MonadPerspectivesTransaction ContextInstance
createChannelContext channelName = do
  host <- lift2 getHost
  port <- lift2 getPort
  eChannel <- constructContext $ ContextSerialization
    { id: "model:User$" <> channelName
    , prototype: Nothing
    , ctype: "sys:Channel"
    , rollen: fromFoldable [(Tuple "model:System$Channel$ConnectedPartner" $
      SNA.singleton (RolSerialization
        { id: Nothing
        , properties: PropertySerialization $ fromFoldable
          [
            Tuple "model:System$Channel$ConnectedPartner$Host" [host]
          , Tuple "model:System$Channel$ConnectedPartner$Port" [(show port)]
          ]
        , binding: Just "usr:Me" })
       )]
    , externeProperties: PropertySerialization $ fromFoldable [Tuple "model:System$Channel$External$ChannelDatabaseName" [channelName]]
    }
  -- TODO: dit is eigenlijk niet nodig.
  case eChannel of
    Left e -> throwError (error ("createChannel could not create channel: " <> show e))
    Right (channel :: ContextInstance) -> do
      sysId <- lift2 getMySystem
      void $ createAndAddRoleInstance (EnumeratedRoleType "model:System$PerspectivesSystem$Channels")
        sysId
        (RolSerialization
          { id: Nothing
          , properties: PropertySerialization empty,
          binding: Just (buitenRol $ unwrap channel)})
      pure channel

addUserToChannel :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
addUserToChannel (RoleInstance usr) (ContextInstance channel) = void $ createAndAddRoleInstance (EnumeratedRoleType "model:System$Channel$ConnectedPartner")
  channel
  (RolSerialization
    { id: Nothing
    , properties: PropertySerialization empty,
    binding: Just usr})

type Host = String
type Port = Int

setMyAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setMyAddress host port channel = do
  connectedPartner <- lift2 (getRoleFunction "sys:Channel$ConnectedPartner")
  me <- lift2 (channel ##= filter connectedPartner (lift <<< lift <<< isMe))
  setProperty me (EnumeratedPropertyType "model:System$Channel$ConnectedPartner$Host") [Value host]
  setProperty me (EnumeratedPropertyType "model:System$Channel$ConnectedPartner$Port") [Value (show port)]

setYourAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setYourAddress host port channel =  do
  connectedPartner <- lift2 (getRoleFunction "sys:Channel$ConnectedPartner")
  you <- lift2 (channel ##= filter connectedPartner (lift <<< lift <<< isMe >=> pure <<< not))
  setProperty you (EnumeratedPropertyType "model:System$Channel$ConnectedPartner$Host") [Value host]
  setProperty you (EnumeratedPropertyType "model:System$Channel$ConnectedPartner$Port") [Value (show port)]

setMyRelayAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setMyRelayAddress host port channel = do
  connectedPartner <- lift2 (getRoleFunction "sys:Channel$ConnectedPartner")
  me <- lift2 (channel ##= filter connectedPartner (lift <<< lift <<< isMe))
  setProperty me (EnumeratedPropertyType "model:System$Channel$ConnectedPartner$RelayHost") [Value host]
  setProperty me (EnumeratedPropertyType "model:System$Channel$ConnectedPartner$RelayPort") [Value (show port)]

setYourRelayAddress :: Host -> Port -> ContextInstance -> MonadPerspectivesTransaction Unit
setYourRelayAddress host port channel = do
  connectedPartner <- lift2 (getRoleFunction "sys:Channel$ConnectedPartner")
  me <- lift2 (channel ##= filter connectedPartner (lift <<< lift <<< (isMe >=> pure <<< not)))
  setProperty me (EnumeratedPropertyType "model:System$Channel$ConnectedPartner$RelayHost") [Value host]
  setProperty me (EnumeratedPropertyType "model:System$Channel$ConnectedPartner$RelayPort") [Value (show port)]

-- | For a channel, set the replication of the local copy to the database found at either Host or RelayHost.
-- | If host and port are equal for both partners, do not set replication.
-- | Also set replication for the channel to the post database.
-- TODO. Zodra MonadPerspectives gestapeld is op ExceptT, gebruik dan throwError in plaats van pure unit.
setChannelReplication :: ContextInstance -> MonadPerspectives Unit
setChannelReplication channel = do
  getChannelId <- getPropertyFunction "model:System$Channel$External$ChannelDatabaseName"
  mchannel <- channel ##> externalRole >=> getChannelId
  case mchannel of
    Nothing -> pure unit
    Just (Value channelId) -> do
      connectedPartner <- getRoleFunction "sys:Channel$ConnectedPartner"
      myou <- channel ##> filter connectedPartner (lift <<< lift <<< (isMe >=> pure <<< not))
      case myou of
        Nothing -> pure unit
        Just you -> do
          -- yourIdentifier
          (RoleInstance userBehindYou) <- you ##>> bottom
          host <- getPropertyFunction "sys:Channel$ConnectedPartner$Host"
          hostValue <- you ##> host
          case hostValue of
            Nothing -> do
              relayHost <- getPropertyFunction "sys:Channel$ConnectedPartner$RelayHost"
              relayHostValue <- you ##> relayHost
              case relayHostValue of
                Nothing -> pure unit
                Just (Value h) -> do
                  -- if h equals the host of this PDR, just stop.
                  myHost <- getHost
                  if myHost == h
                    then pure unit
                    else do
                      relayPort <- getPropertyFunction "sys:Channel$ConnectedPartner$RelayPort"
                      portValue <- you ##> relayPort
                      case portValue of
                        Nothing -> pure unit
                        -- Push local copy of channel to RelayHost. Author = you
                        Just (Value p) -> setPushAndPullReplication channelId h p userBehindYou
            Just (Value h) -> do
              -- if h equals the host of this PDR, just stop.
              myHost <- getHost
              if myHost == h
                then pure unit
                else do
                  port <- getPropertyFunction "sys:Channel$ConnectedPartner$Port"
                  portValue <- you ##> port
                  case portValue of
                    Nothing -> pure unit
                    -- Push local copy of channel to PeerHost. Author = me
                    Just (Value p) -> do
                      me <- getUserIdentifier
                      setPushReplication channelId h p me
          post <- postDbName
          localReplication channelId post (Just userBehindYou)

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
    (Just $ selectOnField "author" author)

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
    (Just $ selectOnField "author" me)
  -- Pull in all Transactions from the partner inwards.
  replicateContinuously
    channelDatabaseName
    (host <> ":" <> port <> "/" <> channelDatabaseName)
    (base <> channelDatabaseName)
    (Just $ selectOnField "author" author)

-- | Replicate source to target. Both databases are supposed to be local, so push or pull is unimportant.
localReplication :: String -> String -> Maybe String -> MonadPerspectives Unit
localReplication source target author = do
  base <- getCouchdbBaseURL
  replicateContinuously
    (source <> "_" <> target)
    (base <> source)
    (base <> target)
    (maybe Nothing (\a -> Just $ selectOnField "author" a) author)