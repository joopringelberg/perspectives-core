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
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (empty, fromFoldable)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=))
import Perspectives.Couchdb.Databases (createDatabase)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (isMe)
import Perspectives.Query.Compiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (<<<), (<>), (>=>), not)

-- | Create a new database for the communication between `me` and another user.
-- | Create an instance of sys:Channel. Bind `me` in the role ConnectedPartner. Set the value of ChannelDatabaseName to
-- | that of the new database.
-- | Bind the new Channel to usr:MijnSysteem in the role Channels.
createChannel :: MonadPerspectivesTransaction ContextInstance
createChannel = do
  channelName <- pure ("channel_" <> (show $ guid unit))
  lift2 $ createDatabase channelName
  eChannel <- constructContext $ ContextSerialization
    { id: "model:User$" <> channelName
    , prototype: Nothing
    , ctype: "sys:Channel"
    , rollen: fromFoldable [(Tuple "model:System$Channel$ConnectedPartner"
      [ RolSerialization
        { properties: PropertySerialization empty,
        binding: Just "usr:Me" }
       ])]
    , externeProperties: PropertySerialization $ fromFoldable [Tuple "model:System$Channel$External$ChannelDatabaseName" [channelName]]
    }
  -- TODO: dit is eigenlijk niet nodig.
  case eChannel of
    Left e -> throwError (error ("createChannel could not create channel: " <> show e))
    Right (channel :: ContextInstance) -> do
      void $ createAndAddRoleInstance (EnumeratedRoleType "model:System$PerspectivesSystem$Channels")
        "model:User$MijnSysteem"
        (RolSerialization
          { properties: PropertySerialization empty,
          binding: Just (buitenRol $ unwrap channel)})
      pure channel

addUserToChannel :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
addUserToChannel (RoleInstance usr) (ContextInstance channel) = void $ createAndAddRoleInstance (EnumeratedRoleType "model:System$Channel$ConnectedPartner")
  channel
  (RolSerialization
    { properties: PropertySerialization empty,
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
  you <- lift2 (channel ##= filter connectedPartner (lift <<< lift <<< (isMe >=> pure <<< not)))
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
