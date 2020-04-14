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

module Perspectives.DomeinFile where

import Control.Monad.State (State, execState, modify)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object, empty, insert)
import Perspectives.Couchdb.Revision (class Revision, Revision_, changeRevision, getRev)
import Perspectives.InstanceRepresentation (PerspectRol)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType)
import Perspectives.Representation.View (View)
import Prelude (class Eq, class Ord, class Show, Unit, bind, compare, pure, unit, void, ($), (<<<), (==))

newtype DomeinFile = DomeinFile DomeinFileRecord

type DomeinFileRecord =
  { _rev :: Revision_
  , _id :: String
  , contexts :: Object Context
  , enumeratedRoles :: Object EnumeratedRole
  , calculatedRoles :: Object CalculatedRole
  , enumeratedProperties :: Object EnumeratedProperty
  , calculatedProperties :: Object CalculatedProperty
  , views :: Object View
  , actions :: Object Action
  , crl :: String
  -- These are instances of types in this model that have been declared 'indexed'.
  , indexedRoles :: Array RoleInstance
  , indexedContexts :: Array ContextInstance
  , modelDescription :: Maybe PerspectRol
  , referredModels :: Array DomeinFileId
  }

derive instance genericDomeinFile :: Generic DomeinFile _

derive instance newtypeDomeinFile :: Newtype DomeinFile _

instance encodeDomeinFile :: Encode DomeinFile where
  encode = genericEncode $ defaultOptions --{unwrapSingleConstructors = true}

instance decodeDomeinFile :: Decode DomeinFile where
  decode (json :: Foreign) = do
    rev <- getRev json
    a <- genericDecode defaultOptions json
    pure (changeRevision rev a)

instance showDomeinFile :: Show DomeinFile where
  show = genericShow

instance eqDomeinFile :: Eq DomeinFile where
  eq = genericEq

instance identifiableDomeinFile :: Identifiable DomeinFile DomeinFileId where
  identifier (DomeinFile{_id}) = DomeinFileId _id

instance revisionDomeinFile :: Revision DomeinFile where
  rev = _._rev <<< unwrap
  changeRevision s = over DomeinFile (\vr -> vr {_rev = s})

newtype DomeinFileId = DomeinFileId String
derive instance newtypeDomeinFileId :: Newtype DomeinFileId _
derive instance genericRepDomeinFileId :: Generic DomeinFileId _
derive newtype instance encodeDomeinFileId :: Encode DomeinFileId
derive newtype instance decodeDomeinFileId :: Decode DomeinFileId
instance showDomeinFileId :: Show DomeinFileId where
  show = unwrap
instance eqDomeinFileId :: Eq DomeinFileId where
  eq (DomeinFileId id1) (DomeinFileId id2) = id1 == id2
instance ordDomeinFileId :: Ord DomeinFileId where
  compare (DomeinFileId a) (DomeinFileId b) = compare a b

defaultDomeinFileRecord :: DomeinFileRecord
defaultDomeinFileRecord =
  { _rev: Nothing
  , _id: ""
  , contexts: empty
  , enumeratedRoles: empty
  , calculatedRoles: empty
  , enumeratedProperties: empty
  , calculatedProperties: empty
  , views: empty
  , actions: empty
  , crl: ""
  , indexedRoles: []
  , indexedContexts: []
  , modelDescription: Nothing
  , referredModels: []}

defaultDomeinFile :: DomeinFile
defaultDomeinFile = DomeinFile defaultDomeinFileRecord

type DomeinFileEnumeratedRoles = Object EnumeratedRole

setRevision :: String -> DomeinFile -> DomeinFile
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}

-- | Returns a table with indexed names as key and ContextType as value.
indexedContexts :: DomeinFile -> Object ContextType
indexedContexts (DomeinFile{contexts}) = execState indexedContexts_ empty
  where
    indexedContexts_ :: State (Object ContextType) Unit
    indexedContexts_ = for_ contexts \(Context{_id, indexedContext}) -> case indexedContext of
      Nothing -> pure unit
      Just i -> void $ modify \table -> insert (unwrap i) _id table

-- | Returns a table with indexed names as key and ContextType as value.
indexedRoles :: DomeinFile -> Object EnumeratedRoleType
indexedRoles (DomeinFile{enumeratedRoles}) = execState indexedRoles_ empty
  where
    indexedRoles_ :: State (Object EnumeratedRoleType) Unit
    indexedRoles_ = for_ enumeratedRoles \(EnumeratedRole{_id, indexedRole}) -> case indexedRole of
      Nothing -> pure unit
      Just i -> void $ modify \table -> insert (unwrap i) _id table
