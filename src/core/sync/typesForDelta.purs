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

module Perspectives.TypesForDeltas where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (class Show)

-----------------------------------------------------------
-- UNIVERSECONTEXTDELTA
-----------------------------------------------------------
newtype UniverseContextDelta = UniverseContextDelta
  { id :: ContextInstance
  , contextType :: ContextType
  -- Add, Remove
  , deltaType :: DeltaType
  , users :: Array RoleInstance
  }

derive instance genericUniverseContextDelta :: Generic UniverseContextDelta _

instance showUniverseContextDelta :: Show UniverseContextDelta where
  show = genericShow

derive instance eqUniverseContextDelta :: Eq UniverseContextDelta

instance encodeUniverseContextDelta :: Encode UniverseContextDelta where
  encode = genericEncode defaultOptions
instance decodeUniverseContextDelta :: Decode UniverseContextDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- UNIVERSEROLEDELTA
-----------------------------------------------------------
newtype UniverseRoleDelta = UniverseRoleDelta
  { id :: RoleInstance
  , roleType :: EnumeratedRoleType
  -- Add, Remove
  , deltaType :: DeltaType
  , users :: Array RoleInstance
  }

derive instance genericUniverseRoleDelta :: Generic UniverseRoleDelta _

derive instance newTypeUniverseRoleDelta :: Newtype UniverseRoleDelta _

instance showUniverseRoleDelta :: Show UniverseRoleDelta where
  show = genericShow

derive instance eqUniverseRoleDelta :: Eq UniverseRoleDelta

instance encodeUniverseRoleDelta :: Encode UniverseRoleDelta where
  encode = genericEncode defaultOptions
instance decodeUniverseRoleDelta :: Decode UniverseRoleDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- CONTEXTDELTA
-----------------------------------------------------------
newtype ContextDelta = ContextDelta
  { id :: ContextInstance
  , roleType :: EnumeratedRoleType
  , roleInstance :: RoleInstance
  -- Add, Remove, Delete,
  , deltaType :: DeltaType
  , users :: Array RoleInstance
  }

derive instance genericContextDelta :: Generic ContextDelta _

instance showContextDelta :: Show ContextDelta where
  show = genericShow

derive instance eqContextDelta :: Eq ContextDelta

instance encodeContextDelta :: Encode ContextDelta where
  encode = genericEncode defaultOptions
instance decodeContextDelta :: Decode ContextDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- ROLEBINDINGDELTA
-----------------------------------------------------------
newtype RoleBindingDelta = RoleBindingDelta
  { id :: RoleInstance
  , binding :: Maybe RoleInstance
  , oldBinding :: Maybe RoleInstance
  -- Remove, Change
  , deltaType :: DeltaType
  , users :: Array RoleInstance
  }

derive instance genericRoleDelta :: Generic RoleBindingDelta _

instance showRoleDelta :: Show RoleBindingDelta where
  show = genericShow

derive instance eqRoleDelta :: Eq RoleBindingDelta

instance encodeRoleDelta :: Encode RoleBindingDelta where
  encode = genericEncode defaultOptions
instance decodeRoleDelta :: Decode RoleBindingDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- ROLEPROPERTYDELTA
-----------------------------------------------------------
newtype RolePropertyDelta = RolePropertyDelta
  { id :: RoleInstance
  , property :: EnumeratedPropertyType
  , value :: Maybe Value
  -- Add, Remove, Delete, Change
  , deltaType :: DeltaType
  , users :: Array RoleInstance
  }

derive instance genericPropertyDelta :: Generic RolePropertyDelta _

instance showPropertyDelta :: Show RolePropertyDelta where
  show = genericShow

derive instance eqPropertyDelta :: Eq RolePropertyDelta

instance encodePropertyDelta :: Encode RolePropertyDelta where
  encode = genericEncode defaultOptions
instance decodePropertyDelta :: Decode RolePropertyDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- DELTATYPE
-----------------------------------------------------------
data DeltaType = Add | Remove | Change | Delete

derive instance genericDeltaType :: Generic DeltaType _
derive instance eqDeltaType :: Eq DeltaType

instance showDeltaType :: Show DeltaType where
  show = genericShow

instance encodeDeltaType :: Encode DeltaType where
  encode = genericEncode defaultOptions
instance decodeDeltaType :: Decode DeltaType where
  decode = genericDecode defaultOptions
