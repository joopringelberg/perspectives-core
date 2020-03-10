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
import Prelude (class Show, eq)

-----------------------------------------------------------
-- GENERIC
-----------------------------------------------------------
type DeltaRecord f = {users :: Array RoleInstance, deltaType :: DeltaType, sequenceNumber :: Int | f}

-----------------------------------------------------------
-- UNIVERSECONTEXTDELTA
-----------------------------------------------------------
newtype UniverseContextDelta = UniverseContextDelta (DeltaRecord
  ( id :: ContextInstance
  , contextType :: ContextType
  -- Add, Remove
  ))

derive instance genericUniverseContextDelta :: Generic UniverseContextDelta _

instance showUniverseContextDelta :: Show UniverseContextDelta where
  show = genericShow

instance eqUniverseContextDelta :: Eq UniverseContextDelta where
  eq (UniverseContextDelta r1@{}) (UniverseContextDelta r2@{}) = eq r1{sequenceNumber=0} r2{sequenceNumber=0}

instance encodeUniverseContextDelta :: Encode UniverseContextDelta where
  encode = genericEncode defaultOptions
instance decodeUniverseContextDelta :: Decode UniverseContextDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- UNIVERSEROLEDELTA
-----------------------------------------------------------
newtype UniverseRoleDelta = UniverseRoleDelta (DeltaRecord
  ( id :: RoleInstance
  , roleType :: EnumeratedRoleType
  -- Add, Remove
  ))

derive instance genericUniverseRoleDelta :: Generic UniverseRoleDelta _

derive instance newTypeUniverseRoleDelta :: Newtype UniverseRoleDelta _

instance showUniverseRoleDelta :: Show UniverseRoleDelta where
  show = genericShow

instance eqUniverseRoleDelta :: Eq UniverseRoleDelta where
  eq (UniverseRoleDelta r1@{}) (UniverseRoleDelta r2@{}) = eq r1{sequenceNumber=0} r2{sequenceNumber=0}

instance encodeUniverseRoleDelta :: Encode UniverseRoleDelta where
  encode = genericEncode defaultOptions
instance decodeUniverseRoleDelta :: Decode UniverseRoleDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- CONTEXTDELTA
-----------------------------------------------------------
newtype ContextDelta = ContextDelta (DeltaRecord
  ( id :: ContextInstance
  , roleType :: EnumeratedRoleType
  , roleInstance :: RoleInstance
  -- Add, Remove, Delete,
  ))

derive instance genericContextDelta :: Generic ContextDelta _

instance showContextDelta :: Show ContextDelta where
  show = genericShow

instance eqContextDelta :: Eq ContextDelta where
  eq (ContextDelta r1@{}) (ContextDelta r2@{}) = eq r1{sequenceNumber=0} r2{sequenceNumber=0}

instance encodeContextDelta :: Encode ContextDelta where
  encode = genericEncode defaultOptions
instance decodeContextDelta :: Decode ContextDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- ROLEBINDINGDELTA
-----------------------------------------------------------
newtype RoleBindingDelta = RoleBindingDelta (DeltaRecord
  ( id :: RoleInstance
  , binding :: Maybe RoleInstance
  , oldBinding :: Maybe RoleInstance
  -- Remove, Change
  ))

derive instance genericRoleDelta :: Generic RoleBindingDelta _

instance showRoleDelta :: Show RoleBindingDelta where
  show = genericShow

instance eqRoleDelta :: Eq RoleBindingDelta where
  eq (RoleBindingDelta r1@{}) (RoleBindingDelta r2@{}) = eq r1{sequenceNumber=0} r2{sequenceNumber=0}


instance encodeRoleDelta :: Encode RoleBindingDelta where
  encode = genericEncode defaultOptions
instance decodeRoleDelta :: Decode RoleBindingDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- ROLEPROPERTYDELTA
-----------------------------------------------------------
newtype RolePropertyDelta = RolePropertyDelta (DeltaRecord
  ( id :: RoleInstance
  , property :: EnumeratedPropertyType
  , value :: Maybe Value
  -- Add, Remove, Delete, Change
  ))

derive instance genericPropertyDelta :: Generic RolePropertyDelta _

instance showPropertyDelta :: Show RolePropertyDelta where
  show = genericShow

instance eqPropertyDelta :: Eq RolePropertyDelta where
  eq (RolePropertyDelta r1@{}) (RolePropertyDelta r2@{}) = eq r1{sequenceNumber=0} r2{sequenceNumber=0}

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
