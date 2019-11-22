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

-----------------------------------------------------------
-- DELTA
-----------------------------------------------------------
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Class (class GenericEncode)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (class Show, ($))

-----------------------------------------------------------
-- ROLEDELTA
-----------------------------------------------------------
newtype RoleDelta = RoleDelta
  { id :: ContextInstance
  , role :: EnumeratedRoleType
  , instance :: Maybe RoleInstance
  , deltaType :: DeltaType
  }

derive instance genericRoleDelta :: Generic RoleDelta _

instance showRoleDelta :: Show RoleDelta where
  show = genericShow

derive instance eqRoleDelta :: Eq RoleDelta

instance encodeRoleDelta :: Encode RoleDelta where
  encode = genericEncode defaultOptions
instance decodeRoleDelta :: Decode RoleDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- BINDINGDELTA
-----------------------------------------------------------
newtype BindingDelta = BindingDelta
  { id :: RoleInstance
  , binding :: Maybe RoleInstance
  , deltaType :: DeltaType
  }

derive instance genericBindingDelta :: Generic BindingDelta _

instance showBindingDelta :: Show BindingDelta where
  show = genericShow

derive instance eqBindingDelta :: Eq BindingDelta

instance encodeBindingDelta :: Encode BindingDelta where
  encode = genericEncode defaultOptions
instance decodeBindingDelta :: Decode BindingDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- PROPERTYDELTA
-----------------------------------------------------------
newtype PropertyDelta = PropertyDelta
  { id :: RoleInstance
  , property :: EnumeratedPropertyType
  , value :: Maybe Value
  , deltaType :: DeltaType
  }

derive instance genericPropertyDelta :: Generic PropertyDelta _

instance showPropertyDelta :: Show PropertyDelta where
  show = genericShow

derive instance eqPropertyDelta :: Eq PropertyDelta

instance encodePropertyDelta :: Encode PropertyDelta where
  encode = genericEncode defaultOptions
instance decodePropertyDelta :: Decode PropertyDelta where
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
