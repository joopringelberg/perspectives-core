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
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, RoleType)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Show, show, (<>), (==), (&&))

-----------------------------------------------------------
-- GENERIC
-----------------------------------------------------------
type DeltaRecord f = {users :: Array RoleInstance, sequenceNumber :: Int, subject :: SubjectOfAction | f}

-----------------------------------------------------------
-- SUBJECTOFACTION
-----------------------------------------------------------
data SubjectOfAction = UserInstance RoleInstance | UserType RoleType

derive instance genericSubjectOfAction :: Generic SubjectOfAction _

instance showSubjectOfAction :: Show SubjectOfAction where
  show = genericShow

instance prettyPrintSubjectOfAction :: PrettyPrint SubjectOfAction where
  prettyPrint' t (UserInstance r) = prettyPrint' t r
  prettyPrint' t (UserType r) = prettyPrint' t r

instance encodeSubjectOfAction :: Encode SubjectOfAction where
  encode = genericEncode defaultOptions
instance decodeSubjectOfAction :: Decode SubjectOfAction where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- UNIVERSECONTEXTDELTA
-----------------------------------------------------------
newtype UniverseContextDelta = UniverseContextDelta (DeltaRecord
  ( id :: ContextInstance
  , contextType :: ContextType
  , deltaType :: UniverseContextDeltaType
  ))

derive instance genericUniverseContextDelta :: Generic UniverseContextDelta _
derive instance newtypeUniverseContextDelta :: Newtype UniverseContextDelta _

instance showUniverseContextDelta :: Show UniverseContextDelta where
  show = genericShow

instance eqUniverseContextDelta :: Eq UniverseContextDelta where
  eq (UniverseContextDelta {id: i1, contextType: c1, deltaType: d1}) (UniverseContextDelta {id: i2, contextType: c2, deltaType: d2}) = i1 == i2 && c1 == c2 && d1 == d2

instance encodeUniverseContextDelta :: Encode UniverseContextDelta where
  encode = genericEncode defaultOptions
instance decodeUniverseContextDelta :: Decode UniverseContextDelta where
  decode = genericDecode defaultOptions

-----------------------------------------------------------
-- UNIVERSECONTEXTDELTATYPE
-----------------------------------------------------------
data UniverseContextDeltaType = ConstructEmptyContext | RemoveContextInstance
derive instance genericUniverseContextDeltaType :: Generic UniverseContextDeltaType _
instance showUniverseContextDeltaType :: Show UniverseContextDeltaType where
  show = genericShow

instance eqUniverseContextDeltaType :: Eq UniverseContextDeltaType where
  eq = genericEq

instance encodeUniverseContextDeltaType :: Encode UniverseContextDeltaType where
  encode = genericEncode defaultOptions
instance decodeUniverseContextDeltaType :: Decode UniverseContextDeltaType where
  decode = genericDecode defaultOptions

instance prettyPrintUniverseContextDelta :: PrettyPrint UniverseContextDelta where
  prettyPrint' t (UniverseContextDelta r) = "UniverseContextDelta " <> prettyPrint' (t <> "  ") r

instance prettyPrintUniverseContextDeltaType :: PrettyPrint UniverseContextDeltaType where
  prettyPrint' t = show

-----------------------------------------------------------
-- UNIVERSEROLEDELTA
-----------------------------------------------------------
newtype UniverseRoleDelta = UniverseRoleDelta (DeltaRecord
  ( id :: ContextInstance
  , roleType :: EnumeratedRoleType
  , roleInstances :: SerializableNonEmptyArray RoleInstance
  , deltaType :: UniverseRoleDeltaType
  -- Add, Remove
  ))

derive instance genericUniverseRoleDelta :: Generic UniverseRoleDelta _

derive instance newTypeUniverseRoleDelta :: Newtype UniverseRoleDelta _

instance showUniverseRoleDelta :: Show UniverseRoleDelta where
  show = genericShow

instance eqUniverseRoleDelta :: Eq UniverseRoleDelta where
  eq (UniverseRoleDelta {id:i1, roleType:r1, roleInstances:ri1, deltaType:d1}) (UniverseRoleDelta {id:i2, roleType:r2, roleInstances:ri2, deltaType:d2}) = i1 == i2 && r1 == r2 && ri1 == ri2 && d1 == d2

instance encodeUniverseRoleDelta :: Encode UniverseRoleDelta where
  encode = genericEncode defaultOptions
instance decodeUniverseRoleDelta :: Decode UniverseRoleDelta where
  decode = genericDecode defaultOptions

instance prettyPrintUniverseRoleDelta :: PrettyPrint UniverseRoleDelta where
  prettyPrint' t (UniverseRoleDelta r) = "UniverseRoleDelta " <> prettyPrint' (t <> "  ") r

-----------------------------------------------------------
-- UNIVERSEROLEDELTATYPE
-----------------------------------------------------------
data UniverseRoleDeltaType = ConstructEmptyRole | ConstructExternalRole | RemoveRoleInstance
derive instance genericUniverseRoleDeltaType :: Generic UniverseRoleDeltaType _
instance showUniverseRoleDeltaType :: Show UniverseRoleDeltaType where
  show = genericShow

instance eqUniverseRoleDeltaType :: Eq UniverseRoleDeltaType where
  eq = genericEq

instance encodeUniverseRoleDeltaType :: Encode UniverseRoleDeltaType where
  encode = genericEncode defaultOptions
instance decodeUniverseRoleDeltaType :: Decode UniverseRoleDeltaType where
  decode = genericDecode defaultOptions
instance prettyPrintUniverseRoleDeltaType :: PrettyPrint UniverseRoleDeltaType where
  prettyPrint' t = show

-----------------------------------------------------------
-- CONTEXTDELTA
-----------------------------------------------------------
newtype ContextDelta = ContextDelta (DeltaRecord
  ( id :: ContextInstance
  , roleType :: EnumeratedRoleType
  , roleInstances :: SerializableNonEmptyArray RoleInstance
  , destinationContext :: Maybe ContextInstance
  , deltaType :: ContextDeltaType
  ))

derive instance genericContextDelta :: Generic ContextDelta _
derive instance newtypeContextDelta :: Newtype ContextDelta _

instance showContextDelta :: Show ContextDelta where
  show = genericShow

instance eqContextDelta :: Eq ContextDelta where
  eq (ContextDelta {id:i1, roleType:r1, roleInstances:ri1, destinationContext: dc1, deltaType:d1}) (ContextDelta {id:i2, roleType:r2, roleInstances:ri2, destinationContext: dc2, deltaType:d2}) = i1 == i2 && r1 == r2 && ri1 == ri2 && dc1 == dc2 && d1 == d2

instance encodeContextDelta :: Encode ContextDelta where
  encode = genericEncode defaultOptions
instance decodeContextDelta :: Decode ContextDelta where
  decode = genericDecode defaultOptions

instance prettyPrintContextDelta :: PrettyPrint ContextDelta where
  prettyPrint' t (ContextDelta r) = "ContextDelta " <> prettyPrint' (t <> "  ") r

-----------------------------------------------------------
-- CONTEXTDELTATYPE
-----------------------------------------------------------
data ContextDeltaType =
  AddRoleInstancesToContext |
  MoveRoleInstancesToAnotherContext |
  NoOp

derive instance genericContextDeltaType :: Generic ContextDeltaType _
instance showContextDeltaType :: Show ContextDeltaType where
  show = genericShow

instance eqContextDeltaType :: Eq ContextDeltaType where
  eq = genericEq

instance encodeContextDeltaType :: Encode ContextDeltaType where
  encode = genericEncode defaultOptions
instance decodeContextDeltaType :: Decode ContextDeltaType where
  decode = genericDecode defaultOptions
instance prettyPrintContextDeltaType :: PrettyPrint ContextDeltaType where
  prettyPrint' t = show

-----------------------------------------------------------
-- ROLEBINDINGDELTA
-----------------------------------------------------------
newtype RoleBindingDelta = RoleBindingDelta (DeltaRecord
  ( id :: RoleInstance
  , binding :: Maybe RoleInstance
  , oldBinding :: Maybe RoleInstance
  , deltaType :: RoleBindingDeltaType
  , roleWillBeRemoved :: Boolean
  -- Remove, Change
  ))

derive instance genericRoleDelta :: Generic RoleBindingDelta _
derive instance newtypeRoleBindingDelta :: Newtype RoleBindingDelta _

instance showRoleDelta :: Show RoleBindingDelta where
  show = genericShow

instance eqRoleDelta :: Eq RoleBindingDelta where
  eq (RoleBindingDelta {id:i1, binding:b1, oldBinding:ob1, deltaType:d1, roleWillBeRemoved:r1}) (RoleBindingDelta {id:i2, binding:b2, oldBinding:ob2, deltaType:d2, roleWillBeRemoved:r2}) = i1 == i2 && b1 == b2 && ob1 == ob2 && d1 == d2 && r1 == r2

instance encodeRoleDelta :: Encode RoleBindingDelta where
  encode = genericEncode defaultOptions
instance decodeRoleDelta :: Decode RoleBindingDelta where
  decode = genericDecode defaultOptions

instance prettyPrintRoleBindingDelta :: PrettyPrint RoleBindingDelta where
  prettyPrint' t (RoleBindingDelta r) = "RoleBindingDelta " <> prettyPrint' (t <> "  ") r

-----------------------------------------------------------
-- ROLEBINDINGDELTATYPE
-----------------------------------------------------------
data RoleBindingDeltaType = SetBinding | RemoveBinding

derive instance genericRoleBindingDeltaType :: Generic RoleBindingDeltaType _

instance showRoleBindingDeltaType :: Show RoleBindingDeltaType where
  show = genericShow

instance eqRoleBindingDeltaType :: Eq RoleBindingDeltaType where
  eq = genericEq

instance encodeRoleBindingDeltaType :: Encode RoleBindingDeltaType where
  encode = genericEncode defaultOptions
instance decodeRoleBindingDeltaType :: Decode RoleBindingDeltaType where
  decode = genericDecode defaultOptions
instance prettyPrintRoleBindingDeltaType :: PrettyPrint RoleBindingDeltaType where
  prettyPrint' t = show

-----------------------------------------------------------
-- ROLEPROPERTYDELTA
-----------------------------------------------------------
newtype RolePropertyDelta = RolePropertyDelta (DeltaRecord
  ( id :: RoleInstance
  , property :: EnumeratedPropertyType
  , values :: Array Value
  , deltaType :: RolePropertyDeltaType
  ))

derive instance genericPropertyDelta :: Generic RolePropertyDelta _
derive instance newtypeRolePropertyDelta :: Newtype RolePropertyDelta _

instance showPropertyDelta :: Show RolePropertyDelta where
  show = genericShow

instance eqPropertyDelta :: Eq RolePropertyDelta where
  eq (RolePropertyDelta {id:i1, property:p1, values:v1, deltaType:d1}) (RolePropertyDelta {id:i2, property:p2, values:v2, deltaType:d2}) = i1 == i2 && p1 == p2 && v1 == v2 && d1 == d2

instance encodePropertyDelta :: Encode RolePropertyDelta where
  encode = genericEncode defaultOptions
instance decodePropertyDelta :: Decode RolePropertyDelta where
  decode = genericDecode defaultOptions

instance prettyPrintRolePropertyDelta :: PrettyPrint RolePropertyDelta where
  prettyPrint' t (RolePropertyDelta r) = "RolePropertyDelta " <> prettyPrint' (t <> "  ") r

-----------------------------------------------------------
-- ROLEPROPERTYDELTATYPE
-----------------------------------------------------------
data RolePropertyDeltaType = AddProperty | RemoveProperty | DeleteProperty | SetProperty

derive instance genericRolePropertyDeltaType :: Generic RolePropertyDeltaType _
derive instance eqRolePropertyDeltaType :: Eq RolePropertyDeltaType

instance showRolePropertyDeltaType :: Show RolePropertyDeltaType where
  show = genericShow

instance encodeRolePropertyDeltaType :: Encode RolePropertyDeltaType where
  encode = genericEncode defaultOptions
instance decodeRolePropertyDeltaType :: Decode RolePropertyDeltaType where
  decode = genericDecode defaultOptions

instance prettyPrintRolePropertyDeltaType :: PrettyPrint RolePropertyDeltaType where
  prettyPrint' t = show
