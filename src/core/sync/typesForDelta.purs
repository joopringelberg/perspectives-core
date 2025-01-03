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

module Perspectives.TypesForDeltas where

import Data.Eq (class Eq)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Foreign (unsafeToForeign)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, RoleType)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Ord, class Show, show, (&&), (<<<), (<>), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- GENERIC
-----------------------------------------------------------
-- | The subject is the user role with the perspective that should include the right verbs to allow the delta
-- | and with an object that corresponds to the resource being modified by the delta.
-- | This is often taken from the transacton in which modifications are made. It is the member 'authoringRole' of Transaction, provided when executing a transaction with runMonadPerspectivesTransaction
-- | (not to be confused with member 'author', who must be an instance of sys:PerspectivesSystem$User). THIS REMARK MAY BE OBSOLETE!
type DeltaRecord f = {subject :: RoleType | f}

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

derive newtype instance WriteForeign UniverseContextDelta
derive newtype instance ReadForeign UniverseContextDelta

derive instance Ord UniverseContextDelta

-----------------------------------------------------------
-- UNIVERSECONTEXTDELTATYPE
-----------------------------------------------------------
data UniverseContextDeltaType = ConstructEmptyContext
derive instance genericUniverseContextDeltaType :: Generic UniverseContextDeltaType _
instance showUniverseContextDeltaType :: Show UniverseContextDeltaType where
  show = genericShow

instance eqUniverseContextDeltaType :: Eq UniverseContextDeltaType where
  eq = genericEq

instance WriteForeign UniverseContextDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign UniverseContextDeltaType where readImpl = enumReadForeign

instance prettyPrintUniverseContextDelta :: PrettyPrint UniverseContextDelta where
  prettyPrint' t (UniverseContextDelta r) = "UniverseContextDelta " <> prettyPrint' (t <> "  ") r

instance prettyPrintUniverseContextDeltaType :: PrettyPrint UniverseContextDeltaType where
  prettyPrint' t = show

derive instance Ord UniverseContextDeltaType

-----------------------------------------------------------
-- UNIVERSEROLEDELTA
-----------------------------------------------------------
newtype UniverseRoleDelta = UniverseRoleDelta (DeltaRecord
  ( id :: ContextInstance
  , contextType :: ContextType
  , roleType :: EnumeratedRoleType
  -- To be provided when deltaType is ConstructExternalRole or RemoveUnboundExternalRoleInstance or RemoveExternalRoleInstance.
  -- It is the context role type that binds the external role; this is the role type that the user is authorized to construct and fill.
  -- It can also be a calculated rol that results in external roles (usually taken from the database).
  , authorizedRole :: Maybe RoleType
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

derive newtype instance WriteForeign UniverseRoleDelta
derive newtype instance ReadForeign UniverseRoleDelta

instance prettyPrintUniverseRoleDelta :: PrettyPrint UniverseRoleDelta where
  prettyPrint' t (UniverseRoleDelta r) = "UniverseRoleDelta " <> prettyPrint' (t <> "  ") r

derive instance Ord UniverseRoleDelta

-----------------------------------------------------------
-- UNIVERSEROLEDELTATYPE
-----------------------------------------------------------
data UniverseRoleDeltaType = ConstructEmptyRole | ConstructExternalRole | RemoveRoleInstance | RemoveUnboundExternalRoleInstance | RemoveExternalRoleInstance
derive instance genericUniverseRoleDeltaType :: Generic UniverseRoleDeltaType _
instance showUniverseRoleDeltaType :: Show UniverseRoleDeltaType where
  show = genericShow

instance eqUniverseRoleDeltaType :: Eq UniverseRoleDeltaType where
  eq = genericEq

instance WriteForeign UniverseRoleDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign UniverseRoleDeltaType where readImpl = enumReadForeign

instance prettyPrintUniverseRoleDeltaType :: PrettyPrint UniverseRoleDeltaType where
  prettyPrint' t = show

derive instance Ord UniverseRoleDeltaType
-----------------------------------------------------------
-- CONTEXTDELTA
-----------------------------------------------------------
newtype ContextDelta = ContextDelta (DeltaRecord
  ( contextInstance :: ContextInstance
  , contextType :: ContextType
  , roleType :: EnumeratedRoleType
  , roleInstance :: RoleInstance
  , destinationContext :: Maybe ContextInstance
  , destinationContextType :: Maybe ContextType
  , deltaType :: ContextDeltaType
  ))

derive instance genericContextDelta :: Generic ContextDelta _
derive instance newtypeContextDelta :: Newtype ContextDelta _

instance showContextDelta :: Show ContextDelta where
  show = genericShow

instance eqContextDelta :: Eq ContextDelta where
  eq (ContextDelta {contextInstance:i1, roleType:r1, roleInstance:ri1, {-destinationContext: dc1,-} deltaType:d1}) (ContextDelta {contextInstance:i2, roleType:r2, roleInstance:ri2, {-destinationContext: dc2,-} deltaType:d2}) = i1 == i2 && r1 == r2 && ri1 == ri2 && {-dc1 == dc2 &&-} d1 == d2

derive newtype instance WriteForeign ContextDelta
derive newtype instance ReadForeign ContextDelta

instance prettyPrintContextDelta :: PrettyPrint ContextDelta where
  prettyPrint' t (ContextDelta r) = "ContextDelta " <> prettyPrint' (t <> "  ") r

derive instance Ord ContextDelta

-----------------------------------------------------------
-- CONTEXTDELTATYPE
-----------------------------------------------------------
data ContextDeltaType =
  AddRoleInstancesToContext |
  AddExternalRole |
  MoveRoleInstancesToAnotherContext

derive instance genericContextDeltaType :: Generic ContextDeltaType _
instance showContextDeltaType :: Show ContextDeltaType where
  show = genericShow

instance eqContextDeltaType :: Eq ContextDeltaType where
  eq = genericEq

instance WriteForeign ContextDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign ContextDeltaType where readImpl = enumReadForeign

instance prettyPrintContextDeltaType :: PrettyPrint ContextDeltaType where
  prettyPrint' t = show

derive instance Ord ContextDeltaType

-----------------------------------------------------------
-- ROLEBINDINGDELTA
-----------------------------------------------------------
newtype RoleBindingDelta = RoleBindingDelta (DeltaRecord
  ( filled :: RoleInstance
  , filledType :: EnumeratedRoleType
  , filler :: Maybe RoleInstance
  , fillerType :: Maybe EnumeratedRoleType
  , oldFiller :: Maybe RoleInstance
  , oldFillerType :: Maybe EnumeratedRoleType
  , deltaType :: RoleBindingDeltaType
  -- Remove, Change
  ))

derive instance genericRoleDelta :: Generic RoleBindingDelta _
derive instance newtypeRoleBindingDelta :: Newtype RoleBindingDelta _

instance showRoleDelta :: Show RoleBindingDelta where
  show = genericShow

instance eqRoleDelta :: Eq RoleBindingDelta where
  eq (RoleBindingDelta {filled:i1, filler:b1, oldFiller:ob1, deltaType:d1}) (RoleBindingDelta {filled:i2, filler:b2, oldFiller:ob2, deltaType:d2}) = i1 == i2 && b1 == b2 && ob1 == ob2 && d1 == d2

derive newtype instance WriteForeign RoleBindingDelta
derive newtype instance ReadForeign RoleBindingDelta

instance prettyPrintRoleBindingDelta :: PrettyPrint RoleBindingDelta where
  prettyPrint' t (RoleBindingDelta r) = "RoleBindingDelta " <> prettyPrint' (t <> "  ") r

derive instance Ord RoleBindingDelta

  -----------------------------------------------------------
-- ROLEBINDINGDELTATYPE
-----------------------------------------------------------
data RoleBindingDeltaType = SetFirstBinding | RemoveBinding | ReplaceBinding

derive instance genericRoleBindingDeltaType :: Generic RoleBindingDeltaType _

instance showRoleBindingDeltaType :: Show RoleBindingDeltaType where
  show = genericShow

instance eqRoleBindingDeltaType :: Eq RoleBindingDeltaType where
  eq = genericEq

instance prettyPrintRoleBindingDeltaType :: PrettyPrint RoleBindingDeltaType where
  prettyPrint' t = show

instance WriteForeign RoleBindingDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign RoleBindingDeltaType where readImpl = enumReadForeign

derive instance Ord RoleBindingDeltaType

-----------------------------------------------------------
-- ROLEPROPERTYDELTA
-----------------------------------------------------------
newtype RolePropertyDelta = RolePropertyDelta (DeltaRecord
  ( id :: RoleInstance
  , roleType :: EnumeratedRoleType
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

derive newtype instance WriteForeign RolePropertyDelta
derive newtype instance ReadForeign RolePropertyDelta

instance prettyPrintRolePropertyDelta :: PrettyPrint RolePropertyDelta where
  prettyPrint' t (RolePropertyDelta r) = "RolePropertyDelta " <> prettyPrint' (t <> "  ") r

derive instance Ord RolePropertyDelta

-----------------------------------------------------------
-- ROLEPROPERTYDELTATYPE
-----------------------------------------------------------
data RolePropertyDeltaType = AddProperty | RemoveProperty | DeleteProperty | SetProperty | UploadFile

derive instance genericRolePropertyDeltaType :: Generic RolePropertyDeltaType _
derive instance eqRolePropertyDeltaType :: Eq RolePropertyDeltaType

instance showRolePropertyDeltaType :: Show RolePropertyDeltaType where
  show = genericShow

instance WriteForeign RolePropertyDeltaType where writeImpl = unsafeToForeign <<< show
instance ReadForeign RolePropertyDeltaType where readImpl = enumReadForeign

instance prettyPrintRolePropertyDeltaType :: PrettyPrint RolePropertyDeltaType where
  prettyPrint' t = show

derive instance Ord RolePropertyDeltaType

