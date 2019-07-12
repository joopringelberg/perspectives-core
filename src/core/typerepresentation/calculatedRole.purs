module Perspectives.Representation.CalculatedRole where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.Class.Revision (class Revision)
import Perspectives.Representation.QueryFunction (QueryFunction)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType, ContextType, RoleKind)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- CALCULATED ROLE TYPE CLASS
-----------------------------------------------------------
class CalculatedRoleClass r where
  kindOfRole :: r -> RoleKind
  calculation :: r -> QueryFunction
  context :: r -> ContextType

instance calculatedRoleCalculatedRoleClass :: CalculatedRoleClass CalculatedRole where
  kindOfRole r = (unwrap r).kindOfRole
  calculation r = (unwrap r).calculation
  context r = (unwrap r).context

-----------------------------------------------------------
-- CALCULATEDROLE
-----------------------------------------------------------
newtype CalculatedRole = CalculatedRole CalculatedRoleRecord

type CalculatedRoleRecord =
  { _id :: CalculatedRoleType
  , _rev :: Revision
  , displayName :: String
  , kindOfRole :: RoleKind

  , calculation :: QueryFunction
  , context :: ContextType
  }

derive instance genericRepCalculatedRole :: Generic CalculatedRole _

instance showCalculatedRole :: Show CalculatedRole where
  show = genericShow

instance eqCalculatedRole :: Eq CalculatedRole where
  eq (CalculatedRole {_id : id1}) (CalculatedRole {_id : id2}) = id1 == id2

derive instance newtypeCalculatedRole :: Newtype CalculatedRole _

derive newtype instance writeForeignCalculatedRole :: WriteForeign CalculatedRole

derive newtype instance readForeignCalculatedRole :: ReadForeign CalculatedRole

instance revisionCalculatedRole :: Revision CalculatedRole where
  rev = _._rev <<< unwrap
  changeRevision s = over CalculatedRole (\vr -> vr {_rev = s})
