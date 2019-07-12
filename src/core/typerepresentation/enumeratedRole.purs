module Perspectives.Representation.EnumeratedRole where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.Class.Revision (class Revision)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, RoleType, RoleKind)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- ENUMERATEDROLE TYPE CLASS
-----------------------------------------------------------
class EnumeratedRoleClass r where
  kindOfRole :: r -> RoleKind
  roleAspects :: r -> Array EnumeratedRoleType
  context :: r -> ContextType
  binding :: r -> RoleType

instance calculatedRoleCalculatedRoleClass :: EnumeratedRoleClass EnumeratedRole where
  kindOfRole r = (unwrap r).kindOfRole
  roleAspects r = (unwrap r).roleAspects
  context r = (unwrap r).context
  binding r = (unwrap r).binding

-----------------------------------------------------------
-- ENUMERATEDROLE
-----------------------------------------------------------
newtype EnumeratedRole = EnumeratedRole EnumeratedRoleRecord

type EnumeratedRoleRecord =
  { _id :: EnumeratedRoleType
  , _rev :: Revision
  , displayName :: String
  , kindOfRole :: RoleKind

  , roleAspects :: Array EnumeratedRoleType

  , context :: ContextType
  , binding :: RoleType
  }

derive instance genericRepEnumeratedRole :: Generic EnumeratedRole _

instance showEnumeratedRole :: Show EnumeratedRole where
  show = genericShow

instance eqEnumeratedRole :: Eq EnumeratedRole where
  eq (EnumeratedRole {_id : id1}) (EnumeratedRole {_id : id2}) = id1 == id2

derive instance newtypeEnumeratedRole :: Newtype EnumeratedRole _

derive newtype instance writeForeignEnumeratedRole :: WriteForeign EnumeratedRole

derive newtype instance readForeignEnumeratedRole :: ReadForeign EnumeratedRole

instance revisionEnumeratedRole :: Revision EnumeratedRole where
  rev = _._rev <<< unwrap
  changeRevision s = over EnumeratedRole (\vr -> vr {_rev = s})
