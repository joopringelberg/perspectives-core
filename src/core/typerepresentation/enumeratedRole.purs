module Perspectives.Representation.EnumeratedRole where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, PropertyType, RoleKind)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- ENUMERATEDROLE
-----------------------------------------------------------
newtype EnumeratedRole = EnumeratedRole EnumeratedRoleRecord

type EnumeratedRoleRecord =
  { _id :: EnumeratedRoleType
  , _rev :: Revision_
  , displayName :: String
  , kindOfRole :: RoleKind

  , roleAspects :: Array EnumeratedRoleType
  , properties :: Array PropertyType

  , context :: ContextType
  , binding :: ADT EnumeratedRoleType

  , functional :: Boolean
  , mandatory :: Boolean
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

instance identifiableEnumeratedRole :: Identifiable EnumeratedRole EnumeratedRoleType where
  identifier (EnumeratedRole{_id}) = _id
