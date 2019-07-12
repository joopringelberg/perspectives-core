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

newtype CalculatedRole = CalculatedRole ComputedRoleRecord

type ComputedRoleRecord =
  { _id :: CalculatedRoleType
  , _rev :: Revision
  , displayName :: String
  , kindOfRole :: RoleKind

  , calculation :: QueryFunction
  , context :: ContextType
  }

derive instance genericRepComputedRole :: Generic CalculatedRole _

instance showComputedRole :: Show CalculatedRole where
  show = genericShow

instance eqComputedRole :: Eq CalculatedRole where
  eq (CalculatedRole {_id : id1}) (CalculatedRole {_id : id2}) = id1 == id2

derive instance newtypeComputedRole :: Newtype CalculatedRole _

derive newtype instance writeForeignComputedRole :: WriteForeign CalculatedRole

derive newtype instance readForeignComputedRole :: ReadForeign CalculatedRole

instance revisionComputedRole :: Revision CalculatedRole where
  rev = _._rev <<< unwrap
  changeRevision s = over CalculatedRole (\vr -> vr {_rev = s})
