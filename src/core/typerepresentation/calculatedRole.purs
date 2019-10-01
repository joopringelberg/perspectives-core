module Perspectives.Representation.CalculatedRole where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), RoleKind, RoleType(..))
import Prelude (class Eq, class Show, (<<<), (==), ($))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- CALCULATEDROLE
-----------------------------------------------------------
newtype CalculatedRole = CalculatedRole CalculatedRoleRecord

type CalculatedRoleRecord =
  { _id :: CalculatedRoleType
  , _rev :: Revision_
  , displayName :: String
  , kindOfRole :: RoleKind

  , calculation :: QueryFunctionDescription
  , context :: ContextType

  , pos :: ArcPosition
  }

defaultCalculatedRole :: String -> String -> RoleKind -> String -> ArcPosition -> CalculatedRole
defaultCalculatedRole qname dname kindOfRole context pos = CalculatedRole
  { _id: CalculatedRoleType qname
  , _rev: Nothing
  , displayName: dname
  , kindOfRole: kindOfRole

  , calculation: SQD (CDOM $ ST $ (ContextType "")) (RolGetter (ENR (EnumeratedRoleType ""))) (RDOM (ST (EnumeratedRoleType "")))
  , context: ContextType context

  , pos: pos
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

instance identifiableCalculatedRole :: Identifiable CalculatedRole CalculatedRoleType where
  identifier (CalculatedRole{_id}) = _id
