module Perspectives.Representation.CalculatedProperty where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_)
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Prelude (class Eq, class Show, (<<<), (==), ($))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- CALCULATEDPROPERTY
-----------------------------------------------------------
newtype CalculatedProperty = CalculatedProperty CalculatedPropertyRecord

type CalculatedPropertyRecord =
  { _id :: CalculatedPropertyType
  , _rev :: Revision_
  , displayName :: String

  , calculation :: QueryFunctionDescription
  -- , computation :: Maybe (RoleInContext ~~> Value)
  , role :: EnumeratedRoleType
  }

defaultCalculatedProperty :: String -> String -> String -> CalculatedProperty
defaultCalculatedProperty id dn role = CalculatedProperty
  { _id: CalculatedPropertyType id
  , _rev: Nothing
  , displayName: dn
  , calculation: SQD (RDOM $ ST $ (EnumeratedRoleType "")) (DataTypeGetter "") (PDOM (EnumeratedPropertyType ""))
  , role: EnumeratedRoleType role}

derive instance genericRepCalculatedProperty :: Generic CalculatedProperty _

instance showCalculatedProperty :: Show CalculatedProperty where
  show = genericShow

instance eqCalculatedProperty :: Eq CalculatedProperty where
  eq (CalculatedProperty {_id : id1}) (CalculatedProperty {_id : id2}) = id1 == id2

derive instance newtypeCalculatedProperty :: Newtype CalculatedProperty _

derive newtype instance writeForeignCalculatedProperty :: WriteForeign CalculatedProperty

derive newtype instance readForeignCalculatedProperty :: ReadForeign CalculatedProperty

instance revisionCalculatedProperty :: Revision CalculatedProperty where
  rev = _._rev <<< unwrap
  changeRevision s = over CalculatedProperty (\vr -> vr {_rev = s})

instance identifiableCalculatedProperty :: Identifiable CalculatedProperty CalculatedPropertyType where
  identifier (CalculatedProperty{_id}) = _id
