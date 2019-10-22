module Perspectives.Representation.Action where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Kishimen (genericSumToVariant)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition)
import Perspectives.Representation.Assignment (AssignmentStatement)
import Perspectives.Representation.Calculation (Calculation)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType, RoleType, ViewType)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-----------------------------------------------------------
-- ACTION
-----------------------------------------------------------
newtype Action = Action ActionRecord

type ActionRecord =
  { _id :: ActionType
  , _rev :: Revision_
  , displayName :: String

  -- TODO: For synchronization, we might need to allow CalculatedRoles as subject of an action.
  , subject :: EnumeratedRoleType
  , verb :: Verb
  , object :: RoleType
  , requiredObjectProperties :: Maybe ViewType
  , requiredSubjectProperties :: Maybe ViewType
  , requiredIndirectObjectProperties :: Maybe ViewType
  , indirectObject :: Maybe RoleType
  , condition :: Calculation
  , effect :: Maybe AssignmentStatement
  , executedByBot :: Boolean
  , pos :: ArcPosition
  }

derive instance genericRepAction :: Generic Action _

instance showAction :: Show Action where
  show = genericShow

instance eqAction :: Eq Action where
  eq (Action {_id : id1}) (Action {_id : id2}) = id1 == id2

derive instance newtypeAction :: Newtype Action _

derive newtype instance writeForeignAction :: WriteForeign Action

derive newtype instance readForeignAction :: ReadForeign Action

instance revisionAction :: Revision Action where
  rev = _._rev <<< unwrap
  changeRevision s = over Action (\vr -> vr {_rev = s})

instance identifiableAction :: Identifiable Action ActionType where
  identifier (Action{_id}) = _id

-----------------------------------------------------------
-- VERB
-----------------------------------------------------------

data Verb = Create | Consult | Change | Delete | Custom String

derive instance genericRepVerb :: Generic Verb _
instance writeForeignVerb :: WriteForeign Verb where
  writeImpl = writeImpl <<< genericSumToVariant
instance readForeignVerb :: ReadForeign Verb where
  readImpl f = readImpl f
instance showVerb :: Show Verb where
  show = genericShow
instance eqVerb :: Eq Verb where
  eq = genericEq
