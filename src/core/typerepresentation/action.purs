module Perspectives.Representation.Action where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, unwrap)
import Kishimen (genericSumToVariant)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.Class.Revision (class Revision)
import Perspectives.Representation.QueryFunction (QueryFunction)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedRoleType, PropertyType, RoleType)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-----------------------------------------------------------
-- ACTION TYPE CLASS
-----------------------------------------------------------
class ActionClass c where
  subject :: c -> Array EnumeratedRoleType
  verb :: c -> Verb
  object :: c -> RoleType
  indirectObject :: c -> RoleType
  requiredObjectProperties :: c -> Array PropertyType
  condition :: c -> QueryFunction

instance contextActionClass :: ActionClass Action where
  subject = _.subject <<< unwrap
  verb = _.verb <<< unwrap
  object = _.object <<< unwrap
  indirectObject = _.indirectObject <<< unwrap
  requiredObjectProperties = _.requiredObjectProperties <<< unwrap
  condition = _.condition <<< unwrap

-----------------------------------------------------------
-- ACTION
-----------------------------------------------------------
newtype Action = Action ActionRecord

type ActionRecord =
  { _id :: ActionType
  , _rev :: Revision
  , displayName :: String

  , subject :: Array EnumeratedRoleType
  , verb :: Verb
  , object :: RoleType
  , requiredObjectProperties :: Array PropertyType
  , indirectObject :: RoleType
  , condition :: QueryFunction
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
