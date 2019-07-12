module Perspectives.Representation.Context where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Object (Object)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.Class.Revision (class Revision)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, RoleType)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- CONTEXT TYPE CLASS
-----------------------------------------------------------
class ContextClass c where
  contextAspects :: c -> Array ContextType
  defaultPrototype :: c -> Maybe String
  rolInContext :: c -> Object RoleType
  contextRole :: c -> Object RoleType
  externalRole :: c -> EnumeratedRoleType
  userRole :: c -> Object EnumeratedRoleType
  botRole :: c -> Object EnumeratedRoleType

instance contextContextClass :: ContextClass Context where
  contextAspects = _.contextAspects <<< unwrap
  defaultPrototype = _.defaultPrototype <<< unwrap
  rolInContext = _.rolInContext <<< unwrap
  contextRole = _.contextRol <<< unwrap
  externalRole = _.externeRol <<< unwrap
  userRole = _.gebruikerRol <<< unwrap
  botRole = _.botRol <<< unwrap

-----------------------------------------------------------
-- CONTEXT
-----------------------------------------------------------
newtype Context = Context ContextRecord

type ContextRecord =
  { _id :: ContextType
  , _rev :: Revision
  , displayName :: String

  , contextAspects :: Array ContextType
  , defaultPrototype :: Maybe String

  , rolInContext :: Object RoleType
  , contextRol :: Object RoleType
  , externeRol :: EnumeratedRoleType
  , gebruikerRol :: Object EnumeratedRoleType
  , botRol :: Object EnumeratedRoleType
  }

derive instance genericRepContext :: Generic Context _

instance showContext :: Show Context where
  show = genericShow

instance eqContext :: Eq Context where
  eq (Context {_id : id1}) (Context {_id : id2}) = id1 == id2

derive instance newtypeContext :: Newtype Context _

derive newtype instance writeForeignContext :: WriteForeign Context

derive newtype instance readForeignContext :: ReadForeign Context

instance revisionContext :: Revision Context where
  rev = _._rev <<< unwrap
  changeRevision s = over Context (\vr -> vr {_rev = s})
