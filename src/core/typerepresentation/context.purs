module Perspectives.Representation.Context where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Kishimen (genericSumToVariant)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision, Revision_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType, ContextType(..), EnumeratedRoleType(..), RoleType)
import Prelude (class Eq, class Show, map, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-----------------------------------------------------------
-- CONTEXT TYPE CLASS
-----------------------------------------------------------
class ContextClass c where
  contextAspects :: c -> Array ContextType
  defaultPrototype :: c -> Maybe ContextInstance
  roleInContext :: c -> Array RoleType
  contextRole :: c -> Array RoleType
  externalRole :: c -> EnumeratedRoleType
  userRole :: c -> Array EnumeratedRoleType
  botRole :: c -> Array EnumeratedRoleType
  actions :: c -> Array ActionType
  aspects :: c -> Array ContextType
  nestedContexts :: c -> Array ContextType

instance contextContextClass :: ContextClass Context where
  contextAspects = _.contextAspects <<< unwrap
  defaultPrototype = _.defaultPrototype <<< unwrap
  roleInContext = _.rolInContext <<< unwrap
  contextRole = _.contextRol <<< unwrap
  externalRole = _.externeRol <<< unwrap
  userRole = _.gebruikerRol <<< unwrap
  botRole = _.botRol <<< unwrap
  actions = _.actions <<< unwrap
  aspects = _.contextAspects <<< unwrap
  nestedContexts = _.nestedContexts <<< unwrap

-----------------------------------------------------------
-- CONTEXT
-----------------------------------------------------------
newtype Context = Context ContextRecord

type ContextRecord =
  { _id :: ContextType
  , _rev :: Revision_
  , displayName :: String
  , kindOfContext :: ContextKind

  , contextAspects :: Array ContextType
  , defaultPrototype :: Maybe ContextInstance

  -- TODO: heroverweeg of dit niet samengevoegd kan worden.
  , rolInContext :: Array RoleType
  , contextRol :: Array RoleType
  , externeRol :: EnumeratedRoleType
  , gebruikerRol :: Array EnumeratedRoleType
  , botRol :: Array EnumeratedRoleType

  , nestedContexts :: Array ContextType
  , actions :: Array ActionType
  , context :: Maybe ContextType
  }

data ContextKind = Domain | Case | Party | Activity | State

defaultContext :: String -> String -> ContextKind -> Maybe String -> Context
defaultContext id dname kind context = Context { _id: (ContextType id)
  , _rev: Nothing
  , displayName: dname
  , kindOfContext: kind
  , contextAspects: []
  , defaultPrototype: Nothing

  , rolInContext: []
  , contextRol: []
  , externeRol: EnumeratedRoleType ""
  , gebruikerRol: []
  , botRol: []

  , nestedContexts: []
  , actions: []
  , context: map ContextType context
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

instance identifiableContext :: Identifiable Context ContextType where
  identifier (Context{_id}) = _id

derive instance genericContextKind :: Generic ContextKind _
instance showContextKind :: Show ContextKind where show = genericShow
derive instance eqContextKind :: Eq ContextKind
instance writeForeignContextKind :: WriteForeign ContextKind where
  writeImpl = writeImpl <<< genericSumToVariant
instance readForeignContextKind :: ReadForeign ContextKind where
  readImpl f = readImpl f
