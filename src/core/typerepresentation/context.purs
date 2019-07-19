module Perspectives.Representation.Context where

import Data.Array (null, uncons)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Perspectives.Identifiers (isContainingNamespace)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.Revision (class Revision)
import Perspectives.Representation.TypeIdentifiers (ActionType, CalculatedRoleType(..), ContextType, EnumeratedRoleType(..), RoleType(..))
import Prelude (class Eq, class Show, (<<<), (==), (>>>))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- CONTEXT TYPE CLASS
-----------------------------------------------------------
class ContextClass c where
  contextAspects :: c -> Array ContextType
  defaultPrototype :: c -> Maybe String
  roleInContext :: c -> Array RoleType
  contextRole :: c -> Array RoleType
  externalRole :: c -> EnumeratedRoleType
  userRole :: c -> Array EnumeratedRoleType
  botRole :: c -> Array EnumeratedRoleType
  actions :: c -> Array ActionType

instance contextContextClass :: ContextClass Context where
  contextAspects = _.contextAspects <<< unwrap
  defaultPrototype = _.defaultPrototype <<< unwrap
  roleInContext = _.rolInContext <<< unwrap
  contextRole = _.contextRol <<< unwrap
  externalRole = _.externeRol <<< unwrap
  userRole = _.gebruikerRol <<< unwrap
  botRole = _.botRol <<< unwrap
  actions = _.actions <<< unwrap

-- | If a role with the given qualified name is available, return it as a RoleType. From the type we can find out its RoleKind, too.
lookForRoleType :: String -> Context -> Maybe RoleType
-- TODO: breid uit voor andere roltypen.
-- TODO: breid uit voor Aspecten.
lookForRoleType = lookForRole_ (==)

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleType :: String -> Context -> Maybe RoleType
lookForUnqualifiedRoleType = lookForRole_ isContainingNamespace

type RoleTypeIdentifier = String
lookForRole_ :: (RoleTypeIdentifier -> String -> Boolean) -> RoleTypeIdentifier -> Context -> (Maybe RoleType)
lookForRole_ criterium rn c = some (roleInContext c) (roleTypeIdentifier >>> (criterium rn))
  where

    roleTypeIdentifier :: RoleType -> String
    roleTypeIdentifier (ENR (EnumeratedRoleType i)) = i
    roleTypeIdentifier (CR (CalculatedRoleType i)) = i

    some :: forall a. Array a -> (a -> Boolean) -> Maybe a
    some candidates test | null candidates = Nothing
    some candidates test = case uncons candidates of
      Nothing -> Nothing
      (Just {head, tail}) -> if (test head) then (Just head) else some tail test
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

  , rolInContext :: Array RoleType
  , contextRol :: Array RoleType
  , externeRol :: EnumeratedRoleType
  , gebruikerRol :: Array EnumeratedRoleType
  , botRol :: Array EnumeratedRoleType

  , actions :: Array ActionType
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
