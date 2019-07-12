module Perspectives.Representation.Context where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Foreign.Object (Object)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.Class.Revision (class Revision)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRolType, RoleType)
import Prelude (class Eq, class Show, (<<<), (==))
import Simple.JSON (class ReadForeign, class WriteForeign)

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
  , externeRol :: EnumeratedRolType
  , gebruikerRol :: Object EnumeratedRolType
  , botRol :: Object EnumeratedRolType
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
