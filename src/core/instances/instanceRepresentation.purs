module Perspectives.InstanceRepresentation where

import Perspectives.EntiteitAndRDFAliases

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (Object) as F
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType)
import Prelude (class Show, class Eq, (==), (>>>))
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON, readJSON')

-----------------------------------------------------------
-- PERSPECTCONTEXT TYPE CLASS
-----------------------------------------------------------
class PerspectContextClass c where
  identifier :: c -> String
  pspType :: c -> ContextType
  externalRole :: c -> String
  roles :: c -> F.Object (Array ID)

instance perspectContextPerspectContextClass :: PerspectContextClass PerspectContext where
  identifier c = (unwrap c)._id
  pspType c = (unwrap c).pspType
  externalRole c = (unwrap c).buitenRol
  roles c = (unwrap c).rolInContext

-----------------------------------------------------------
-- PERSPECTCONTEXT
-----------------------------------------------------------
newtype PerspectContext = PerspectContext ContextRecord

type ContextRecord =
  { _id :: ID
  , _rev :: Revision
  , displayName :: String
  , pspType :: ContextType
  , buitenRol :: ID
  , rolInContext :: F.Object (Array ID)
  }

derive instance genericRepPerspectContext :: Generic PerspectContext _

instance showPerspectContext :: Show PerspectContext where
  show = genericShow

instance encodePerspectContext :: Encode PerspectContext where
  encode = writeJSON >>> unsafeToForeign

instance decodePerspectContext :: Decode PerspectContext where
  decode = unsafeFromForeign >>> readJSON'

instance eqPerspectContext :: Eq PerspectContext where
  eq (PerspectContext {_id : id1}) (PerspectContext {_id : id2}) = id1 == id2

derive instance newtypePerspectContext :: Newtype PerspectContext _

derive newtype instance writeForeignPerspectContext :: WriteForeign PerspectContext
derive newtype instance readForeignPerspectContext :: ReadForeign PerspectContext

-----------------------------------------------------------
-- PERSPECTROL
-----------------------------------------------------------
newtype PerspectRol = PerspectRol RolRecord

type RolRecord =
  { _id :: ID
  , pspType :: EnumeratedRoleType
  , context :: ID
  -- While the fields above occur in every role, those below do not.
  , _rev :: Revision
  , binding :: Binding
  -- The four fields below could also be modeled as Maybe values.
  , properties :: F.Object (Array String)
  , gevuldeRollen :: F.Object (Array RolID)
  , occurrence :: Int
  }

derive instance genericRepPerspectRol :: Generic PerspectRol _

instance showPerspectRol :: Show PerspectRol where
  show = genericShow

derive instance newtypePerspectRol :: Newtype PerspectRol _

instance encodePerspectRol :: Encode PerspectRol where
  encode = writeJSON >>> unsafeToForeign

instance decodePerspectRol :: Decode PerspectRol where
  decode = unsafeFromForeign >>> readJSON'

derive newtype instance writeForeignPerspectRol :: WriteForeign PerspectRol

derive newtype instance readForeignPerspectRol :: ReadForeign PerspectRol
-----------------------------------------------------------
-- REVISION, BINDING
-----------------------------------------------------------
type Revision = Maybe String

revision :: String -> Revision
revision = Just

type Binding = Maybe RolID

binding :: RolID -> Binding
binding id = case id of
  "" -> Nothing
  otherwise -> (Just id)
