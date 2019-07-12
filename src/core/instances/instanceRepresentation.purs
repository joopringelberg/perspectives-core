module Perspectives.InstanceRepresentation where

import Perspectives.EntiteitAndRDFAliases

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object) as F
import Prelude (class Show, ($), class Eq, (==))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- PERSPECTCONTEXT
-----------------------------------------------------------
newtype PerspectContext = PerspectContext ContextRecord

type ContextRecord =
  { _id :: ID
  , _rev :: Revision
  , displayName :: String
  , pspType :: ID
  , buitenRol :: ID
  , rolInContext :: F.Object (Array ID)
  }

derive instance genericRepPerspectContext :: Generic PerspectContext _

instance showPerspectContext :: Show PerspectContext where
  show = genericShow

instance encodePerspectContext :: Encode PerspectContext where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePerspectContext :: Decode PerspectContext where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance eqPerspectContext :: Eq PerspectContext where
  eq (PerspectContext {_id : id1}) (PerspectContext {_id : id2}) = id1 == id2

derive instance newtypePerspectContext :: Newtype PerspectContext _

derive newtype instance writeForeignPerspectContext :: WriteForeign PerspectContext

-----------------------------------------------------------
-- PERSPECTROL
-----------------------------------------------------------
newtype PerspectRol = PerspectRol RolRecord

type RolRecord =
  { _id :: ID
  , pspType :: ID
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

instance encodePerspectRol :: Encode PerspectRol where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePerspectRol :: Decode PerspectRol where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

derive instance newtypePerspectRol :: Newtype PerspectRol _

derive newtype instance writeForeignPerspectRol :: WriteForeign PerspectRol
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
