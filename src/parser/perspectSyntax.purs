module Perspectives.Syntax where

import Perspectives.EntiteitAndRDFAliases

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Perspectives.Identifiers (QualifiedName, PEIdentifier)
import Perspectives.PerspectivesTypesInPurescript (Context, ContextDef, BuitenRol, RolDef, RolInContext, Val)
import Prelude (class Show, ($))

-----------------------------------------------------------
-- PERSPECTCONTEXT
-----------------------------------------------------------
-- | The type parameter r must be constrained with class Rol wherever it PerspectContext is used and b must be constrained by Binding.
newtype PerspectContext r b = PerspectContext (ContextRecord r b)

type ContextRecord r b =
  { _id :: Context
  , _rev :: Revision
  , displayName :: String
  , pspType :: ContextDef
  , binnenRol :: PerspectRol r b
  , buitenRol :: BuitenRol
  , rolInContext :: StrMap (Array RolInContext)
  , comments :: Comments
  }

derive instance genericRepPerspectContext :: Generic (PerspectContext r b) _

instance showPerspectContext :: (Show r, Show b) => Show (PerspectContext r b) where
  show = genericShow

instance encodePerspectContext :: (Encode r, Encode b) => Encode (PerspectContext r b) where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePerspectContext :: (Decode r, Decode b) => Decode (PerspectContext r b) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance respondablePerspectContext :: (Decode r, Decode b, Respondable r, Respondable b) => Respondable (PerspectContext r b) where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- PERSPECTROL
-----------------------------------------------------------
-- | The type parameter r must be constrained with class Rol wherever it PerspectRol is used and b must be constrained by Binding.
newtype PerspectRol r b = PerspectRol (RolRecord r b)

type RolRecord r b =
  { _id :: r
  , pspType :: RolDef
  , context :: Context
  -- While the fields above occur in every role, those below do not.
  , _rev :: Revision
  , binding :: b
  -- The four fields below could also be modeled as Maybe values.
  , properties :: StrMap PropertyValueWithComments
  , gevuldeRollen :: StrMap (Array r)
  , occurrence :: Int
  , comments :: Comments
  }

derive instance genericRepPerspectRol :: Generic (PerspectRol r b) _

instance showPerspectRol :: (Show r, Show b) => Show (PerspectRol r b) where
  show = genericShow

instance encodePerspectRol :: (Encode r, Encode b) => Encode (PerspectRol r b) where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePerspectRol :: (Decode r, Decode b) => Decode (PerspectRol r b) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance respondablePerspectRol :: (Decode r, Decode b, Respondable r, Respondable b) => Respondable (PerspectRol r b) where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- REVISION, BINDING
-----------------------------------------------------------
type Revision = Maybe String

revision :: String -> Revision
revision = Just

-----------------------------------------------------------
-- COMMENTS
-----------------------------------------------------------
newtype Comments = Comments
  { commentBefore :: Array Comment
  , commentAfter :: Array Comment
  }

derive instance genericRepComments :: Generic Comments _

instance showComments :: Show Comments where
  show = genericShow

instance encodeComments :: Encode Comments where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodeComments :: Decode Comments where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- PROPERTYVALUEWITHCOMMENTS
-----------------------------------------------------------
newtype PropertyValueWithComments = PropertyValueWithComments
  { commentBefore :: Array Comment
  , commentAfter :: Array Comment
  , value :: Array Val
  }

derive instance genericRepPropertyValueWithComments :: Generic PropertyValueWithComments _

instance showPropertyValueWithComments :: Show PropertyValueWithComments where
  show = genericShow

instance encodePropertyValueWithComments :: Encode PropertyValueWithComments where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePropertyValueWithComments :: Decode PropertyValueWithComments where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

propertyValue :: PropertyValueWithComments -> Array Val
propertyValue (PropertyValueWithComments{value}) = value

-----------------------------------------------------------
-- CONTEXTDECLARATION, ENCLOSINGCONTEXTDECLARATION
-----------------------------------------------------------
data ContextDeclaration = ContextDeclaration QualifiedName QualifiedName (Array Comment)

derive instance genericContextDeclaration :: Generic ContextDeclaration _

instance showContextDeclaration :: Show ContextDeclaration where
  show = genericShow

data EnclosingContextDeclaration = EnclosingContextDeclaration PEIdentifier (Array Comment)

derive instance genericEnclosingContextDeclaration :: Generic EnclosingContextDeclaration _

instance showEnclosingContextDeclaration :: Show EnclosingContextDeclaration where
  show = genericShow
