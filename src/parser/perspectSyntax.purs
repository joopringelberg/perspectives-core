module Perspectives.Syntax where

import Perspectives.EntiteitAndRDFAliases
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Perspectives.Identifiers (QualifiedName, PEIdentifier)
import Prelude (class Show, ($))

-----------------------------------------------------------
-- PERSPECTCONTEXT
-----------------------------------------------------------
newtype PerspectContext = PerspectContext ContextRecord

type ContextRecord =
  { _id :: ID
  , _rev :: Revision
  , displayName :: String
  , pspType :: ID
  , binnenRol :: PerspectRol
  , buitenRol :: ID
  , rolInContext :: StrMap (Array ID)
  , comments :: Comments
  }

derive instance genericRepPerspectContext :: Generic PerspectContext _

instance showPerspectContext :: Show PerspectContext where
  show = genericShow

instance encodePerspectContext :: Encode PerspectContext where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePerspectContext :: Decode PerspectContext where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance respondablePerspectContext :: Respondable PerspectContext where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

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
  , properties :: StrMap PropertyValueWithComments
  , gevuldeRollen :: StrMap (Array RolID)
  , occurrence :: Int
  , comments :: Comments
  }

derive instance genericRepPerspectRol :: Generic PerspectRol _

instance showPerspectRol :: Show PerspectRol where
  show = genericShow

instance encodePerspectRol :: Encode PerspectRol where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePerspectRol :: Decode PerspectRol where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance respondablePerspectRol :: Respondable PerspectRol where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- REVISION, BINDING
-----------------------------------------------------------
type Revision = NullOrUndefined String

fromRevision :: Revision -> Maybe String
fromRevision = unNullOrUndefined

toRevision :: Maybe String -> Revision
toRevision = NullOrUndefined

revision :: String -> Revision
revision r = NullOrUndefined (Just r)

revision' :: Maybe String -> Revision
revision' = NullOrUndefined

noRevision :: Revision
noRevision = NullOrUndefined Nothing

type Binding = NullOrUndefined RolID

fromBinding :: Binding -> Maybe RolID
fromBinding = unNullOrUndefined

binding :: RolID -> Binding
binding id = NullOrUndefined (Just id)

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
  , value :: Array String
  }

derive instance genericRepPropertyValueWithComments :: Generic PropertyValueWithComments _

instance showPropertyValueWithComments :: Show PropertyValueWithComments where
  show = genericShow

instance encodePropertyValueWithComments :: Encode PropertyValueWithComments where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePropertyValueWithComments :: Decode PropertyValueWithComments where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

propertyValue :: PropertyValueWithComments -> Array String
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
