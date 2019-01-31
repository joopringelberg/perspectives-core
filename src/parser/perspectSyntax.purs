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
import Perspectives.PerspectivesTypesInPurescript (BinnenRol, BuitenRol, Context, ContextDef, RolDef, RolInContext, Val)
import Prelude (class Show, ($))

-----------------------------------------------------------
-- PERSPECTCONTEXT
-----------------------------------------------------------

newtype PerspectContext c = PerspectContext (ContextRecord c)

type ContextRecord c =
  { _id :: c
  , _rev :: Revision
  , displayName :: String
  , pspType :: ContextDef
  , binnenRol :: PerspectRol BinnenRol BuitenRol
  , buitenRol :: BuitenRol
  , rolInContext :: StrMap (Array RolInContext)
  , comments :: Comments
  }
 
derive instance genericRepPerspectContext :: Generic (PerspectContext c) _

instance showPerspectContext :: Show c => Show (PerspectContext c) where
  show = genericShow

instance encodePerspectContext :: Encode c => Encode (PerspectContext c) where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodePerspectContext :: Decode c => Decode (PerspectContext c) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance respondablePerspectContext :: Decode c => Respondable (PerspectContext c) where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- PERSPECTROL
-----------------------------------------------------------
-- | The type parameter r must be constrained with class Rol wherever it PerspectRol is used and b must be constrained by Binding.
newtype PerspectRol r b = PerspectRol (RolRecord r b)

type RolRecord r b =
  { _id :: r
  , pspType :: RolDef -- Hier mag alles staan dat gedefinieerd is met psp:Rol. Dus alles waarvan het type psp:Rol is, of waarvan het type psp:Rol als Aspect heeft. Dat zijn de members van de class RolDef.
  , context :: Context
  -- While the fields above occur in every role, those below do not.
  , _rev :: Revision
  , binding :: Maybe b
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
