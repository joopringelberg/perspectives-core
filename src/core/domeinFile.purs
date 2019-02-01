module Perspectives.DomeinFile where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, empty, insert)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Perspectives.PerspectivesTypesInPurescript (class Binding, class ContextType, class RolType)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), Revision)
import Prelude (($))
import Unsafe.Coerce (unsafeCoerce)

-- | Constrain:
-- |  * the type parameter c with class ContextType
-- |  * The type parameter r class RolType and
-- |  * the type parameter b with Binding.
newtype DomeinFile = DomeinFile
  { _rev :: Revision
  , _id :: String
  , contexts :: DomeinFileContexts
  , roles ::DomeinFileRoles
  }

derive instance genericDomeinFile :: Generic DomeinFile _

instance encodeDomeinFile :: (Encode c, Encode r, Encode b) => Encode DomeinFile where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodeDomeinFile :: (Decode c, Decode r, Decode b) => Decode DomeinFile where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance respondableDomeinFile :: (Decode c, Decode r, Decode b, Respondable c, Respondable r, Respondable b) => Respondable DomeinFile where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

defaultDomeinFile :: forall c r b. ContextType c => RolType r => Binding b => DomeinFile
defaultDomeinFile = DomeinFile{ _rev: Nothing, _id: "", contexts: empty, roles: empty}

-- | DomeinFileContexts is an immutable map of resource type names to PerspectContexts.
type DomeinFileContexts = StrMap (PerspectContext String)

-- | The type parameter r must be constrained with class RolType and b must be constrained by Binding.
type DomeinFileRoles = StrMap (PerspectRol String String)

-- The same context may be inserted multiple times without consequence; it is an idempotent operation.
addContextToDomeinFile :: forall c. Newtype c String => PerspectContext c -> DomeinFile -> DomeinFile
addContextToDomeinFile c@(PerspectContext {_id}) (DomeinFile dff@{contexts}) = DomeinFile dff {contexts = insert (unwrap _id) (unsafeCoerce c) contexts}

addRolToDomeinFile :: forall r b. Newtype r String => PerspectRol r b -> DomeinFile -> DomeinFile
addRolToDomeinFile c@(PerspectRol {_id}) (DomeinFile dff@{roles}) = DomeinFile dff {roles = insert (unwrap _id) (unsafeCoerce c) roles}

setRevision :: String -> DomeinFile -> DomeinFile
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}
