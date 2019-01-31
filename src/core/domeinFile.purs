module Perspectives.DomeinFile where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, empty, insert)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Perspectives.PerspectivesTypesInPurescript (class Binding, class ContextType, class RolType)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), Revision)
import Prelude (($))

-- | Constrain:
-- |  * the type parameter c with class ContextType
-- |  * The type parameter r class RolType and
-- |  * the type parameter b with Binding.
newtype DomeinFile c r b = DomeinFile
  { _rev :: Revision
  , _id :: String
  , contexts :: DomeinFileContexts c
  , roles ::DomeinFileRoles r b
  }

derive instance genericDomeinFile :: Generic (DomeinFile c r b) _

instance encodeDomeinFile :: (Encode c, Encode r, Encode b) => Encode (DomeinFile c r b) where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodeDomeinFile :: (Decode c, Decode r, Decode b) => Decode (DomeinFile c r b) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance respondableDomeinFile :: (Decode c, Decode r, Decode b, Respondable c, Respondable r, Respondable b) => Respondable (DomeinFile c r b) where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

defaultDomeinFile :: forall c r b. ContextType c => RolType r => Binding b => DomeinFile c r b
defaultDomeinFile = DomeinFile{ _rev: Nothing, _id: "", contexts: empty, roles: empty}

-- | DomeinFileContexts is an immutable map of resource type names to PerspectContexts.
-- | The type parameter r must be constrained with class RolType and b must be constrained by Binding.
type DomeinFileContexts c = StrMap (PerspectContext c)

-- | The type parameter r must be constrained with class RolType and b must be constrained by Binding.
type DomeinFileRoles r b = StrMap (PerspectRol r b)

-- The same context may be inserted multiple times without consequence; it is an idempotent operation.
addContextToDomeinFile :: forall c r b. ContextType c => RolType r => Binding b => PerspectContext c -> DomeinFile c r b -> DomeinFile c r b
addContextToDomeinFile c@(PerspectContext {_id}) (DomeinFile dff@{contexts}) = DomeinFile dff {contexts = insert (unwrap _id) c contexts}

addRolToDomeinFile :: forall c r b. ContextType c => RolType r => Binding b => PerspectRol r b -> DomeinFile c r b -> DomeinFile c r b
addRolToDomeinFile c@(PerspectRol {_id}) (DomeinFile dff@{roles}) = DomeinFile dff {roles = insert (unwrap _id) c roles}

setRevision :: forall c r b. ContextType c => RolType r => Binding b => String -> DomeinFile c r b -> DomeinFile c r b
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}
