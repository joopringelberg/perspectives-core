module Perspectives.DomeinFile where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, empty, insert)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Perspectives.PerspectivesTypesInPurescript (class Binding, class Rol, Context(..))
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), Revision)
import Prelude (($))

-- | The type parameter r must be constrained with class Rol and b must be constrained by Binding.
newtype DomeinFile r b = DomeinFile
  { _rev :: Revision
  , _id :: String
  , contexts :: DomeinFileContexts
  , roles ::DomeinFileRoles r b
  }

derive instance genericDomeinFile :: Generic (DomeinFile r b) _

instance encodeDomeinFile :: (Encode r, Encode b) => Encode (DomeinFile r b) where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodeDomeinFile :: (Decode r, Decode b) => Decode (DomeinFile r b) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance respondableDomeinFile :: (Decode r, Decode b, Respondable r, Respondable b) => Respondable (DomeinFile r b) where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

defaultDomeinFile :: forall r b. Binding r => Binding b => DomeinFile r b
defaultDomeinFile = DomeinFile{ _rev: Nothing, _id: "", contexts: empty, roles: empty}

-- | DomeinFileContexts is an immutable map of resource type names to PerspectContexts.
-- | The type parameter r must be constrained with class Rol and b must be constrained by Binding.
type DomeinFileContexts = StrMap PerspectContext

-- | The type parameter r must be constrained with class Rol and b must be constrained by Binding.
type DomeinFileRoles r b = StrMap (PerspectRol r b)

-- The same context may be inserted multiple times without consequence; it is an idempotent operation.
addContextToDomeinFile :: forall r b. Rol r => Binding b => PerspectContext -> DomeinFile r b -> DomeinFile r b
addContextToDomeinFile c@(PerspectContext {_id:(Context id)}) (DomeinFile dff@{contexts}) = DomeinFile dff {contexts = insert id c contexts}

addRolToDomeinFile :: forall r b. Rol r => Binding b => PerspectRol r b -> DomeinFile r b -> DomeinFile r b
addRolToDomeinFile c@(PerspectRol {_id}) (DomeinFile dff@{roles}) = DomeinFile dff {roles = insert (unwrap _id) c roles}

setRevision :: forall r b. Rol r => Binding b => String -> DomeinFile r b -> DomeinFile r b
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}
