module Perspectives.DomeinFile where

import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, empty, insert)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Perspectives.PerspectivesTypesInPurescript (class Binding, Context(..))
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), Revision)
import Prelude (($))

-- | The type parameter r must be constrained with class Rol and b must be constrained by Binding.
newtype DomeinFile r b = DomeinFile
  { _rev :: Revision
  , _id :: String
  , contexts :: DomeinFileContexts
  , roles ::DomeinFileRoles r b
  }

derive instance genericDomeinFile :: Generic (DomeinFile a) _

instance encodeDomeinFile :: (Encode a) => Encode (DomeinFile a) where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance respondableDomeinFile :: (Respondable a) => Respondable (DomeinFile a) where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

defaultDomeinFile :: Binding a => DomeinFile a
defaultDomeinFile = DomeinFile{ _rev: Nothing, _id: "", contexts: empty, roles: empty}

-- | DomeinFileContexts is an immutable map of resource type names to PerspectContexts.
-- | The type parameter r must be constrained with class Rol and b must be constrained by Binding.
type DomeinFileContexts r b= StrMap (PerspectContext r b)

-- | The type parameter r must be constrained with class Rol and b must be constrained by Binding.
type DomeinFileRoles r b = StrMap (PerspectRol r b)

-- The same context may be inserted multiple times without consequence; it is an idempotent operation.
addContextToDomeinFile :: Binding a => PerspectContext -> DomeinFile a -> DomeinFile a
addContextToDomeinFile c@(PerspectContext {_id:(Context id)}) (DomeinFile dff@{contexts}) = DomeinFile dff {contexts = insert id c contexts}

addRolToDomeinFile :: Binding a => PerspectRol a -> DomeinFile a -> DomeinFile a
addRolToDomeinFile c@(PerspectRol {_id}) (DomeinFile dff@{roles}) = DomeinFile dff {roles = insert (unwrap _id) c roles}

setRevision :: Binding a => String -> DomeinFile a -> DomeinFile a
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}
