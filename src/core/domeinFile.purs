module Perspectives.DomeinFile where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign (unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object, empty, insert)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), Revision)
import Prelude (($), (>>>))
import Simple.JSON (class WriteForeign, writeJSON)

newtype DomeinFile = DomeinFile
  { _rev :: Revision
  , _id :: String
  , contexts :: DomeinFileContexts
  , roles ::DomeinFileRoles
  }

derive instance genericDomeinFile :: Generic DomeinFile _

instance encodeDomeinFile :: Encode DomeinFile where
  -- encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
  encode = writeJSON >>> unsafeToForeign

instance decodeDomeinFile :: Decode DomeinFile where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

derive newtype instance writeForeignDomeinFile :: WriteForeign DomeinFile

defaultDomeinFile :: DomeinFile
defaultDomeinFile = DomeinFile{ _rev: Nothing, _id: "", contexts: empty, roles: empty}

-- | DomeinFileContexts is an immutable map of resource type names to PerspectContexts.
type DomeinFileContexts = Object PerspectContext

type DomeinFileRoles = Object PerspectRol

-- The same context may be inserted multiple times without consequence; it is an idempotent operation.
addContextToDomeinFile :: PerspectContext -> DomeinFile -> DomeinFile
addContextToDomeinFile c@(PerspectContext {_id}) (DomeinFile dff@{contexts}) = DomeinFile dff {contexts = insert _id c contexts}

addRolToDomeinFile :: PerspectRol -> DomeinFile -> DomeinFile
addRolToDomeinFile c@(PerspectRol {_id}) (DomeinFile dff@{roles}) = DomeinFile dff {roles = insert _id c roles}

setRevision :: String -> DomeinFile -> DomeinFile
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}
