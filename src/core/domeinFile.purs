module Perspectives.DomeinFile where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign (unsafeFromForeign, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (Object, empty)
import Perspectives.InstanceRepresentation (Revision)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.View (View)
import Prelude (($), (>>>))
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

newtype DomeinFile = DomeinFile
  { _rev :: Revision
  , _id :: String
  , contexts :: DomeinFileContexts
  , enumeratedRoles :: Object EnumeratedRole
  , calculatedRoles :: Object CalculatedRole
  , enumeratedProperties :: Object EnumeratedProperty
  , calculatedProperties :: Object CalculatedProperty
  , views :: Object View
  }

derive instance genericDomeinFile :: Generic DomeinFile _

instance encodeDomeinFile :: Encode DomeinFile where
  -- encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
  encode = writeJSON >>> unsafeToForeign

instance decodeDomeinFile :: Decode DomeinFile where
  -- decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
  decode = unsafeFromForeign >>> readJSON'

derive newtype instance writeForeignDomeinFile :: WriteForeign DomeinFile
derive newtype instance readForeignDomeinFile :: ReadForeign DomeinFile

defaultDomeinFile :: DomeinFile
defaultDomeinFile = DomeinFile{ _rev: Nothing, _id: "", contexts: empty, enumeratedRoles: empty, calculatedRoles: empty, enumeratedProperties: empty, calculatedProperties: empty, views: empty}

-- | DomeinFileContexts is an immutable map of resource type names to PerspectContexts.
type DomeinFileContexts = Object Context

type DomeinFileEnumeratedRoles = Object EnumeratedRole

setRevision :: String -> DomeinFile -> DomeinFile
setRevision vs (DomeinFile dff) = DomeinFile $ dff {_rev = Just vs}
