module Perspectives.TypesForDeltas where

-----------------------------------------------------------
-- DELTA
-----------------------------------------------------------
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Class (class Encode)
import Foreign.Generic (defaultOptions, genericEncode)
import Foreign.Generic.Class (class GenericEncode)
import Perspectives.EntiteitAndRDFAliases (ID)
import Prelude (class Show, ($))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- DELTA
-----------------------------------------------------------
newtype Delta = Delta
  { id :: ID
  , memberName :: String
  , value :: Maybe String
  , deltaType :: DeltaType
  , isContext :: Boolean
  }

derive instance genericDelta :: Generic Delta _

instance showDelta :: Show Delta where
  show = genericShow

instance encodeDelta :: Encode Delta where
  encode = encodeDefault

derive instance eqDelta :: Eq Delta

derive newtype instance writeForeignDelta :: WriteForeign Delta

-----------------------------------------------------------
-- DELTATYPE
-----------------------------------------------------------
data DeltaType = Add | Remove | Change

derive instance genericDeltaType :: Generic DeltaType _
derive instance eqDeltaType :: Eq DeltaType

instance showDeltaType :: Show DeltaType where
  show = genericShow

instance encodeDeltaType :: Encode DeltaType where
  encode = encodeDefault

instance writeForeignDeltaType :: WriteForeign DeltaType where
  writeImpl Add = unsafeToForeign "Add"
  writeImpl Remove = unsafeToForeign "Remove"
  writeImpl Change = unsafeToForeign "Change"

encodeDefault :: forall t a. Generic a t => GenericEncode t => a -> Foreign
encodeDefault = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
