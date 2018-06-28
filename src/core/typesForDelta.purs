module Perspectives.TypesForDeltas where

-----------------------------------------------------------
-- DELTA
-----------------------------------------------------------
import Data.Eq (class Eq)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Data.Foreign.Generic.Class (class GenericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Perspectives.EntiteitAndRDFAliases (ID)
import Prelude (class Show, ($))

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

encodeDefault :: forall t a. Generic a t => GenericEncode t => a -> Foreign
encodeDefault = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
