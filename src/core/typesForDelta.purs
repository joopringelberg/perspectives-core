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
import Data.Maybe (Maybe)
import Prelude (class Show, ($))

-----------------------------------------------------------
-- DELTA
-----------------------------------------------------------
-- | Type parameter s should be contstrained by Subject, p by Predicate and o by Object.
newtype Delta s p o = Delta
  { id :: s
  , memberName :: p
  , value :: Maybe o
  , deltaType :: DeltaType
  , isContext :: Boolean
  }

derive instance genericDelta :: Generic (Delta s p o) _

instance showDelta :: (Show s, Show p, Show o) => Show (Delta s p o) where
  show = genericShow

instance encodeDelta :: (Encode s, Encode p, Encode o) => Encode (Delta s p o) where
  encode = encodeDefault

derive instance eqDelta :: (Eq s, Eq p, Eq o) => Eq (Delta s p o)

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
