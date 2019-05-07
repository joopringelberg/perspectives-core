-- | A totally unsafe StrMap, tracked by an effect.
-- |

module Perspectives.GlobalUnsafeStrMap
  ( GLStrMap
  , new
  , peek
  , poke
  , delete
  ) where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Effect (Effect)
import Foreign (Foreign, isUndefined, unsafeFromForeign)
import Foreign.Object (Object)
import Prelude (class Show, bind, pure, show)
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable map
-- |
-- | The type parameter defines the type of elements of the mutable array.
-- |
foreign import data GLStrMap :: Type -> Type

-- | Create a new, empty mutable map
foreign import new :: forall a. Unit -> GLStrMap a

-- | Get the value for a key in a global map
peek :: forall a. GLStrMap a -> String -> Effect (Maybe a)
peek map key = do
  x <- peekImpl map key
  if isUndefined x then pure Nothing else pure (Just (unsafeFromForeign x))

foreign import peekImpl :: forall a. GLStrMap a -> String -> Effect Foreign

-- | Update the value for a key in a global map
foreign import poke :: forall a. GLStrMap a -> String -> a -> Effect (GLStrMap a)

-- | Remove a key and the corresponding value from a global map
foreign import delete :: forall a. GLStrMap a -> String -> Effect (GLStrMap a)

instance showGLStrMap :: Show a => Show (GLStrMap a) where
  show s = show (unsafeCoerce s :: Object a)
