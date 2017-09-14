-- | A totally unsafe StrMap, tracked by an effect.
-- |

module Perspectives.GlobalUnsafeStrMap
  ( GLStrMap
  , GLOBALMAP
  , new
  , peek
  , poke
  , delete
  ) where

import Control.Monad.Eff (Eff, kind Effect)
import Data.Foreign (Foreign, isUndefined, unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Prelude (bind, pure)

-- | A reference to a mutable map
-- |
-- | The type parameter defines the type of elements of the mutable array.
-- |
foreign import data GLStrMap :: Type -> Type

-- | The GLOBALMAP Effect labels that an unsafe global stringmap is used.
foreign import data GLOBALMAP :: Effect

-- | Create a new, empty mutable map
foreign import new :: forall a. Unit -> GLStrMap a

-- | Get the value for a key in a global map
peek :: forall a e. GLStrMap a -> String -> Eff (gm :: GLOBALMAP | e) (Maybe a)
peek map key = do
  x <- peekImpl map key
  if isUndefined x then pure Nothing else pure (Just (unsafeFromForeign x))

foreign import peekImpl :: forall a e. GLStrMap a -> String -> Eff (gm :: GLOBALMAP | e) Foreign

-- | Update the value for a key in a global map
foreign import poke :: forall a e. GLStrMap a -> String -> a -> Eff (gm :: GLOBALMAP | e) (GLStrMap a)

-- | Remove a key and the corresponding value from a global map
foreign import delete :: forall e a. GLStrMap a -> String -> Eff (gm :: GLOBALMAP | e) (GLStrMap a)
