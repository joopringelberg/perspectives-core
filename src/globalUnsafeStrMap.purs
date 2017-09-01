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
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)

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
peek :: forall a. GLStrMap a -> String -> (Maybe a)
peek = peekImpl Just Nothing

foreign import peekImpl :: forall a b. (a -> b) -> b -> GLStrMap a -> String -> b

-- | Update the value for a key in a global map
foreign import poke :: forall a e. GLStrMap a -> String -> a -> Eff (gm :: GLOBALMAP | e) (GLStrMap a)

-- | Remove a key and the corresponding value from a global map
foreign import delete :: forall e a. GLStrMap a -> String -> Eff (gm :: GLOBALMAP | e) (GLStrMap a)
