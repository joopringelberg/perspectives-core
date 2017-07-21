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

-- | A reference to a mutable map
-- |
-- | The type parameter defines the type of elements of the mutable array.
-- |
foreign import data GLStrMap :: Type -> Type

-- | The GLOBALMAP Effect labels that an unsafe global stringmap is used.
foreign import data GLOBALMAP :: Effect

-- | Create a new, empty mutable map
foreign import new :: forall a r. Eff (gm :: GLOBALMAP | r) (GLStrMap a)

-- | Get the value for a key in a global map
peek :: forall a r. GLStrMap a -> String -> Eff (gm :: GLOBALMAP | r) (Maybe a)
peek = peekImpl Just Nothing

foreign import peekImpl :: forall a b r. (a -> b) -> b -> GLStrMap a -> String -> Eff (gm :: GLOBALMAP | r) b

-- | Update the value for a key in a global map
foreign import poke :: forall a r. GLStrMap a -> String -> a -> Eff (gm :: GLOBALMAP | r) (GLStrMap a)

-- | Remove a key and the corresponding value from a global map
foreign import delete :: forall a r. GLStrMap a -> String -> Eff (gm :: GLOBALMAP | r) (GLStrMap a)
