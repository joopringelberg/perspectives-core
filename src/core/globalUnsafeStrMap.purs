-- | A totally unsafe StrMap, tracked by an effect.
-- |

module Perspectives.GlobalUnsafeStrMap
  ( GLStrMap
  , new
  , peek
  , poke
  , delete
  , clear
  , ensure
  , modify
  , keys
  ) where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Foreign (Foreign, isUndefined, unsafeFromForeign)
import Foreign.Object (Object)
import Prelude (class Show, show)
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable map
-- |
-- | The type parameter defines the type of elements of the mutable array.
-- |
foreign import data GLStrMap :: Type -> Type

-- | Create a new, empty mutable map
foreign import new :: forall a. Unit -> GLStrMap a

-- | Get the value for a key in a global map
peek :: forall a. GLStrMap a -> String -> (Maybe a)
peek map key = let
  x = peekImpl map key in
  if isUndefined x then Nothing else (Just (unsafeFromForeign x))

-- | Look up a key. Returns the value found or the default value and then puts the default under that key in the map.
ensure :: forall a. GLStrMap a -> String -> a -> a
ensure map key default = let
  ma = peek map key in
  case ma of
    Nothing -> let ignore = poke map key default in default
    (Just a) -> a

-- | Modify the value at the key with a function.
modify :: forall a. GLStrMap a -> String -> (a -> a) -> a -> GLStrMap a
modify map key f default = poke map key (f (ensure map key default) )

foreign import peekImpl :: forall a. GLStrMap a -> String -> Foreign

-- | Update the value for a key in a global map
foreign import poke :: forall a. GLStrMap a -> String -> a -> (GLStrMap a)

-- | Remove a key and the corresponding value from a global map
foreign import delete_ :: forall a. GLStrMap a -> String -> Foreign

delete :: forall a. GLStrMap a -> String -> Maybe a
delete map key = let
  x = delete_ map key in
  if isUndefined x then Nothing else (Just (unsafeFromForeign x))

instance showGLStrMap :: Show a => Show (GLStrMap a) where
  show s = show (unsafeCoerce s :: Object a)

-- | Remove all keys and values.
foreign import clear :: forall a. GLStrMap a -> (GLStrMap a)

foreign import keys :: forall a. GLStrMap a -> Array String
