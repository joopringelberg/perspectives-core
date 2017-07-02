-- | A map object that just uses assignment without introducing effects.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.DestructiveMap
  ( DestructiveMap
  , getFromMap
  , setInMap
  , createMap
  , deleteFromMap
  )
where

import Data.Show

-- | `Map a` represents a map from `String`s to values of type `a`.
import Data.Unit (Unit)

-- | `Map a` represents a map from `String`s to values of type `a`.
foreign import data DestructiveMap :: Type -> Type

-- | Unsafely get the value for a key in a map.
-- |
-- | This function does not check whether the key exists in the map.
foreign import getFromMap :: forall a. DestructiveMap a -> String -> a

-- | Unsafely set the value for a key in a map.
foreign import setInMap :: forall a. DestructiveMap a -> String -> a -> DestructiveMap a

-- | Delete a value from a map by its key
foreign import deleteFromMap :: forall a. DestructiveMap a -> String -> DestructiveMap a

-- | Create an empty map.
foreign import createMap :: forall a. Unit -> DestructiveMap a

-- | Show a map.
foreign import showMapImpl :: forall a. DestructiveMap a -> String

-- | DestructiveMap is an instance of Show.
instance showMap :: Show (DestructiveMap a) where
  show m = showMapImpl m
