-- | The Updatable class encapsulates the idea of updating a value to a new value
-- | from a Delta.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Updatable where

import Delta
import Data.Maybe (Maybe(..))
import Data.Boolean (otherwise)
import Data.Eq (class Eq, (==))
import Data.Array (union, difference)

class Eq a <= Updatable a where
	applyUpdate :: a -> Delta a -> a

-- | The Maybe instance of Updatable adds or replaces an Erbij Delta value and
-- | removes an Eraf Delta value to return Nothing, or has no effect.
instance updatableMaybe :: Eq a => Updatable (Maybe a) where
	applyUpdate (Just a) (Erbij (Just b)) = Just b
	applyUpdate (Just a) (Eraf (Just b))
          | (a == b) = Nothing
          | otherwise = Just a
	applyUpdate Nothing (Erbij (Just a)) = Just a
	applyUpdate _ _ = Nothing

-- | The Array instance of Updatable unions an Erbij Array value and subtracts an Eraf Array
-- | value.
instance updatableArray :: Eq a => Updatable (Array a) where
	applyUpdate ar (Erbij extra) = ar `union` extra
	applyUpdate ar (Eraf minus) = ar `difference` minus
