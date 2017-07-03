-- | The Updatable class encapsulates the idea of updating a value to a new value from a Delta.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.Updatable where

import Delta
import Data.Maybe (Maybe(..))
import Data.Boolean (otherwise)
import Data.Eq (class Eq, (==))

class Eq a <= Updatable a where
	applyUpdate :: a -> Delta a -> a

instance updatableMaybe :: Eq a => Updatable (Maybe a) where
	applyUpdate (Just a) (Erbij (Just b)) = Just b
	applyUpdate (Just a) (Eraf (Just b))
          | (a == b) = Nothing
          | otherwise = Just a
	applyUpdate Nothing (Erbij (Just a)) = Just a
	applyUpdate _ _ = Nothing
