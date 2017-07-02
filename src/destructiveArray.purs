-- | An Array that just uses assignment without introducing effects.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.DestructiveArray
  ( DestructiveArray
  , setInDestructiveArrayAt
  , pushInDestructiveArray
  , removeFromDestructiveArrayAt
  )
where

newtype DestructiveArray a = DestructiveArray (Array a)

foreign import setInDestructiveArrayAt :: forall a. DestructiveArray a -> Int -> a -> DestructiveArray a

foreign import pushInDestructiveArray :: forall a. DestructiveArray a -> a -> DestructiveArray a

foreign import removeFromDestructiveArrayAt :: forall a. DestructiveArray a -> Int -> DestructiveArray a
