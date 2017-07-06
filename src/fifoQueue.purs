-- | A first-in, first-out queue.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.FifoQueue where

import Data.Unit

foreign import data Queue :: Type -> Type

foreign import queue :: forall a. Unit -> Queue a

foreign import appendToEnd :: forall a. Queue a -> Array a -> Unit

foreign import popFromFront :: forall a. Queue a -> a

foreign import empty :: forall a. Queue a -> Boolean
