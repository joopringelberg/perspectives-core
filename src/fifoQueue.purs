-- | A first-in, first-out queue.
-- |
-- | **Copyright** Perspectives-IT 2017
-- |
-- | **Author** Joop Ringelberg

module Perspectives.FifoQueue where

import Data.Unit
import Data.Boolean (otherwise)
import Data.Maybe (Maybe(..))

foreign import data Queue :: Type -> Type

foreign import queue :: forall a. Array a -> Queue a

foreign import appendToEnd :: forall a. Queue a -> Array a -> Queue a

foreign import next :: forall a. Queue a -> a

foreign import empty :: forall a. Queue a -> Boolean

foreign import cumulator :: forall a. Queue a -> Array a
