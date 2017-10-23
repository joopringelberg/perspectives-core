module Perspectives.ObjectCollection where

import Data.Array (head, null)
import Data.Maybe (Maybe(..))
import Prelude (class Functor, id)

class Functor ef <= ObjectCollection ef where
  empty :: forall a. ef a
  isEmpty :: forall a. ef a -> Boolean
  fromArray :: forall a. Array a -> ef a

instance objectInMaybe :: ObjectCollection Maybe where
  empty = Nothing
  isEmpty Nothing = true
  isEmpty otherwise = false
  fromArray = head

instance objectsInArray :: ObjectCollection Array where
  empty = []
  isEmpty = null
  fromArray = id
