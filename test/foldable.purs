module Research.Foldable where

import Prelude
import Data.Array (singleton, (..))
import Data.Traversable (for, traverse, sequence)

x :: Array (Array Int)
x = for [1, 2, 3] (\n -> do
  pure (n * n))

y :: Array (Array Int)
y = traverse (\n -> [n * 2]) (1..10)

z :: Array (Array Int)
z = sequence (map (((*)2) >>> singleton) (1..10))
