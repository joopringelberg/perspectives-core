module Test.For where

import Prelude
import Data.Array
import Data.Foldable
import Control.MonadZero
import Data.Traversable (for)

--  t = for [1, 2, 3] \n -> do
--    pure (n * n)

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\i ->
  map (\j -> [i, j]) (i..n)) (1..n)

pairs' :: Int -> Array (Array Int)
pairs' n = do
  i <- 1 .. n
  j <- i .. n
  pure [i,j]

factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs n)

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1..n
  j <- i..n
  guard $ i * j == n
  pure [i,j]

--foldl (\acc s -> acc <> (show s) ) "" (1..5) -- "12345"
--foldr (\s acc -> acc <> (show s) ) "" (1..5) -- "54321"

myReverser :: forall a. Array a -> Array a
myReverser = foldr (\x xs -> xs <> [x]) []

myReversel :: forall a. Array a -> Array a
myReversel = foldl (\xs x -> [x] <> xs) []

--(:) <$> [1] <*> (pairs' 2)
