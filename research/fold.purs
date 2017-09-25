module Research.Fold where

import Prelude
import Data.Maybe(Maybe(..), maybe)
import Data.Array (cons, (..), foldr)
import Data.Int (even)

a :: Array (Maybe Int)
a = map (\x -> if even x then Just x else Nothing) (1..20)

foldit :: Array (Maybe Int) -> Array Int
foldit arr = foldr (maybe id cons) [] arr
