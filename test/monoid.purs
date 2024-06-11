module Test.Monoid where

import Prelude

import Data.Array (foldMap, foldr, intersect, singleton, union)
import Data.Foldable as FOLD
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, ala, unwrap)
import Perspectives.Representation.ADT (ADT)

f :: Int
f = ala Additive foldMap [1,2,3,4]

f' :: Int
f' = foldr (+) 0 [1, 2, 3]

f'' :: Int
f'' = unwrap $ foldMap Additive [1, 2, 3]

-- Use Conj and Disj to make a Monoid from HArray, so we can use foldMap.
-- In order to be able to use Conj and Disj, HArray should have a HeytingAlgebra instance.
g :: HArray Int
g = ala Conj foldMap [HArray [1, 2], HArray [2, 3]]
-- should be HArray [2]

-- g' :: Array Int
-- g' = ala (Conj <<< HArray) foldMap [[1,2], [2,3]]

g' :: Array Int
g' = foldr union [] [[1, 2], [2, 3]]
-- Should be [1,2,3]

g'' :: Array Int
g'' = unwrap $ unwrap $ foldMap (Conj <<< HArray) [[1, 2], [2, 3]]
-- Should be [2]
 
x :: Conj (HArray Int)
x = Conj (HArray [1])

i :: HArray Int
i = ala Disj foldMap [HArray [1, 2], HArray [2, 3]]
-- Should be HArray [1,2,3]

j :: Array Int
j = unwrap $ ala Disj foldMap $ HArray <$> [[1, 2], [2, 3]]
-- Should be [1,2,3]


newtype HArray a = HArray (Array a)

derive instance Newtype (HArray a) _

-- This instance allows us to create Conj and Disj instances of HArray - both monoids.
-- Those instances allow us to use foldMap to fold HAarray instances under either union or intersection.
instance Eq a => HeytingAlgebra (HArray a) where
  ff = HArray []
  tt = HArray []
  conj (HArray a) (HArray b) = HArray (intersect a b)
  disj (HArray a) (HArray b) = HArray (union a b)
  -- This is nonsense; but we only need ff and tt to be able to implement mempty for the Monoid instances of Conj and Disj,
  -- and conj and disj to implement append for the Semigroup instances of Conj and Disj.
  implies a b = a
  not a = a

-- alleBladeren :: forall a. Eq a => ADT a -> Array a
-- alleBladeren a = unwrap $ unwrap $ FOLD.foldMap (Conj <<< HArray <<< singleton) a
