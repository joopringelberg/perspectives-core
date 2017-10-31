module Research.ContravariantFunctor where

import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.String (length)
import Prelude ((<<<), (<=))

newtype Predicate a = Predicate{ p :: a -> Boolean}

instance contravariantPredicate :: Contravariant Predicate where
  -- cmap :: (b -> a) -> Predicate a -> Predicate b
  cmap f (Predicate{p}) = Predicate {p : (p <<< f)}

-- f :: Int -> Boolean
-- f a = a <= 4

p1 :: Predicate Int
p1 = Predicate { p : (\a -> a <= 4)}

runPredicate (Predicate{p}) = p

test = runPredicate p1 3

p2 = cmap length p1

newtype Kleisli m b c = Kleisli{ runKleisli :: b -> m c}
