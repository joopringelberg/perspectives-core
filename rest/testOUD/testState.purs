module TestState
where

import Prelude
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef, runST)

augment :: forall eff h. Int -> Eff (st :: ST h | eff) Int
augment x0 = do
  ref <- newSTRef { x: x0 }
  _ <- modifySTRef ref \o -> { x: o.x + 1 }
  final <- readSTRef ref
  pure final.x

augment' :: Int -> String
augment' x0 = show (runPure (runST (augment x0)))
