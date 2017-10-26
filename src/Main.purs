module Main where

import Test.TestEffects
import Test.Properties
import Control.Monad.Aff (Fiber, runAff)
import Control.Monad.Eff (Eff)
import Prelude (Unit)

main :: forall e. Eff (CancelerEffects e) (Fiber (CancelerEffects e) Unit)
main = runAff handleError test
