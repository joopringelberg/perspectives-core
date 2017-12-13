module Test.Main where

import Test.TestEffects as TE
import Control.Monad.Aff (Fiber, runAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Prelude (Unit, pure)

-- main :: forall e. Eff (TE.TestEffects (console :: CONSOLE | e)) (Fiber (TE.TestEffects (console :: CONSOLE | e)) Unit)
-- main = runAff TE.handleError test

main = pure 1
