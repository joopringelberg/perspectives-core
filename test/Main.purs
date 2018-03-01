module Test.Main where

import Test.TestEffects as TE
import Control.Monad.Aff (Aff, Fiber, runAff, runAff_)
import Control.Monad.Aff.Console (CONSOLE, log) as AC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Perspectives.PerspectivesState (runPerspectives)
import Prelude (class Show, Unit, pure, unit, (>>=), show)

import Test.Properties

main :: forall e. Eff (TE.CancelerEffects (ref :: REF | e)) (Fiber (TE.CancelerEffects (ref :: REF | e)) Unit)
main = runAff TE.handleError tt

tt :: forall e. Aff (TE.CancelerEffects (ref :: REF | e)) Unit
tt = (runPerspectives "cor" "geheim" test)

runTest :: forall e a. Show a => Aff (console :: AC.CONSOLE | e) a -> Eff (console :: AC.CONSOLE | e) Unit
runTest t =
  runAff_ (\_->pure unit) (t >>= (\r -> AC.log (show r)))


-- main = pure 1
