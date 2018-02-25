module Test.Main where

import Test.Couchdb
import Test.TestEffects as TE
import Control.Monad.Aff (Aff, Fiber, runAff, runAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Perspectives.PerspectivesState (runPerspectives)
import Prelude (class Show, Unit, pure, unit, (>>=), show)

main :: forall e. Eff (TE.CancelerEffects (ref :: REF | e)) (Fiber (TE.CancelerEffects (ref :: REF | e)) Unit)
main = runAff TE.handleError tt

tt :: forall e. Aff (TE.CancelerEffects (ref :: REF | e)) Unit
tt = (runPerspectives "admin" "admin" test)

runTest :: forall e a. Show a => Aff (console :: CONSOLE | e) a -> Eff (console :: CONSOLE | e) Unit
runTest t =
  runAff_ (\_->pure unit) (t >>= (\r -> log (show r)))


-- main = pure 1
