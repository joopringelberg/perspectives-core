module Test.Main where

import Test.ObjectsGetterLookup (test)
import Test.TestEffects as TE
import Control.Monad.Aff (Fiber, runAff)
import Control.Monad.Eff (Eff)
import Perspectives.PerspectivesState (runPerspectives)
import Prelude (Unit)

-- import Test.BoundContexts

main :: forall e. Eff (TE.CancelerEffects e) (Fiber (TE.CancelerEffects e) Unit)
main = runAff TE.handleError (runPerspectives "cor" "geheim" test)

-- runTest :: forall e a. Show a => Aff (console :: AC.CONSOLE | e) a -> Eff (console :: AC.CONSOLE | e) Unit
-- runTest t =
--   runAff_ (\_->pure unit) (t >>= (\r -> AC.log (show r)))


-- main = pure 1
