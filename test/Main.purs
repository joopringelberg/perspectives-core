module Test.Main where

import Control.Monad.Aff (Fiber, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, empty, insert)
import Node.FS (FS)
import Perspectives.PerspectivesState (runPerspectives)
import Prelude (Unit, bind, discard, pure, unit, ($))
import Test.SortModules (test)
import Test.TestEffects as TE
import Unsafe.Coerce (unsafeCoerce)

-- import Test.BoundContexts

-- main = test
-- main :: forall e. Eff (TE.TestEffects (fs :: FS | e)) (Fiber (TE.TestEffects (fs :: FS | e)) Unit)
-- main = runAff TE.handleError (runPerspectives "cor" "geheim" test)

-- runTest :: forall e a. Show a => Aff (console :: AC.CONSOLE | e) a -> Eff (console :: AC.CONSOLE | e) Unit
-- runTest t =
--   runAff_ (\_->pure unit) (t >>= (\r -> AC.log (show r)))


-- main = pure 1


main :: forall e a. StringOrInt a => Eff (console :: CONSOLE) Unit
main = do
  ((Store store) :: Store a) <- pure $ Store empty
  _ <- pure $ insert "one" (unsafeCoerce (One 1)) store
  _ <- pure $ insert "twee" (unsafeCoerce (Two "twee")) store
  pure unit

newtype Store a = Store (StrMap a)
newtype One = One Int
newtype Two = Two String

class StringOrInt a

instance stringOrIntOne :: StringOrInt One
instance stringOrIntTwo :: StringOrInt Two
