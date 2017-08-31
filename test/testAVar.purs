module Test.AVar where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, runAff, makeAff, launchAff, delay, forkAff, forkAll, Canceler(..), cancel, attempt, finally, apathize)
import Control.Monad.Aff.AVar (AVAR, makeVar, makeVar', putVar, modifyVar, takeVar, peekVar, killVar, tryTakeVar, tryPeekVar, AVar)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log) as Eff
import Control.Monad.Eff.Exception (EXCEPTION, catchException, error, message, throwException, try)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parallel, sequential)
import Data.Either (either, fromLeft, fromRight)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable (replicate)
import Data.Unit (Unit)
import Partial.Unsafe (unsafePartial)


type Test a = forall e. Aff (console :: CONSOLE | e) a

-- An asynchronous computation of a value labeled with the CONSOLE and AVAR effect.
type TestAVar a = forall e. Aff (console :: CONSOLE, avar :: AVAR | e) a

timeout :: Milliseconds → TestAVar Unit → TestAVar Unit
timeout ms aff = do
  exn <- makeVar
  clr1 <- forkAff (delay ms *> putVar exn (Just "Timed out"))
  clr2 <- forkAff (aff *> putVar exn Nothing)
  res ← takeVar exn
  log (show res)
  case res of
    Nothing -> void (clr1 `cancel` error "Done")
    Just e -> void (clr2 `cancel` error "Done") *> throwError (error e)

test_peekVar :: TestAVar Unit
test_peekVar = do
    v <- makeVar
    _ <- forkAff (delay (Milliseconds 0.0) *> putVar v 1.0)
    a1 <- peekVar v
    a2 <- takeVar v
    when (a1 /= a2) do
      throwError (error "Something horrible went wrong - peeked var is not equal to taken var")
    log ("Success: Peeked value not consumed")

test :: Eff (console :: CONSOLE, avar :: AVAR, exception :: EXCEPTION) Unit
test = do
  void $ -- void geeft unit in de betreffende monad, c.q. Eff (met CONSOLE en EXCEPTION).
    runAff -- runAff neemt een error handler en een success callback en een computation in Aff en geeft een computation in Eff met de canceler. Oftewel, de asynchrone berekening wordt een synchrone.
      throwException -- de error handler: neemt een error en produceert een computation in Eff (met EXCEPTION)
      (const (pure unit)) -- de succes callback: consumeert elke waarde en geeft een computation in Eff van Unit.
      $ do test_putTakeVar -- de computation (van unit) in Aff, gelabeled met CONSOLE en AVAR.

test_putTakeVar :: TestAVar Unit
test_putTakeVar = do
  v <- makeVar
  _ <- forkAff (delay (Milliseconds 0.0) *> putVar v 1.0)
  _ <- forkAff $ peeksAtVar v "Looking in one async computation, the value is: "
  _ <- forkAff $ peeksAtVar v "Looking in another async computation, the value is: "
  a <- takeVar v
  log ("Success: Value " <> show a)

peeksAtVar :: forall e. AVar Number -> String -> Aff (avar :: AVAR, console :: CONSOLE | e ) Unit
peeksAtVar v message = do
  c <- peekVar v
  log( message <> show c)
