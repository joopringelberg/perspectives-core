--
-- module Main where
--
-- import Test.TypeDefChecker (test)
-- import Test.TestEffects as TE
-- import Control.Monad.Aff (Aff, Fiber, runAff, runAff_)
-- import Control.Monad.Aff.Console (CONSOLE, log) as AC
-- import Control.Monad.Eff (Eff)
-- import Perspectives.PerspectivesState (runPerspectives)
-- import Prelude (class Show, Unit, pure, unit, (>>=), show)
--
-- -- import Test.BoundContexts
--
-- main :: forall e. Eff (TE.CancelerEffects e) (Fiber (TE.CancelerEffects e) Unit)
-- main = runAff TE.handleError (runPerspectives "cor" "geheim" test)






module Main where
import Control.Aff.Sockets (SOCKETIO)
import Control.Monad.Aff (Error, Milliseconds(..), delay, forkAff, runAff)
import Control.Monad.Aff.AVar (AVar, makeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Rec.Class (forever)
import DOM (DOM)
import Data.Either (Either(..))
import Node.FS (FS)
import Node.Process (PROCESS)
import Perspectives.Api (setupApi, setupTcpApi)
import Perspectives.ComputedTripleGetters (addComputedTripleGetters)
import Perspectives.CoreTypes (Transactie, createTransactie)
import Perspectives.Effects (AjaxAvarCache, REACT)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupUser (setupUser)
import Perspectives.TheoryChange (propagate)
import Prelude (Unit, bind, pure, ($), (<>), show, void, discard)

main :: Eff (AjaxAvarCache (console :: CONSOLE, dom :: DOM, react :: REACT, socketio :: SOCKETIO, now :: NOW, fs :: FS, exception :: EXCEPTION, process :: PROCESS)) Unit
main = void $ runAff handleError do
  -- TODO: retrieve the couchdb credentials from the trusted cluster or through the user interface.
  usr <- pure "cor"
  pwd <- pure "geheim"
  url <- pure "http://127.0.0.1:5984/"
  (av :: AVar String) <- makeVar "This value will be removed on first authentication!"
  (tr :: Transactie) <- createTransactie usr
  state <- makeVar $ newPerspectivesState {userName: usr, couchdbPassword: pwd, couchdbBaseURL: url} tr av
  void $ forkAff $ runPerspectivesWithState f state
  void $ forkAff $ runPerspectivesWithState setupTcpApi state
  void $ forkAff $ forever do
    delay (Milliseconds 1000.0)
    -- liftEff $ log "propagating"
    runPerspectivesWithState propagate state
  where
    f = do
      void $ setupUser
      addComputedTripleGetters
      setupApi

handleError :: forall e a. (Either Error a -> Eff (console :: CONSOLE | e) Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Perspectives-core has started!"
