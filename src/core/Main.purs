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
import Control.Monad.Aff (Error, forkAff, runAff)
import Control.Monad.Aff.AVar (AVar, makeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Either (Either(..))
import Perspectives.Api (CreateRequestEmitter, createRequestEmitter, setupApi, setupTcpApi)
import Perspectives.CoreTypes (PerspectivesState)
import Perspectives.Effects (AjaxAvarCache, REACT)
import Perspectives.PerspectivesState (newPerspectivesState, runPerspectivesWithState)
import Prelude (Unit, bind, pure, ($), (<>), show, void, discard)

main :: Eff (AjaxAvarCache (console :: CONSOLE, dom :: DOM, react :: REACT, socketio :: SOCKETIO)) ->
  (CreateRequestEmitter e eff -> Eff (AjaxAvarCache (console :: CONSOLE, dom :: DOM, react :: REACT, socketio :: SOCKETIO)) Unit)
main = void $ runAff handleError do
  -- TODO: retrieve the couchdb credentials from the trusted cluster or through the user interface.
  usr <- pure "cor"
  pwd <- pure "geheim"
  url <- pure "http://127.0.0.1:5984/"
  (av :: AVar String) <- makeVar "This value will be removed on first authentication!"
  state <- makeVar $ newPerspectivesState {userName: usr, couchdbPassword: pwd, couchdbBaseURL: url} av
  void $ forkAff $ runPerspectivesWithState setupApi state
  -- void $ forkAff $ runPerspectivesWithState setupTcpApi state
  f state
  where
    f :: forall eff.
      PerspectivesState
      -> CreateRequestEmitter e eff
      -> Eff (AjaxAvarCache (console :: CONSOLE, dom :: DOM, react :: REACT, socketio :: SOCKETIO)) Unit
    f state createRequestEmitter = void runAff handleError (forkAff $ runPerspectivesWithState setupApi_ createRequestEmitter state)

handleError :: forall e a. (Either Error a -> Eff (console :: CONSOLE | e) Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Perspectives core started!"
