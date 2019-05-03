--
-- module Main where
--
-- import Test.TypeDefChecker (test)
-- import Test.TestEffects as TE
-- import Effect.Aff (Aff, Fiber, runAff, runAff_)
-- import Effect.Aff.Console (CONSOLE, log) as AC
-- import Effect (Effect)
-- import Perspectives.PerspectivesState (runPerspectives)
-- import Prelude (class Show, Unit, pure, unit, (>>=), show)
--
-- -- import Test.BoundContexts
--
-- main :: forall e. Effect (TE.CancelerEffects e) (Fiber (TE.CancelerEffects e) Unit)
-- main = runAff TE.handleError (runPerspectives "cor" "geheim" test)






module Main where
import Effect.Aff (Error, Milliseconds(..), delay, forkAff, runAff)
import Effect.Aff.AVar (AVar, new)
import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Perspectives.Api (setupApi, setupTcpApi)
import Perspectives.ComputedTripleGetters (addComputedTripleGetters)
import Perspectives.CoreTypes (Transactie, createTransactie)

import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupUser (setupUser)
import Perspectives.TheoryChange (propagate)
import Prelude (Unit, bind, pure, ($), (<>), show, void, discard)

main :: Effect Unit
main = void $ runAff handleError do
  -- TODO: retrieve the couchdb credentials from the trusted cluster or through the user interface.
  usr <- pure "cor"
  pwd <- pure "geheim"
  url <- pure "http://127.0.0.1:5984/"
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  (tr :: Transactie) <- createTransactie usr
  state <- new $ newPerspectivesState {userName: usr, couchdbPassword: pwd, couchdbBaseURL: url} tr av
  void $ forkAff $ runPerspectivesWithState f state
  void $ forkAff $ runPerspectivesWithState setupTcpApi state
  void $ forkAff $ forever do
    delay (Milliseconds 1000.0)
    -- liftEffect $ log "propagating"
    runPerspectivesWithState propagate state
  where
    f = do
      void $ setupUser
      addComputedTripleGetters
      setupApi

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Perspectives-core has started!"
