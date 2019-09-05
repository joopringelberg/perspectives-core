module Main where
import Effect.Aff (Error, forkAff, runAff)
import Effect.Aff.AVar (AVar, new)
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Perspectives.Api (setupApi, setupTcpApi)
-- import Perspectives.ComputedTripleGetters (addComputedTripleGetters)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupUser (setupUser)
import Prelude (Unit, bind, pure, ($), (<>), show, void, discard)

main :: Effect Unit
main = void $ runAff handleError do
  -- TODO: retrieve the couchdb credentials from the trusted cluster or through the user interface.
  usr <- pure "cor"
  pwd <- pure "geheim"
  url <- pure "http://127.0.0.1:5984/"
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  state <- new $ newPerspectivesState {userName: usr, couchdbPassword: pwd, couchdbBaseURL: url} av
  void $ forkAff $ runPerspectivesWithState f state
  void $ forkAff $ runPerspectivesWithState setupTcpApi state
  where
    f = do
      void $ setupUser
      -- addComputedTripleGetters
      setupApi

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Perspectives-core has started!"
