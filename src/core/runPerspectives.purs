module Perspectives.RunPerspectives where

import Control.Monad.Reader (runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, new)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Sync.Transactie (Transactie, createTransactie)
import Prelude (bind, ($))

-- | Run an action in MonadPerspectives, given a username and password.
runPerspectives :: forall a. String -> String -> MonadPerspectives a
  -> Aff a
runPerspectives userName password mp = do
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  (tr :: Transactie) <- createTransactie userName
  (rf :: AVar PerspectivesState) <- new $
    newPerspectivesState
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"}
      tr
      av
  runReaderT mp rf

runPerspectivesWithState :: forall a. MonadPerspectives a -> (AVar PerspectivesState) -> Aff a
runPerspectivesWithState = runReaderT
