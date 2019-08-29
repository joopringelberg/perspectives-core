module Perspectives.RunPerspectives where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber)
import Effect.Aff.AVar (AVar, new, read)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Sync.Transactie (Transactie, createTransactie)
import Prelude (bind, discard, pure, ($))

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

-- | Run an action in MonadPerspectives, given a username and password,
-- | while propagating changes in the dependency network in parallel.
runPerspectivesWithPropagation :: forall a.
  String ->
  String ->
  MonadPerspectives a ->
  Number ->
  Aff (Tuple PerspectivesState a)
runPerspectivesWithPropagation userName password mp duration = do
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  (tr :: Transactie) <- createTransactie userName
  (rf :: AVar PerspectivesState) <- new $
    newPerspectivesState
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"}
      tr
      av
  computation <- forkAff $ runPerspectivesWithState mp rf
  propagation <- forkAff $ forever do
    delay (Milliseconds 50.0)
    -- runPerspectivesWithState propagate rf
  delay (Milliseconds duration)
  do
    killFiber (error "Error on killing the computation") computation
    killFiber (error "Error on killing the propagation") propagation
  a <- joinFiber computation
  s <- read rf
  pure (Tuple s a)

runPerspectivesWithState :: forall a. MonadPerspectives a -> (AVar PerspectivesState) -> Aff a
runPerspectivesWithState = runReaderT
