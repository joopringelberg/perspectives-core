module Perspectives.RunPerspectives where

import Control.Monad.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, readVar)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Class (forever)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState, Transactie, createTransactie)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.TheoryChange (propagate)
import Prelude (bind, discard, pure, ($))

-- | Run an action in MonadPerspectives, given a username and password.
runPerspectives :: forall a e. String -> String -> MonadPerspectives (avar :: AVAR, now :: NOW | e) a
  -> Aff (avar :: AVAR, now :: NOW | e) a
runPerspectives userName password mp = do
  (av :: AVar String) <- makeVar "This value will be removed on first authentication!"
  (tr :: Transactie) <- createTransactie userName
  (rf :: AVar PerspectivesState) <- makeVar $
    newPerspectivesState
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"}
      tr
      av
  runReaderT mp rf

-- | Run an action in MonadPerspectives, given a username and password,
-- | while propagating changes in the dependency network in parallel.
runPerspectivesWithPropagation :: forall a e.
  String ->
  String ->
  MonadPerspectives (now :: NOW | (AjaxAvarCache e)) a ->
  Number ->
  Aff (now :: NOW | (AjaxAvarCache e)) (Tuple PerspectivesState a)
runPerspectivesWithPropagation userName password mp duration = do
  (av :: AVar String) <- makeVar "This value will be removed on first authentication!"
  (tr :: Transactie) <- createTransactie userName
  (rf :: AVar PerspectivesState) <- makeVar $
    newPerspectivesState
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"}
      tr
      av
  computation <- forkAff $ runPerspectivesWithState mp rf
  propagation <- forkAff $ forever do
    delay (Milliseconds 50.0)
    runPerspectivesWithState propagate rf
  delay (Milliseconds duration)
  do
    killFiber (error "Error on killing the computation") computation
    killFiber (error "Error on killing the propagation") propagation
  a <- joinFiber computation
  s <- readVar rf
  pure (Tuple s a)

runPerspectivesWithState :: forall e a. MonadPerspectives (avar :: AVAR | e) a -> (AVar PerspectivesState) -> Aff (avar :: AVAR | e) a
runPerspectivesWithState = runReaderT
