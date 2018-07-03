module Test.Coroutines where

import Prelude

import Control.Coroutine (Consumer, Producer, runProcess, consumer, ($$))
import Control.Coroutine.Aff (produce, produceAff)
import Control.Monad.Aff (Aff, runAff, delay)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)

p :: forall eff. Producer String (Aff (avar :: AVAR | eff)) String
p = produceAff \emit -> do
  delay (wrap 1000.0)
  emit $ Left "Working..."
  delay (wrap 1000.0)
  emit $ Left "Working..."
  delay (wrap 1000.0)
  emit $ Left "Working..."
  delay (wrap 1000.0)
  emit $ Right "Done!"

q = produce \emit -> do
  log "Working..."
  emit (Left "progress")
  log "Done!"
  emit (Right "finished")

c :: forall eff. Consumer String (Aff (console :: CONSOLE | eff)) String
c = consumer \s -> liftEff (log s) $> Nothing

test :: forall eff. Eff (console :: CONSOLE, avar :: AVAR, err :: EXCEPTION | eff) Unit
test = void $ runAff (either logShow log) $ runProcess (q $$ c)
