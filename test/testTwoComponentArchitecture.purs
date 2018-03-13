module Test.TwoComponentArchitecture where

import Control.Promise as Promise
import Control.Monad.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Compat (EffFn1, EffFn2, EffFn3, EffFnAff(..), fromEffFnAff, mkEffFn1, mkEffFn2, runEffFn3)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Promise (Promise)
import Prelude (Unit, bind, discard, pure, unit, ($), (<>), (<<<))

type TestEffects e = (avar :: AVAR, console :: CONSOLE | e)

test :: forall e. Aff (TestEffects e) Unit
test = do
  channel <- makeEmptyVar
  _ <- forkAff $ perspectives channel
  _ <- forkAff $ gui channel
  pure unit

perspectives :: forall e. AVar String -> Aff (TestEffects e) Unit
perspectives var = do
  value <- takeVar var
  log $ "Got a value: " <> value
  putVar "Hello to you" var

-- De gui geïmporteerd uit javascript.
gui :: forall e.
  AVar String -> Aff (TestEffects e) Unit
gui = fromEffFnAff <<< (_gui setter getter)
  where
    setter :: String -> AVar String -> Eff (avar :: AVAR | e) Unit
    setter val var = launchAff_ $ putVar val var

    getter :: AVar String -> Eff (avar :: AVAR | e) (Promise.Promise String)
    getter = Promise.fromAff <<< takeVar

foreign import _gui :: forall e.
  (String -> AVar String -> Eff (avar :: AVAR | e) Unit)
  -> (AVar String -> Eff (avar :: AVAR | e) (Promise.Promise String))
  -> AVar String
  -> EffFnAff (TestEffects e) Unit

-- gui :: forall e.
--   AVar String -> Aff (TestEffects e) Unit
-- gui = liftEff <<< (f setter getter)
--   where
--     setter :: String -> AVar String -> Eff (avar :: AVAR | e) Unit
--     setter val var = launchAff_ $ putVar val var
--
--     getter :: AVar String -> Eff (avar :: AVAR | e) (Promise.Promise String)
--     getter = Promise.fromAff <<< takeVar
--
--     f s g av = runEffFn3 _gui (mkEffFn2 s) (mkEffFn1 g) av
--
-- foreign import _gui :: forall e.
--   EffFn3 (TestEffects e)
--   (EffFn2 (avar :: AVAR | e) String (AVar String) Unit)
--   (EffFn1 (avar :: AVAR | e) (AVar String) (Promise.Promise String))
--   (AVar String)
--   Unit

-- De orginele gui in purescript.
gui' :: forall e.
  (String -> Eff (TestEffects e) Unit)
  -> Aff (TestEffects e) Unit
gui' setter = do
  delay (Milliseconds 100.0)
  liftEff $ setter "hello"
