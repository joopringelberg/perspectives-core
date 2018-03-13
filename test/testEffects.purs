module Test.TestEffects where

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Data.Either (Either(..))
import Data.Show (class Show, show)
import Perspectives.Effects (AjaxAvarCache)
import Prelude (Unit, (<>), ($))

type CancelerEffects e = (AjaxAvarCache (console :: CONSOLE, ref :: REF, now :: NOW | e))
type TestEffects e = CancelerEffects (exception :: EXCEPTION | e)

handleError :: forall e a. Show a => (Either Error a -> Eff (console :: CONSOLE | e) Unit)
handleError (Left e) = log $ "An error caught in test: " <> (show e)
handleError (Right a) = log $ "Success in test: " <> (show a)

handleSuccess :: forall a e. Show a => (a -> Eff (console :: CONSOLE | e) Unit)
handleSuccess a = log $ "Success in test: " <> (show a)
