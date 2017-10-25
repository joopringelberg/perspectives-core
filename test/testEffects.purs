module Test.TestEffects where

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Show (class Show, show)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Property (PropDefsEffects)
import Prelude (Unit, (<>), ($))

type CancelerEffects e = (PropDefsEffects (console :: CONSOLE, gm :: GLOBALMAP | e))
type TestEffects e = CancelerEffects (exception :: EXCEPTION | e)

handleError :: forall e. (Error -> Eff (console :: CONSOLE | e) Unit)
handleError e = log $ "An error caught in test: " <> (show e)

handleSuccess :: forall a e. Show a => (a -> Eff (console :: CONSOLE | e) Unit)
handleSuccess a = log $ "Success in test: " <> (show a)
