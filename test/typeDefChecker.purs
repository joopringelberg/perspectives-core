module Test.TypeDefChecker where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Trans.Class (lift)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.TypeDefChecker (checkContext)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  messages <- checkContext "model:Perspectives$Property"
  lift $ logShow "Test has run."
