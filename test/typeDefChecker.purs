module Test.TypeDefChecker where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perpectives.TypeChecker (rolIsInstanceOfType)
import Perspectives.CoreTypes (MonadPerspectives, runMonadPerspectivesQuery, tripleObjects, (##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Property (getContextType, getRolType)
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (rolQuery)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.Syntax (PerspectContext(..))
import Perspectives.SystemQueries (contextRolTypes, propertyIsVerplicht)
import Perspectives.TypeDefChecker (checkContext)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  -- messages <- checkContext "model:Perspectives$Property$range"
  -- lift $ logShow ("Testresult is " <> (show messages))

  -- ismandatory <- runMonadPerspectivesQuery "model:Perspectives$Rol$isFunctioneel" (toBoolean propertyIsVerplicht)
  -- lift $ logShow ("isFunctioneel: " <> show ismandatory)

  t <- "model:Perspectives$Property$range" ## contextRolTypes
  lift $ logShow (tripleObjects t)
