module Test.TypeDefChecker where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perpectives.TypeChecker (rolIsInstanceOfType)
import Perspectives.CoreTypes (MonadPerspectives, tripleObjects)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery, (##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Property (getContextType, getRolType)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (toBoolean, ref)
import Perspectives.QueryCompiler (rolQuery)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.Syntax (PerspectContext(..))
import Perspectives.SystemQueries (contextRolTypes, contextType, propertyIsVerplicht)
import Perspectives.TypeDefChecker (checkContext)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  -- messages1 <- checkContext "model:Test$rolZonderMogelijkeBinding"
  -- lift $ logShow ("Testresult is " <> (show messages1))
  --
  -- messages2 <- checkContext "model:Test$viewMetVerkeerdeBinding"
  -- lift $ logShow ("Testresult is " <> (show messages2))
  --
  -- messages3 <- checkContext "model:Test$viewMetOngedefineerdeRol"
  -- lift $ logShow ("Testresult is " <> (show messages3))
  --
  -- messages4 <- checkContext "model:Test$viewMetGedefinieerdeRol"
  -- lift $ logShow ("Testresult is " <> (show messages4))

  t <- "model:Test$ViewMetExtraRol" ## (ref "#start")
  lift $ logShow (tripleObjects t)

  -- ismandatory <- runMonadPerspectivesQuery "model:Perspectives$Rol$isFunctioneel" (toBoolean propertyIsVerplicht)
  -- lift $ logShow ("isFunctioneel: " <> show ismandatory)

  -- t <- "model:Perspectives$Property$range" ## contextType >-> contextRolTypes
  -- lift $ logShow (tripleObjects t)

  -- t <- "model:Perspectives$Property$range" ## contextType
  -- lift $ logShow (tripleObjects t)
