module Test.TypeDefChecker where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_, intercalate)
import Data.Maybe (Maybe)
import Perpectives.TypeChecker (typeIsInstanceOfType)
import Perspectives.CoreTypes (MonadPerspectives, tripleObjects)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Property (getContextType, getRolType)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (toBoolean, ref)
import Perspectives.QueryCompiler (rolQuery)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery, (##))
import Perspectives.Syntax (PerspectContext(..))
import Perspectives.SystemQueries (aspect, contextOwnRolTypes, contextRolTypes, contextType, propertyIsVerplicht, rolPropertyTypes)
import Perspectives.TypeDefChecker (checkContext, propertyIsFunctional)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  -- messages1 <- checkContext "model:Test$rolZonderMogelijkeBinding"
  -- lift $ for_ messages1 logShow
  --
  -- messages2 <- checkContext "model:Test$viewMetVerkeerdeBinding"
  -- lift $ for_ messages2 logShow
  --
  -- messages3 <- checkContext "model:Test$viewMetOngedefineerdeRol"
  -- lift $ for_ messages3 logShow
  --
  -- messages4 <- checkContext "model:Test$viewMetGedefinieerdeRol"
  -- lift $ for_ messages4 logShow
  --
  -- messages5 <- checkContext "model:Test$viewMetGedefinieerdeRol1"
  -- lift $ for_ messages5 logShow
  --
  -- messages6 <- checkContext "model:Test$rolZonderFunctioneleProperty"
  -- lift $ for_ messages6 logShow
  --
  -- messages7 <- checkContext "model:Test$rolMetTeveelPropertyWaarden"
  -- lift $ for_ messages7 logShow
  --
  -- messages8 <- checkContext "model:Test$rolMetVerkeerdePropertyWaarde"
  -- lift $ for_ messages8 logShow

  messages9 <- checkContext "model:Test$rolMetNietBestaandeProperty"
  lift $ for_ messages9 logShow

  t <- "model:Test$rolMetNietBestaandeProperty" ## rolPropertyTypes
  lift $ logShow (tripleObjects t)

  -- ismandatory <- runMonadPerspectivesQuery "model:Perspectives$Rol$isFunctioneel" (toBoolean propertyIsVerplicht)
  -- lift $ logShow ("isFunctioneel: " <> show ismandatory)

  -- t <- "model:Perspectives$Property$range" ## contextType >-> contextRolTypes
  -- lift $ logShow (tripleObjects t)

  -- t <- "model:Perspectives$Property$range" ## contextType
  -- lift $ logShow (tripleObjects t)
