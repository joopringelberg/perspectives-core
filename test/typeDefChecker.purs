module Test.TypeDefChecker where

import Prelude

import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Perspectives.CoreTypes (MonadPerspectives, tripleObjects)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.SystemQueries (contextTypeOfRolType)
import Perspectives.TypeDefChecker (checkModel)

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
  --
  -- messages9 <- checkContext "model:Test$rolMetNietBestaandeExterneProperty"
  -- lift $ for_ messages9 logShow
  --
  -- messages10 <- checkContext "model:Test$rolMetNietBestaandeInterneProperty"
  -- lift $ for_ messages10 logShow
  --
  -- messages11 <- checkContext "model:Test$nietBestaandeRolProperty"
  -- lift $ for_ messages11 logShow
  --
  -- messages12 <- checkContext "model:Test$ongedefineerdeAspectRol"
  -- lift $ for_ messages12 logShow
  --
  -- messages13 <- checkContext "model:Test$Sub"
  -- lift $ for_ messages13 logShow

  -- messages14 <- checkContext "model:Test$ViewMetAspectRol"
  -- lift $ for_ messages14 logShow

  messages15 <- checkModel "model:Perspectives"
  lift $ for_ messages15 logShow

  -- t <- "model:Perspectives$Rol" ## contextTypeOfRolType
  -- lift $ logShow (tripleObjects t)

  -- ismandatory <- runMonadPerspectivesQuery "model:Perspectives$Rol$isFunctioneel" (toBoolean propertyIsVerplicht)
  -- lift $ logShow ("isFunctioneel: " <> show ismandatory)

  -- t <- "model:Perspectives$Property$range" ## contextType >-> contextRolTypes
  -- lift $ logShow (tripleObjects t)

  -- t <- "model:Perspectives$Property$range" ## contextType
  -- lift $ logShow (tripleObjects t)
