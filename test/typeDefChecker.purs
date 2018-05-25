module Test.TypeDefChecker where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Perpectives.TypeChecker (checkRolForQualifiedProperty, contextHasType, isOrHasAspect)
import Perspectives.ContextRolAccessors (firstOnly)
import Perspectives.CoreTypes (MonadPerspectives, runMonadPerspectivesQueryCompiler, tripleObjects)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI, deconstructNamespace)
import Perspectives.ModelBasedTripleGetters (aspectDef, aspectDefClosure, boundContexts, buitenRolBeschrijving, contextDef, ownExternePropertyDef, ownInternePropertyDef, ownPropertyDef, ownRolDef, propertyDef, propertyIsVerplicht, rolDef)
import Perspectives.ObjectGetterConstructors (getGebondenAls, getRolByLocalName)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.QueryCombinators (contains, toBoolean)
import Perspectives.QueryFunctionDescriptionCompiler (compileElementaryQueryStep)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery, (##))
import Perspectives.Syntax (PerspectContext(..))
import Perspectives.SystemObjectGetters (getBuitenRol, getRolBinding, getRolContext)
import Perspectives.TypeDefChecker (checkContext, checkModel, getPropertyFunction)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  -- messages1 <- checkContext "model:Test$rolZonderMogelijkeBinding"
  -- lift $ for_ messages1 logShow

  -- let pn = "model:QueryAst$PropertyGetter$buitenRolBeschrijving$functionName"
  -- let rn =
  -- r <- runMonadPerspectivesQueryCompiler rn (compileElementaryQueryStep (QualifiedProperty pn) (pn <> "_getter"))
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
  lift $ for_ messages15
    \m -> do
      logShow m
      logShow "------"

  -- b <- "model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht" ## propertyIsVerplicht
  -- lift $ logShow b

  -- b <- contextHasType "model:Perspectives$Property" "model:Perspectives$Property"
  -- lift $ logShow b

  -- b <- "model:QueryAst$label" `contextHasType` "model:Perspectives$Context"
  -- lift $ logShow b

  -- as <- "model:QueryAst$SingularPropertyFunction" ## aspectDefClosure
  -- lift $ logShow as

  -- as <- "model:QueryAst$contextType" ## aspectDef
  -- lift $ logShow as

   -- bc <- "model:Perspectives" ## boundContexts
   -- lift $ logShow bc
   --
   -- rt <- getRolType

  -- b <- "model:Perspectives$BuitenRol" `isOrHasAspect` "model:Perspectives$Rol"
  -- lift $ logShow b

  -- context "model:Perspectives$Context$externalProperty_buitenRol"

  -- pts <- "model:Perspectives$Rol" ## ownExternePropertyDef
  -- lift $ logShow pts

  -- let pn = "model:Perspectives$Rol$buitenRol$isVerplicht"
  -- let rn = "model:Perspectives$Rol$buitenRol"
  --
  -- r <- runMonadPerspectivesQueryCompiler rn (compileElementaryQueryStep (QualifiedExternalProperty pn) (pn <> "_getter"))
  -- lift $ logShow r
  --
  -- x <- firstOnly (getRolByLocalName "property" /-/ getRolBinding /-/ getRolContext) "model:Perspectives$View$propertyReferentie$volgNummer_getter"
  -- lift $ logShow x
  --
  -- _ <- getPropertyFunction pn rn
  -- pure unit

  -- isExternal <- (getBuitenRol /-/ getGebondenAls "model:Perspectives$Context$externalProperty") pn
  -- lift $ logShow $ head isExternal

  -- (y :: Maybe PerspectContext) <- getPerspectEntiteit "model:Perspectives$View$propertyReferentie$volgNummer_getter"
  -- lift $ logShow y


  -- b <- checkRolForQualifiedProperty pn rn
  -- lift $ logShow b

  -- b <- runMonadPerspectivesQuery rn (toBoolean (contains pn propertyDef))
  -- lift $ logShow b

  -- b <- rn ## propertyDef
  -- lift $ logShow b
  --
  -- props <- rn ## ownPropertyDef
  -- lift $ logShow props

  -- b <- rn `isOrHasAspect` "model:Perspectives$Rol"
  -- lift $ logShow b
  -- -- true

  -- ln <- pure $ deconstructLocalNameFromDomeinURI pn
  -- lift $ logShow ln
  --  (Just "isVerplicht")

  -- ln <- pure $ deconstructNamespace pn
  -- lift $ logShow ln
  -- --  (Just "model:Perspectives$Rol")

  -- (lift $ logShow ["noot"]) *> empty <|> (lift $ logShow ["aap"])

  -- t <- "model:Perspectives$Rol" ## contextDef
  -- lift $ logShow (tripleObjects t)

  -- t <- getRolUsingAspects "model:Perspectives$Rol$mogelijkeBinding" "model:Perspectives$SingularFunction$range"
  -- lift $ logShow t

  -- t2 <- getRol "model:Perspectives$Rol$aspectRol" "model:Perspectives$SingularFunction$range"
  -- lift $ logShow t2

  -- ismandatory <- runMonadPerspectivesQuery "model:Perspectives$Rol$isFunctioneel" (toBoolean propertyIsVerplicht)
  -- lift $ logShow ("isFunctioneel: " <> show ismandatory)

  -- t <- "model:Perspectives$Property$range" ## contextType >-> contextRolTypes
  -- lift $ logShow (tripleObjects t)

  -- t <- "model:Perspectives$Property$range" ## contextType
  -- lift $ logShow (tripleObjects t)
