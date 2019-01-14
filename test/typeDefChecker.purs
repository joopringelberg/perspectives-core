module Test.TypeDefChecker where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Perpectives.TypeChecker (checkContextForQualifiedRol, checkRolForQualifiedProperty, contextHasType, importsAspect, isOrHasAspect, rolHasType)
import Perspectives.CoreTypes (MonadPerspectives, runMonadPerspectivesQueryCompiler, tripleObjects, (%%>>))
import Perspectives.DataTypeObjectGetters (binding, buitenRol, context, contextType, iedereRolInContext, typeVanIedereRolInContext)
import Perspectives.DataTypeTripleGetters (contextTypeM)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI, deconstructNamespace, guardWellFormedNess, isInNamespace)
import Perspectives.ModelBasedObjectGetters (binnenRolContextDef, contextDef, rolInContextContextDef)
import Perspectives.ModelBasedTripleGetters (aspectenDefM, aspectenDefMClosure, aspectRollenDefM, boundContextsM, buitenRolBeschrijvingM, contextDefM, ownExternePropertiesDefM, ownInternePropertiesDefM, ownPropertiesDefM, ownRollenDefM, propertiesDefM, propertyIsVerplichtM, rollenDefM, rolInContextDefM)
import Perspectives.ObjectGetterConstructors (getExternalProperty, getGebondenAls, getRol, getRolByLocalName)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.QueryCombinators (contains, toBoolean)
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.QueryFunctionDescriptionCompiler (compileElementaryQueryStep)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.ResourceRetrieval (fetchPerspectEntiteitFromCouchdb)
import Perspectives.RunMonadPerspectivesQuery (runMonadPerspectivesQuery, (##), (##>>))
import Perspectives.Syntax (PerspectContext(..))
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (constructExternalPropertyGetter, constructInverseRolGetter, constructRolGetter, constructRolPropertyGetter)
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

  -- messages14 <- checkContext "model:Test$Test11$rol"
  -- lift $ for_ messages14 logShow

  messages15 <- checkModel "model:Perspectives"
  lift $ for_ messages15
    \m -> do
      logShow m
      logShow "------"

-- betrouwbaarheid <- "model:User$MijnAangifte" ## ((constructRolGetter "model:Politie$Aangifte$Verbalisant")
-- >-> (constructExternalPropertyGetter "model:Politie$Aangifte$Verbalisant$betrouwbaarheid"))

-- De buitenRol is er wel, maar ik kan niet terugnavigeren over rolInContext. En dat klopt, want het is een binnenRolBeschrijving rol.
  -- c <- getBinnenRolContextDef "model:Perspectives$Property$binnenRolBeschrijving"
  -- lift $ logShow c

  -- c <- "model:Test$Test11$rol" ## rolInContextDef
  -- lift $ logShow c
  --
  -- a <- "model:Test$Test11$rol" ## aspectRolDef
  -- lift $ logShow a
  --
  -- x <- "model:Test$Test11" ## aspectDef >-> rolDef
  -- lift $ logShow x

  -- b <- ("model:Systeem$Systeem$gebruiker" `contextHasType` "model:QueryAst$ComputedRolGetter")
  -- lift $ logShow b
  --
  -- let cid = "model:User$MijnSysteem"
  -- let rn = "model:Systeem$Systeem$gebruiker"
  -- let ctxtType = "model:Systeem$Systeem"
  --
  -- b <- checkContextForQualifiedRol rn ctxtType
  -- lift $ logShow b
  --
  -- r <- runMonadPerspectivesQueryCompiler ctxtType (compileElementaryQueryStep (QualifiedRol rn) (rn <> "_getter"))
  -- case r of
  --   (Right typeDescriptionID) -> do
  --     (description :: PerspectContext) <- getPerspectEntiteit typeDescriptionID
  --     lift $ logShow description
  --     _ <- constructQueryFunction typeDescriptionID
  --     lift $ logShow typeDescriptionID
  --   otherwise -> lift $ logShow "Mislukt"

  -- Je kunt geen user entiteiten ophalen...
  -- t <- "model:User$MijnSysteem" %%>> contextType
  -- lift $ logShow t

  -- let rn = "model:Perspectives$Property$buitenRolBeschrijving"
  -- let pn = "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel"
  -- getter <- (getPropertyFunction pn rn)
  -- b <- "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel_buitenRol" ## getter
  -- lift $ logShow b -- []

  -- r <- runMonadPerspectivesQueryCompiler rn (compileElementaryQueryStep (QualifiedProperty pn) (pn <> "_getter"))
  -- case r of
  --   (Right typeDescriptionID) -> do
  --     fn <- (firstOnly (getExternalProperty "model:QueryAst$PropertyGetter$buitenRolBeschrijving$functionName") typeDescriptionID)
  --     lift $ logShow fn -- (Just "constructRolPropertyGetter")
  --     pn' <- (firstOnly ((getRol "model:QueryAst$PropertyGetter$property") /-/ getRolBinding /-/ getRolContext) typeDescriptionID)
  --     lift $ logShow pn' -- (Just "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel_getter$property0")
  --   otherwise -> lift $ logShow "Mislukt"

  -- b <- pure $ "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel" `isInNamespace` "model:Perspectives$Property$buitenRolBeschrijving"
  -- lift $ logShow b

  -- getter <- pure $ constructRolPropertyGetter "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel"
  -- b <- "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel_buitenRol" ## getter
  -- lift $ logShow b  -- ["true"]

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

  -- let pn = "model:Perspectives$Property$buitenRolBeschrijving$isFunctioneel"
  -- let rn = "model:QueryAst$ComputedPropertyGetter$buitenRolBeschrijving"
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

  -- (y :: PerspectContext) <- getPerspectEntiteit "model:User$MijnSysteem"
  -- lift $ logShow y


  -- namespaceOfProperty <- guardWellFormedNess deconstructNamespace pn
  -- b <- checkRolHasAspect rn namespaceOfProperty pn
  -- b <- rn `importsAspect` namespaceOfProperty
  -- aspecten <- rn ## aspectRollenDefM
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

  -- t <- "model:Perspectives$Rol" ## rolInContextDef
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
