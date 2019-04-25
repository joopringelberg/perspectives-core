module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Data.Array (foldl, unsnoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (type (**>), type (~~>), MP, MonadPerspectives, runMonadPerspectivesQueryCompiler, (##>))
import Perspectives.DataTypeObjectGetters (contextType, rolBindingDef, genericContext)
import Perspectives.DataTypeTripleGetters (buitenRol, contextType, genericBinding, identity, iedereRolInContext, label, genericContext, genericRolType) as DTG
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (deconstructNamespace)
import Perspectives.ObjectGetterConstructors (searchContextRol, searchExternalProperty, getInternalProperty) as OGC
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (AnyContext, PropertyDef(..), RolDef(..), Value, genericBinding, typeWithPerspectivesTypes)
import Perspectives.QueryAST (ElementaryQueryStep(..))
import Perspectives.QueryCache (queryCacheLookup)
import Perspectives.QueryCombinators (closure', conj, constant, disj, filter, ignoreCache, implies, lastElement, notEmpty, ref, useCache, var)
import Perspectives.QueryFunctionDescriptionCompiler (compileElementaryQueryStep)
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter, closure, concat, constructInverseRolGetter, getInternalProperty, rolesOf, searchContextRol, searchExternalProperty, searchExternalUnqualifiedProperty, searchInternalUnqualifiedProperty, searchProperty, searchUnqualifiedProperty)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.Utilities (onNothing, onNothing')
import Prelude (bind, pure, ($), (<$>), (<*>), (<<<), (<>), (>>=), map, (>=>), show)

getPropertyFunction :: forall e.
  String ->
  MonadPerspectives (AjaxAvarCache e) (StringTypedTripleGetter e)
getPropertyFunction = constructGetter QualifiedProperty

getInternalPropertyFunction :: forall e.
  String ->
  MonadPerspectives (AjaxAvarCache e) (StringTypedTripleGetter e)
getInternalPropertyFunction = constructGetter QualifiedInternalProperty

getRolFunction :: forall e.
  String ->
  MonadPerspectives (AjaxAvarCache e) (StringTypedTripleGetter e)
getRolFunction = constructGetter QualifiedRol

-- | Returns a getter, lookup function or compiled query.
constructGetter :: forall e.
  (String -> ElementaryQueryStep) ->
  String ->
  MonadPerspectives (AjaxAvarCache e) (StringTypedTripleGetter e)
constructGetter queryAstConstructor pn = do
  mrn <- pure $ deconstructNamespace pn
  case mrn of
    Nothing -> throwError (error $ "invalid name: " <> pn)
    (Just rn) -> do
      -- The QueryDescriptionCompiler checks if the Rol defines the Property.
      -- That check is of no importance here, as it leads to the same code.
      -- Hence we use the namespace of the Property as the name of the Rol.
      r <- runMonadPerspectivesQueryCompiler rn (compileElementaryQueryStep (queryAstConstructor pn) (pn <> "_getterDescription"))
      case r of
        (Left m) -> throwError $ error $ show m
        (Right descriptionId) -> constructQueryFunction descriptionId

-- | From the id of a context that is a description of a Query, construct a function that computes the value of that
-- | query from the id of an entity.
-- TODO: voeg state toe waarin bijgehouden wordt welke variabelen al gedefinieerd zijn, zodat je kunt stoppen als vooruit verwezen wordt. Houdt daar ook het domein van de querystap bij.
constructQueryFunction :: forall s o e.
  AnyContext ->
  MonadPerspectives (AjaxAvarCache e) ((String **> String) e)
constructQueryFunction typeDescriptionID = do
  queryStepType <- onNothing (errorMessage "no type found" "")
    (typeDescriptionID ##> contextType)
  case queryStepType of
    "model:QueryAst$DataTypeGetter" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID ##> (OGC.searchExternalProperty (PropertyDef "model:QueryAst$DataTypeGetter$buitenRolBeschrijving$functionName")) )
      case unwrap functionName of
        "binding" -> pure $ typeWithPerspectivesTypes DTG.genericBinding
        "context" -> pure $ typeWithPerspectivesTypes DTG.genericContext
        "identity" -> pure DTG.identity
        "contextType" -> pure DTG.contextType
        "rolType" -> pure DTG.genericRolType
        "buitenRol" -> pure $ typeWithPerspectivesTypes DTG.buitenRol
        "iedereRolInContext" -> pure DTG.iedereRolInContext
        "label" -> pure DTG.label
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for DataTypeGetter: '" <> unwrap functionName <> "'")
    "model:QueryAst$PropertyGetter" -> do
      (functionName :: Value) <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID ##> (OGC.searchExternalProperty $ PropertyDef "model:QueryAst$PropertyGetter$buitenRolBeschrijving$functionName") )
      -- 'property' is either qualified or unqualified.
      property <- onNothing
        (errorMessage "no property provided" queryStepType)
        (typeDescriptionID ##> getBindingOfRol "model:QueryAst$PropertyGetter$property")
      case unwrap functionName of
        "searchProperty" -> pure $ typeWithPerspectivesTypes ((searchProperty property))
        "getInternalPropery" -> pure $ getInternalProperty property
        "searchExternalProperty" -> pure $ typeWithPerspectivesTypes $ searchExternalProperty property
        "searchUnqualifiedProperty" -> pure $ typeWithPerspectivesTypes $ ((searchUnqualifiedProperty property))
        "searchInternalUnqualfiedProperty" -> pure $ typeWithPerspectivesTypes $ searchInternalUnqualifiedProperty property
        "searchExternalUnqualifiedProperty" -> pure $ typeWithPerspectivesTypes $ searchExternalUnqualifiedProperty property
        "propertyQuery" -> constructGetter QualifiedProperty property
        "computedPropertyGetter" -> do
          computingFunctionName <- onNothing (errorMessage "no computing function name provided" queryStepType) (property ##> (OGC.searchExternalProperty $ PropertyDef "model:QueryAst$ComputedPropertyGetter$buitenRolBeschrijving$functionName"))
          mcomputingFunction <- queryCacheLookup $ unwrap computingFunctionName
          case mcomputingFunction of
            (Just computingFunction) -> pure computingFunction
            otherwise -> throwError (error $ "constructQueryFunction: unknown computing function for computedPropertyGetter: '" <> unwrap computingFunctionName <> "'")
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for PropertyGetter: '" <> unwrap functionName <> "'")
    "model:QueryAst$RolGetter" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID ##> (OGC.searchExternalProperty $ PropertyDef "model:QueryAst$RolGetter$buitenRolBeschrijving$functionName"))
      rol <- onNothing
        (errorMessage "no rol provided" queryStepType)
        (typeDescriptionID ##> getBindingOfRol "model:QueryAst$RolGetter$rol")
      case unwrap functionName of
        "searchRol" -> pure $ typeWithPerspectivesTypes $ searchContextRol rol
        "rolQuery" -> constructGetter QualifiedRol rol
        "constructInverseRolGetter" -> pure $ constructInverseRolGetter rol
        "computedRolGetter" -> do
          computingFunctionName <- onNothing (errorMessage "no computing function name provided" queryStepType) (rol ##> (OGC.searchExternalProperty $ PropertyDef "model:QueryAst$ComputedRolGetter$buitenRolBeschrijving$functionName"))
          mcomputingFunction <- queryCacheLookup $ unwrap computingFunctionName
          case mcomputingFunction of
            (Just computingFunction) -> pure computingFunction
            otherwise -> throwError (error $ "constructQueryFunction: unknown computing function for computedRolGetter: '" <> unwrap computingFunctionName <> "'")
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for RolGetter: '" <> unwrap functionName <> "'")

    "model:QueryAst$rolesOf" ->
      typeWithPerspectivesTypes $ rolesOf <$> (onNothing
        (errorMessage "no context" queryStepType)
        (queryStepType ##> getBindingOfRol "model:QueryAst$rolesOf$context"))
    "model:QueryAst$UnaryCombinator" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID ##> (OGC.searchExternalProperty $ PropertyDef "model:QueryAst$UnaryCombinator$buitenRolBeschrijving$functionName") )
      case unwrap functionName of
        "laatste" -> applyUnaryCombinator lastElement queryStepType
        "notEmpty" -> applyUnaryCombinator (typeWithPerspectivesTypes notEmpty) queryStepType
        "closure" -> applyUnaryCombinator closure queryStepType
        "closure'" -> applyUnaryCombinator closure' queryStepType
        "useCache" -> applyUnaryCombinator useCache queryStepType
        "ignoreCache" -> applyUnaryCombinator ignoreCache queryStepType
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for UnaryCombinator: '" <> unwrap functionName <> "'")
    "model:QueryAst$nAryCombinator" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID ##> (OGC.searchExternalProperty $ PropertyDef "model:QueryAst$UnaryCombinator$buitenRolBeschrijving$functionName") )
      case unwrap functionName of
        "compose" -> applyBinaryCombinator (>->) queryStepType
        "concat" -> applyBinaryCombinator concat queryStepType
        "and" -> applyBinaryCombinator (typeWithPerspectivesTypes conj) queryStepType
        "or" -> applyBinaryCombinator (typeWithPerspectivesTypes disj) queryStepType
        "implies" -> applyBinaryCombinator (typeWithPerspectivesTypes implies) queryStepType
        -- "equal" -> (applyBinaryCombinator (equal :: StringTypedTripleGetter e -> StringTypedTripleGetter e -> (String **> PBool) e) queryStepType) `followedBy` unwrap
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for nAryCombinator: '" <> unwrap functionName <> "'")
    "model:QueryAst$filter" -> do
      criteriumId <- onNothing (errorMessage "no criterium" queryStepType)
        (typeDescriptionID ##> getBindingOfRol "model:QueryAST$filter$criterium")
      candidateId <- onNothing (errorMessage "no candidates" queryStepType)
        (typeDescriptionID ##> getBindingOfRol "model:QueryAST$filter$candidates")
      typeWithPerspectivesTypes filter <$> (constructQueryFunction criteriumId) <*> (constructQueryFunction candidateId)
    "model:QueryAst$Constant" -> do
      constant <$> onNothing (errorMessage "no constant value provided" queryStepType) (typeDescriptionID ##> (OGC.searchExternalProperty (PropertyDef "model:QueryAst$Constant$value") >=> pure <<< map unwrap ))
    "model:QueryAst$Variable" -> do
      variableName <- onNothing (errorMessage "no variable name found" queryStepType)
        (typeDescriptionID ##> (OGC.getInternalProperty $ PropertyDef "model:QueryAst$Variable$name"))
      pure $ ref $ unwrap variableName
    "model:QueryAst$setVariable" -> do
      variableName <- onNothing (errorMessage "no variable name found" queryStepType)
        (typeDescriptionID ##> (OGC.getInternalProperty $ PropertyDef "model:QueryAst$Variable$name"))
      valueDescriptionID <- onNothing (errorMessage "no value found" queryStepType)
        (typeDescriptionID ##> getBindingOfRol "model:QueryAST$setVariable$value" /-/ genericBinding /-/ genericContext)
      valueQuery <- constructQueryFunction valueDescriptionID
      pure $ var (unwrap variableName) valueQuery

    _ -> throwError (error $ "constructQueryFunction: unknown type description: '" <> typeDescriptionID <> "'")

  where
    getBindingOfRol :: String -> (AnyContext ~~> AnyContext) e
    getBindingOfRol rolName = OGC.searchContextRol (RolDef rolName) /-/ rolBindingDef

    applyUnaryCombinator :: forall t.
      ((String **> String) e -> (String **> String) e) ->
      ID ->
      MP e ((String **> String) e)
    applyUnaryCombinator c queryStepType = do
      query <- onNothing (errorMessage "no query provided" queryStepType) (typeDescriptionID ##> getBindingOfRol "model:QueryAst$UnaryCombinator$query")
      constructQueryFunction query >>= pure <<< c

    applyBinaryCombinator ::
      ((String **> String) e -> (String **> String) e -> (String **> String) e) ->
      ID ->
      MP e ((String **> String) e)
    applyBinaryCombinator c queryStepType = do
      (operandIds :: Array AnyContext) <- (OGC.searchContextRol (RolDef "model:QueryAST$nAryCombinator$operand") /-/ rolBindingDef) typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      pure $ foldl c last init

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructQueryFunction: " <> s <> " for: " <> t <> " " <> typeDescriptionID)
