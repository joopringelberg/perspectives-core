module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, unsnoc)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MonadPerspectives, TypeID, TypedTripleGetter(..), ObjectsGetter, (%%>), (%%>>))
import Perspectives.DataTypeObjectGetters (contextType, rolBindingDef)
import Perspectives.DataTypeTripleGetters (bindingM, buitenRolM, contextM, contextTypeM, identityM, iedereRolInContextM, labelM, rolTypeM)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID)
import Perspectives.ObjectGetterConstructors (getExternalProperty, getInternalProperty, getRol)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.QueryCache (queryCacheInsert, queryCacheLookup)
import Perspectives.QueryCombinators (closure, closure', concat, conj, constant, disj, equal, filter, ignoreCache, implies, lastElement, notEmpty, ref, rolesOf, useCache, var)
import Perspectives.RunMonadPerspectivesQuery (runTypedTripleGetter)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (constructExternalPropertyGetter, constructExternalPropertyLookup, constructInternalPropertyGetter, constructInternalPropertyLookup, constructInverseRolGetter, constructRolGetter, constructRolLookup, constructRolPropertyGetter, constructRolPropertyLookup)
import Perspectives.Utilities (ifNothing, onNothing, onNothing')
import Prelude (bind, id, pure, ($), (<$>), (<*>), (<<<), (<>), (>>=))

-- | From a qualified name for a computed Rol or Property, construct a function that computes the instances of that Rol or the values of that Property for a given context.
-- | This function caches its results in the QueryCache.
rolQuery  :: forall e.
  TypeID -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
rolQuery rn = ifNothing (queryCacheLookup rn)
  do
    -- We must run the resulting function in its own State.
    -- TODO. Dit ziet er beter uit als we ObjectsGetters gebruiken.
    typeDescriptionID <- rn %%>> contextType
    tg@(TypedTripleGetter n _) <- constructQueryFunction typeDescriptionID
    queryCacheInsert n $ TypedTripleGetter n (lift <<< (runTypedTripleGetter tg))
  (pure <<< id)

propertyQuery  :: forall e.
  TypeID -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
propertyQuery rn = rolQuery rn

-- | An alias that generalises over roles and properties.
memberQuery  :: forall e.
  TypeID -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
memberQuery = rolQuery

-- | From the id of a context that is a description of a Query, construct a function that computes the value of that
-- | query from the id of an entity.
-- TODO: voeg state toe waarin bijgehouden wordt welke variabelen al gedefinieerd zijn, zodat je kunt stoppen als vooruit verwezen wordt. Houdt daar ook het domein van de querystap bij.
constructQueryFunction :: forall e.
  ContextID ->
  MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
constructQueryFunction typeDescriptionID = do
  queryStepType <- onNothing (errorMessage "no type found" "")
    (typeDescriptionID %%> contextType)
  case queryStepType of
    "model:QueryAst$DataTypeGetter" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID %%> (getExternalProperty "model:QueryAst$DataTypeGetter$buitenRolBeschrijving$functionName") )
      case functionName of
        "binding" -> pure bindingM
        "context" -> pure contextM
        "identity" -> pure identityM
        "contextType" -> pure contextTypeM
        "rolType" -> pure rolTypeM
        "buitenRol" -> pure buitenRolM
        "iedereRolInContext" -> pure iedereRolInContextM
        "label" -> pure labelM
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for DataTypeGetter: '" <> functionName <> "'")
    "model:QueryAst$PropertyGetter" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID %%> (getExternalProperty "model:QueryAst$PropertyGetter$buitenRolBeschrijving$functionName") )
      property <- onNothing
        (errorMessage "no property provided" queryStepType)
        (typeDescriptionID %%> getBindingOfRol "model:QueryAst$PropertyGetter$property")
      case functionName of
        "constructExternalPropertyGetter" -> pure $ constructExternalPropertyGetter property
        "constructExternalPropertyLookup" -> pure $ constructExternalPropertyLookup property
        "constructRolPropertyLookup" -> pure $ constructRolPropertyLookup property
        "constructRolPropertyGetter" -> pure $ constructRolPropertyGetter property
        "propertyQuery" -> propertyQuery property
        "constructInternalPropertyLookup" -> pure $ constructInternalPropertyLookup property
        "constructInternalPropertyGetter" -> pure $ constructInternalPropertyGetter property
        "computedPropertyGetter" -> do
          computingFunctionName <- onNothing (errorMessage "no computing function name provided" queryStepType) (property %%> (getExternalProperty "model:QueryAst$ComputedPropertyGetter$buitenRolBeschrijving$functionName"))
          mcomputingFunction <- queryCacheLookup computingFunctionName
          case mcomputingFunction of
            (Just computingFunction) -> pure computingFunction
            otherwise -> throwError (error $ "constructQueryFunction: unknown computing function for computedPropertyGetter: '" <> computingFunctionName <> "'")
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for PropertyGetter: '" <> functionName <> "'")
    "model:QueryAst$RolGetter" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID %%> (getExternalProperty "model:QueryAst$RolGetter$buitenRolBeschrijving$functionName"))
      rol <- onNothing
        (errorMessage "no rol provided" queryStepType)
        (typeDescriptionID %%> getBindingOfRol "model:QueryAst$RolGetter$rol")
      case functionName of
        "constructRolGetter" -> pure $ constructRolGetter rol
        "rolQuery" -> rolQuery rol
        "constructRolLookup" -> pure $ constructRolLookup rol
        "constructInverseRolGetter" -> pure $ constructInverseRolGetter rol
        "computedRolGetter" -> do
          computingFunctionName <- onNothing (errorMessage "no computing function name provided" queryStepType) (rol %%> (getExternalProperty "model:QueryAst$ComputedRolGetter$buitenRolBeschrijving$functionName"))
          mcomputingFunction <- queryCacheLookup computingFunctionName
          case mcomputingFunction of
            (Just computingFunction) -> pure computingFunction
            otherwise -> throwError (error $ "constructQueryFunction: unknown computing function for computedRolGetter: '" <> computingFunctionName <> "'")
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for RolGetter: '" <> functionName <> "'")

    "model:QueryAst$rolesOf" ->
      rolesOf <$> (onNothing
        (errorMessage "no context" queryStepType)
        (queryStepType %%> getBindingOfRol "model:QueryAst$rolesOf$context"))
    "model:QueryAst$UnaryCombinator" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID %%> (getExternalProperty "model:QueryAst$UnaryCombinator$buitenRolBeschrijving$functionName") )
      case functionName of
        "laatste" -> applyUnaryCombinator lastElement queryStepType
        "notEmpty" -> applyUnaryCombinator notEmpty queryStepType
        "closure" -> applyUnaryCombinator closure queryStepType
        "closure'" -> applyUnaryCombinator closure' queryStepType
        "useCache" -> applyUnaryCombinator useCache queryStepType
        "ignoreCache" -> applyUnaryCombinator ignoreCache queryStepType
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for UnaryCombinator: '" <> functionName <> "'")
    "model:QueryAst$nAryCombinator" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (typeDescriptionID %%> (getExternalProperty "model:QueryAst$UnaryCombinator$buitenRolBeschrijving$functionName") )
      case functionName of
        "compose" -> applyBinaryCombinator (>->) queryStepType
        "concat" -> applyBinaryCombinator concat queryStepType
        "and" -> applyBinaryCombinator conj queryStepType
        "or" -> applyBinaryCombinator disj queryStepType
        "implies" -> applyBinaryCombinator implies queryStepType
        "equal" -> applyBinaryCombinator equal queryStepType
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for nAryCombinator: '" <> functionName <> "'")
    "model:QueryAst$filter" -> do
      criteriumId <- onNothing (errorMessage "no criterium" queryStepType)
        (typeDescriptionID %%> getBindingOfRol "model:QueryAST$filter$criterium")
      candidateId <- onNothing (errorMessage "no candidates" queryStepType)
        (typeDescriptionID %%> getBindingOfRol "model:QueryAST$filter$candidates")
      filter <$> (constructQueryFunction criteriumId) <*> constructQueryFunction candidateId
    "model:QueryAst$Constant" -> do
      constant <$> onNothing (errorMessage "no constant value provided" queryStepType) (typeDescriptionID %%> (getExternalProperty "model:QueryAst$Constant$value") )
    "model:QueryAst$Variable" -> do
      variableName <- onNothing (errorMessage "no variable name found" queryStepType)
        (typeDescriptionID %%> (getInternalProperty "model:QueryAst$Variable$name"))
      pure $ ref variableName
    "model:QueryAst$setVariable" -> do
      variableName <- onNothing (errorMessage "no variable name found" queryStepType)
        (typeDescriptionID %%> (getInternalProperty "model:QueryAst$Variable$name"))
      valueDescriptionID <- onNothing (errorMessage "no value found" queryStepType)
        (typeDescriptionID %%> getBindingOfRol "model:QueryAST$setVariable$value" /-/ rolBindingDef)
      valueQuery <- constructQueryFunction valueDescriptionID
      pure $ var variableName valueQuery

    _ -> throwError (error $ "constructQueryFunction: unknown type description: '" <> typeDescriptionID <> "'")

  where
    getBindingOfRol :: ID -> ObjectsGetter e
    getBindingOfRol rolName = getRol rolName /-/ rolBindingDef

    applyUnaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e )
      -> ID
      -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
    applyUnaryCombinator c queryStepType = do
      query <- onNothing (errorMessage "no query provided" queryStepType) (typeDescriptionID %%> getBindingOfRol "model:QueryAst$UnaryCombinator$query")
      constructQueryFunction query >>= pure <<< c

    applyBinaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e -> TypedTripleGetter e)
      -> ID
      -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
    applyBinaryCombinator c queryStepType = do
      (operandIds :: Array ID) <- (getRol "model:QueryAST$nAryCombinator$operand" /-/ rolBindingDef)  typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      pure $ foldl c last init

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructQueryFunction: " <> s <> " for: " <> t <> " " <> typeDescriptionID)
