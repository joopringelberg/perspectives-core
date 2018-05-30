module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, unsnoc)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Perspectives.ContextRolAccessors (firstOnly)
import Perspectives.CoreTypes (MonadPerspectives, TypedTripleGetter(..), TypeID)
import Perspectives.DataTypeTripleGetters (bindingM, buitenRolM, contextM, contextTypeM, identityM, iedereRolInContextM, label, rolTypeM)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID)
import Perspectives.ObjectGetterConstructors (getExternalProperty, getInternalProperty, getRol)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.QueryCache (queryCacheInsert, queryCacheLookup)
import Perspectives.QueryCombinators (closure, closure', concat, constant, filter, ignoreCache, lastElement, notEmpty, ref, rolesOf, useCache, var)
import Perspectives.RunMonadPerspectivesQuery (runTypedTripleGetter)
import Perspectives.DataTypeObjectGetters (contextType, contextTypeF, getRolBindingDef)
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
    typeDescriptionID <- contextTypeF rn
    tg@(TypedTripleGetter n _) <- constructQueryFunction typeDescriptionID
    queryCacheInsert n $ TypedTripleGetter n (lift <<< (runTypedTripleGetter tg))
  (pure <<< id)

propertyQuery  :: forall e.
  TypeID -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
propertyQuery rn = rolQuery rn

-- | From the id of a context that is a description of a Query, construct a function that computes the value of that
-- | query from the id of an entity.
-- TODO: voeg state toe waarin bijgehouden wordt welke variabelen al gedefinieerd zijn, zodat je kunt stoppen als vooruit verwezen wordt. Houdt daar ook het domein van de querystap bij.
constructQueryFunction :: forall e.
  ContextID ->
  MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
constructQueryFunction typeDescriptionID = do
  queryStepType <- onNothing (errorMessage "no type found" "")
    (firstOnly contextType typeDescriptionID)
  case queryStepType of
    "model:QueryAst$DataTypeGetter" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (firstOnly (getExternalProperty "model:QueryAst$DataTypeGetter$buitenRolBeschrijving$functionName") typeDescriptionID)
      case functionName of
        "binding" -> pure bindingM
        "context" -> pure contextM
        "identity" -> pure identityM
        "contextType" -> pure contextTypeM
        "rolType" -> pure rolTypeM
        "buitenRol" -> pure buitenRolM
        "iedereRolInContext" -> pure iedereRolInContextM
        "label" -> pure label
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for DataTypeGetter: '" <> functionName <> "'")
    "model:QueryAst$PropertyGetter" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (firstOnly (getExternalProperty "model:QueryAst$PropertyGetter$buitenRolBeschrijving$functionName") typeDescriptionID)
      property <- onNothing
        (errorMessage "no property provided" queryStepType)
        (getBindingOfRol "model:QueryAst$PropertyGetter$property" typeDescriptionID)
      case functionName of
        "constructExternalPropertyGetter" -> pure $ constructExternalPropertyGetter property
        "constructExternalPropertyLookup" -> pure $ constructExternalPropertyLookup property
        "constructRolPropertyLookup" -> pure $ constructRolPropertyLookup property
        "constructRolPropertyGetter" -> pure $ constructRolPropertyGetter property
        "propertyQuery" -> propertyQuery property
        "constructInternalPropertyLookup" -> pure $ constructInternalPropertyLookup property
        "constructInternalPropertyGetter" -> pure $ constructInternalPropertyGetter property
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for PropertyGetter: '" <> functionName <> "'")
    "model:QueryAst$RolGetter" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (firstOnly (getExternalProperty "model:QueryAst$RolGetter$buitenRolBeschrijving$functionName") typeDescriptionID)
      rol <- onNothing
        (errorMessage "no rol provided" queryStepType)
        (getBindingOfRol "model:QueryAst$RolGetter$rol" typeDescriptionID)
      case functionName of
        "constructRolGetter" -> pure $ constructRolGetter rol
        "rolQuery" -> rolQuery rol
        "constructRolLookup" -> pure $ constructRolLookup rol
        "constructInverseRolGetter" -> pure $ constructInverseRolGetter rol
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for RolGetter: '" <> functionName <> "'")
    "model:QueryAst$rolesOf" ->
      rolesOf <$> (onNothing
        (errorMessage "no context" queryStepType)
        (getBindingOfRol "model:QueryAst$rolesOf$context" queryStepType))
    "model:QueryAst$UnaryCombinator" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (firstOnly (getExternalProperty "model:QueryAst$UnaryCombinator$buitenRolBeschrijving$functionName") typeDescriptionID)
      case functionName of
        "laatste" -> applyUnaryCombinator lastElement queryStepType
        "notEmpty" -> applyUnaryCombinator notEmpty queryStepType
        "closure" -> applyUnaryCombinator closure queryStepType
        "closure'" -> applyUnaryCombinator closure' queryStepType
        "useCache" -> applyUnaryCombinator useCache queryStepType
        "ignoreCache" -> applyUnaryCombinator ignoreCache queryStepType
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for UnaryCombinator: '" <> functionName <> "'")
    "model:QueryAst$nAryCombinator" -> do
      functionName <- onNothing (errorMessage "no function name provided" queryStepType) (firstOnly (getExternalProperty "model:QueryAst$UnaryCombinator$buitenRolBeschrijving$functionName") typeDescriptionID)
      case functionName of
        "compose" -> applyBinaryCombinator (>->) queryStepType
        "concat" -> applyBinaryCombinator concat queryStepType
        otherwise -> throwError (error $ "constructQueryFunction: unknown function for nAryCombinator: '" <> functionName <> "'")
    "model:QueryAst$filter" -> do
      criteriumId <- onNothing (errorMessage "no criterium" queryStepType)
        (getBindingOfRol "model:QueryAST$filter$criterium" typeDescriptionID)
      candidateId <- onNothing (errorMessage "no candidates" queryStepType)
        (getBindingOfRol "model:QueryAST$filter$candidates" typeDescriptionID)
      filter <$> (constructQueryFunction criteriumId) <*> constructQueryFunction candidateId
    "model:QueryAst$Constant" -> do
      constant <$> onNothing (errorMessage "no constant value provided" queryStepType) (firstOnly (getExternalProperty "model:QueryAst$Constant$value") typeDescriptionID)
    "model:QueryAst$Variable" -> do
      variableName <- onNothing (errorMessage "no variable name found" queryStepType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      pure $ ref variableName
    "model:QueryAst$setVariable" -> do
      variableName <- onNothing (errorMessage "no variable name found" queryStepType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      valueDescriptionID <- onNothing (errorMessage "no value found" queryStepType)
        (getBindingOfRol "model:QueryAST$setVariable$value" typeDescriptionID)
      valueQuery <- constructQueryFunction valueDescriptionID
      pure $ var variableName valueQuery

    -- Any other argument will be passed as is, thus implementing that we can create arbitrary contexts.
    _ -> throwError (error $ "constructQueryFunction: unknown type description: '" <> typeDescriptionID <> "'")

  where
    getBindingOfRol :: ID -> ID -> MonadPerspectives (AjaxAvarCache e) (Maybe ID)
    getBindingOfRol rolName = firstOnly (getRol rolName /-/ getRolBindingDef)

    applyUnaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e )
      -> ID
      -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
    applyUnaryCombinator c queryStepType = do
      query <- onNothing (errorMessage "no query provided" queryStepType) (getBindingOfRol "model:QueryAst$UnaryCombinator$query" typeDescriptionID)
      constructQueryFunction query >>= pure <<< c

    applyBinaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e -> TypedTripleGetter e)
      -> ID
      -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
    applyBinaryCombinator c queryStepType = do
      (operandIds :: Array ID) <- (getRol "model:QueryAST$nAryCombinator$operand" /-/ getRolBindingDef)  typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      pure $ foldl c last init

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructQueryFunction: " <> s <> " for: " <> t <> " " <> typeDescriptionID)
