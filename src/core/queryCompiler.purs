module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, unsnoc)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MonadPerspectives, TypedTripleGetter(..), TypeID)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName)
import Perspectives.Property (firstOnly, getContextType, getContextTypeF, getExternalProperty, getInternalProperty, getRol, getRolByLocalName)
import Perspectives.QueryCache (queryCacheInsert, queryCacheLookup)
import Perspectives.QueryCombinators (closure, closure', concat, constant, filter, lastElement, notEmpty, ref, rolesOf, var)
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
    typeDescriptionID <- getContextTypeF rn
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
    (firstOnly getContextType typeDescriptionID)
  case queryStepType of
    "model:QueryAst$propertyQuery" -> do
      propertyName <- onNothing (errorMessage "no propertyName" queryStepType) (firstOnly (getRol "model:QueryAst$propertyQuery$property") typeDescriptionID)
      propertyQuery propertyName
    "model:QueryAst$constructExternalPropertyGetter" ->
      applyPropertyConstructor constructExternalPropertyGetter queryStepType
    "model:QueryAst$constructExternalPropertyLookup" ->
      applyPropertyConstructor constructExternalPropertyLookup queryStepType
    "model:QueryAst$constructInternalPropertyGetter" ->
      applyPropertyConstructor constructInternalPropertyGetter queryStepType
    "model:QueryAst$constructInternalPropertyLookup" ->
      applyPropertyConstructor constructInternalPropertyLookup queryStepType
    "model:QueryAst$constructRolPropertyGetter" ->
      applyPropertyConstructor constructRolPropertyGetter queryStepType
    "model:QueryAst$constructRolPropertyLookup" ->
      applyPropertyConstructor constructRolPropertyLookup queryStepType
    "model:QueryAst$rolQuery" -> do
      rolName <- onNothing (errorMessage "no rolName" queryStepType) (firstOnly (getRol "model:QueryAst$rolQuery$rol") typeDescriptionID)
      rolQuery rolName
    "model:QueryAst$constructRolLookup" -> do
      rolName <- onNothing (errorMessage "no rolName" queryStepType) (firstOnly (getRol "model:QueryAst$constructRolGetter$rol") typeDescriptionID)
      pure $ constructRolLookup rolName
    "model:QueryAst$constructRolGetter" -> do
      rolName <- onNothing (errorMessage "no rolName" queryStepType) (firstOnly (getRol "model:QueryAst$constructRolGetter$rol") typeDescriptionID)
      pure $ constructRolGetter rolName
    "model:QueryAst$constructInverseRolGetter" -> do
      rolName <- onNothing (errorMessage "no rolName" queryStepType) (firstOnly (getRol "model:QueryAst$constructRolGetter$rol") typeDescriptionID)
      pure $ constructInverseRolGetter rolName
    "model:QueryAst$rolesOf" ->
      rolesOf <$> (onNothing (errorMessage "no context" queryStepType) (firstOnly (getRol "model:QueryAst$rolesOf$context") queryStepType))
    "model:QueryAst$notEmpty" -> applyUnaryCombinator notEmpty queryStepType
    "model:QueryAst$closure" -> applyUnaryCombinator closure queryStepType
    "model:QueryAst$closure'" -> applyUnaryCombinator closure' queryStepType
    "model:QueryAst$lastElement'" -> applyUnaryCombinator lastElement queryStepType
    "model:QueryAst$compose" -> applyBinaryCombinator (>->) queryStepType
    "model:QueryAst$concat" -> do
      (operandIds :: Array ID) <- getRolByLocalName "operand" typeDescriptionID
      (operands :: Array (TypedTripleGetter e)) <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      pure $ foldl concat last init
    "model:QueryAst$filter" -> do
      criteriumId <- onNothing (errorMessage "no criterium" queryStepType)
        (firstOnly (getRolByLocalName "criterium") typeDescriptionID)
      candidateId <- onNothing (errorMessage "no candidates" queryStepType)
        (firstOnly (getRolByLocalName "criterium") typeDescriptionID)
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
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery <- constructQueryFunction valueDescriptionID
      pure $ var variableName valueQuery
    -- TODO: binding etc.

    -- Any other argument will be passed as is, thus implementing that we can create arbitrary contexts.
    _ -> pure $ constant typeDescriptionID

  where
    applyPropertyConstructor :: (PropertyName -> TypedTripleGetter e)
      -> ID
      -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
    applyPropertyConstructor f queryStepType = do
      propertyId <- onNothing (errorMessage "no property found" queryStepType)
        (firstOnly (getRolByLocalName "property") typeDescriptionID)
      pure $ f propertyId

    applyUnaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e )
      -> ID
      -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
    applyUnaryCombinator c queryStepType = do
      queryId <- onNothing (errorMessage "no query found" queryStepType)
        (firstOnly (getRolByLocalName "query") typeDescriptionID)
      constructQueryFunction queryId >>= pure <<< c

    applyBinaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e -> TypedTripleGetter e)
      -> ID
      -> MonadPerspectives (AjaxAvarCache e) (TypedTripleGetter e)
    applyBinaryCombinator c queryStepType = do
      (operandIds :: Array ID) <- getRolByLocalName "operand" typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      pure $ foldl c last init

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructEffectStatement: " <> s <> " for: " <> t <> " " <> typeDescriptionID)
