module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, head, unsnoc)
import Data.Foldable (foldM)
import Data.Maybe (Maybe, fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, MonadPerspectivesQueryCompiler, ObjectsGetter, TypedTripleGetter, getQueryStepDomain, runMonadPerspectivesQueryCompiler, withQueryCompilerEnvironment)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolName)
import Perspectives.Identifiers (LocalName, deconstructLocalNameFromDomeinURI)
import Perspectives.Property (getContextType, getContextTypeF, getExternalProperty, getInternalProperty, getRol, getRolByLocalName)
import Perspectives.PropertyComposition ((>->), compose)
import Perspectives.QueryCombinators (closure, closure', concat, constant, contains, filter, lastElement, notEmpty, ref, rolesOf, saveVar, toBoolean, var)
import Perspectives.SystemQueries (contextType, identity)
import Perspectives.TripleGetter (constructExternalPropertyGetter, constructExternalPropertyLookup, constructInternalPropertyGetter, constructInternalPropertyLookup, constructInverseRolGetter, constructRolGetter, constructRolLookup, constructRolPropertyGetter, constructRolPropertyLookup)
import Perspectives.Utilities (onNothing, onNothing')
import Prelude (bind, pure, ($), (<$>), (<*>), (<<<), (<>), (>=>), (>>=))

-- | From a qualified name for a Rol and its context, construct a function that computes the instances of that Rol for a given context.
-- | The Rol may be defined as computed.
rolQuery  :: forall e.
  RolName ->
  ContextID ->
  MonadPerspectivesQuery (AjaxAvarCache e) (TypedTripleGetter e)
rolQuery rn cid = do
  -- Is the type of rolType rn or one of its ancestors q:Query?
  (isAQuery :: Boolean) <- (toBoolean (contains "model:QueryAst$Query" (closure contextType)) rn)
  if isAQuery
    -- TODO: memorize!
    then do
      tg <- runMonadPerspectivesQueryCompiler cid (constructQueryFunction rn)
      -- the identity TypedTripleGetter constructs a triple <subject identity subject> that is saved
      -- and can be found with TripleRef <subject> "model:Perspectives$identity". This TripleRef is stored in
      -- the query variable "#context".
      -- pure $ (var "#context" identity) >-> tg
      pure $ (saveVar "#context" (identity >-> tg))
    else do
      domain <- lift $ getContextTypeF rn
      pure $ constructRolLookup (localName rn)

  where
    localName :: RolName -> LocalName
    localName qn = unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI qn)

-- | From a qualified name for a Property, construct a function that computes the values of that Property for a given rol.
-- | The Property may be defined as computed.
propertyQuery  :: forall e.
  PropertyName ->
  ContextID ->
  MonadPerspectivesQuery (AjaxAvarCache e) (TypedTripleGetter e)
propertyQuery pn cid = do
  -- Is the type of propertyType or one of its ancestors q:Query?
  (isAQuery :: Boolean) <- (toBoolean (contains "model:QueryAst$Query" (closure contextType)) pn)
  if isAQuery
    then do
      tg <- runMonadPerspectivesQueryCompiler pn (constructQueryFunction pn)
      pure $ (saveVar "#rol" (identity >-> tg))
    else do
      domain <- lift $ getContextTypeF pn
      pure $ constructRolPropertyLookup (localName pn)

  where
    localName :: RolName -> LocalName
    localName qn = unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI qn)

-- | From the id of a context that is a description of a Query, construct a function that computes the value of that
-- | query from the id of an entity.
-- TODO: voeg state toe waarin bijgehouden wordt welke variabelen al gedefinieerd zijn, zodat je kunt stoppen als vooruit verwezen wordt. Houdt daar ook het domein van de querystap bij.
constructQueryFunction :: forall e.
  ContextID ->
  MonadPerspectivesQueryCompiler (AjaxAvarCache e) (TypedTripleGetter e)
constructQueryFunction typeDescriptionID = do
  queryStepType <- lift $ lift $ onNothing (errorMessage "no type found" "")
    (firstOnly getContextType typeDescriptionID)
  domain <- getQueryStepDomain
  case queryStepType of
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
    "model:QueryAst$constructRolLookup" ->
      applyPropertyConstructor constructRolLookup queryStepType
    "model:QueryAst$constructRolGetter" -> do
      rolName <- lift $ lift $ onNothing (errorMessage "no rolName" queryStepType) (firstOnly (getRol "model:QueryAst$constructRolGetter$rol") typeDescriptionID)
      pure $ constructRolGetter rolName
    "model:QueryAst$constructInverseRolGetter" -> do
      rolName <- lift $ lift $ onNothing (errorMessage "no rolName" queryStepType) (firstOnly (getRol "model:QueryAst$constructRolGetter$rol") typeDescriptionID)
      pure $ constructInverseRolGetter rolName
    "model:QueryAst$rolesOf" ->
      lift $ lift $ rolesOf <$> (onNothing (errorMessage "no context" queryStepType) (firstOnly (getRol "model:QueryAst$rolesOf$context") queryStepType))
    "model:QueryAst$notEmpty" -> applyUnaryCombinator notEmpty queryStepType
    "model:QueryAst$closure" -> applyUnaryCombinator closure queryStepType
    "model:QueryAst$closure'" -> applyUnaryCombinator closure' queryStepType
    "model:QueryAst$lastElement'" -> applyUnaryCombinator lastElement queryStepType
    "model:QueryAst$compose" -> applyBinaryCombinator compose queryStepType
    "model:QueryAst$concat" -> do
      (operandIds :: Array ID) <- lift $ lift $ getRolByLocalName "operand" typeDescriptionID
      (operands :: Array (TypedTripleGetter e)) <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      lift $ lift $ foldM concat last init
    "model:QueryAst$filter" -> do
      criteriumId <- lift $ lift $ onNothing (errorMessage "no criterium" queryStepType)
        (firstOnly (getRolByLocalName "criterium") typeDescriptionID)
      candidateId <- lift $ lift $ onNothing (errorMessage "no candidates" queryStepType)
        (firstOnly (getRolByLocalName "criterium") typeDescriptionID)
      filter <$> (constructQueryFunction criteriumId) <*> withQueryCompilerEnvironment (constructQueryFunction candidateId)
    "model:QueryAst$Constant" -> do
      lift $ lift $ constant <$> onNothing (errorMessage "no constant value provided" queryStepType) (firstOnly (getExternalProperty "model:QueryAst$Constant$value") typeDescriptionID)
    "model:QueryAst$Variable" -> do
      variableName <- lift $ lift $ onNothing (errorMessage "no variable name found" queryStepType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      pure $ ref variableName
    "model:QueryAst$setVariable" -> do
      variableName <- lift $ lift $ onNothing (errorMessage "no variable name found" queryStepType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      valueDescriptionID <- lift $ lift $ onNothing (errorMessage "no value found" queryStepType)
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery <- constructQueryFunction valueDescriptionID
      pure $ var variableName valueQuery

    -- Any other argument will be passed as is, thus implementing that we can create arbitrary contexts.
    _ -> pure $ constant typeDescriptionID

  where
    applyPropertyConstructor :: (PropertyName -> TypedTripleGetter e)
      -> ID
      -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (TypedTripleGetter e)
    applyPropertyConstructor f queryStepType = do
      propertyId <- lift $ lift $ onNothing (errorMessage "no property found" queryStepType)
        (firstOnly (getRolByLocalName "property") typeDescriptionID)
      pure $ f propertyId

    applyUnaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e )
      -> ID
      -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (TypedTripleGetter e)
    applyUnaryCombinator c queryStepType = do
      queryId <- lift $ lift $ onNothing (errorMessage "no query found" queryStepType)
        (firstOnly (getRolByLocalName "query") typeDescriptionID)
      constructQueryFunction queryId >>= pure <<< c

    applyBinaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e -> TypedTripleGetter e)
      -> ID
      -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (TypedTripleGetter e)
    applyBinaryCombinator c queryStepType = do
      (operandIds :: Array ID) <- lift $ lift $ getRolByLocalName "operand" typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      pure $ foldl c last init

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructEffectStatement: " <> s <> " for: " <> t <> " " <> typeDescriptionID)

    firstOnly :: ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
    firstOnly g = g >=> (pure <<< head)
