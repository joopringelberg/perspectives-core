module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, head, unsnoc)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Perpectives.TypeChecker (isOrHasAspect)
import Perspectives.CoreTypes (MonadPerspectives, ObjectsGetter, TypedTripleGetter, MonadPerspectivesQuery)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolName)
import Perspectives.Identifiers (isInNamespace)
import Perspectives.Property (getContextType, getExternalProperty, getInternalProperty, getRol, getRolByLocalName)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCache (queryCacheLookup)
import Perspectives.QueryCombinators (closure, closure', concat, constant, filter, lastElement, notEmpty, ref, rolesOf, saveVar, var)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleGetter (constructExternalPropertyGetter, constructExternalPropertyLookup, constructInternalPropertyGetter, constructInternalPropertyLookup, constructInverseRolGetter, constructRolGetter, constructRolLookup, constructRolPropertyGetter, constructRolPropertyLookup)
import Perspectives.Utilities (ifNothing, onNothing, onNothing')
import Prelude (bind, ifM, pure, ($), (<$>), (<*>), (<<<), (<>), (>=>), (>>=), id)

-- | From a qualified name for a Rol and the context that it is defined in (hence, the domain of the rol-getter), construct a function that computes the instances of that Rol for a given context.
-- | The Rol may be defined as computed. It may be in the same namespace as the context,
-- | that is, locally defined, or it may come from some Aspect (and so be in another namespace)
rolQuery  :: forall e.
  RolName ->
  ContextID ->
  MonadPerspectivesQuery (AjaxAvarCache e) (TypedTripleGetter e)
rolQuery rn cid = ifNothing (lift $ queryCacheLookup rn)
  do
    ifM (lift $ rn `isOrHasAspect` "model:Perspectives$Function")
      do
        tg <- lift $ constructQueryFunction rn
        -- the identity TypedTripleGetter constructs a triple <subject identity subject> that is saved
        -- and can be found with TripleRef <subject> "model:Perspectives$identity". This TripleRef is stored in the query variable "#context".
        pure $ (saveVar "#context" (identity >-> tg))
      do
        if rn `isInNamespace` cid
          then pure $ constructRolGetter rn
          else pure $ constructRolLookup rn
  (pure <<< id)

-- | From a qualified name for a Property, construct a function that computes the values of that Property for a given rol.
-- | The Property may be defined as computed.
propertyQuery  :: forall e.
  PropertyName ->
  ContextID ->
  MonadPerspectivesQuery (AjaxAvarCache e) (TypedTripleGetter e)
propertyQuery pn cid = ifNothing (lift $ queryCacheLookup pn)
  do
    ifM (lift $ pn `isOrHasAspect` "model:Perspectives$Function")
      do
        tg <- lift $ constructQueryFunction pn
        -- the identity TypedTripleGetter constructs a triple <subject identity subject> that is saved
        -- and can be found with TripleRef <subject> "model:Perspectives$identity". This TripleRef is stored in the query variable "#context".
        pure $ (saveVar "#context" (identity >-> tg))
      do
        if pn `isInNamespace` cid
          then pure $ constructRolPropertyGetter pn
          else pure $ constructRolPropertyLookup pn
  (pure <<< id)

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

    firstOnly :: ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
    firstOnly g = g >=> (pure <<< head)
