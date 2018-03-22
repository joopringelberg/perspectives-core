module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, unsnoc, head)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolName, PropertyName)
import Perspectives.Identifiers (LocalName, deconstructLocalNameFromDomeinURI)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (ObjectsGetter, getContextType, getExternalProperty, getInternalProperty, getRolByLocalName)
import Perspectives.PropertyComposition (compose)
import Perspectives.QueryCombinators (closure, closure', concat, constant, contains, filter, lastElement, notEmpty, rolesOf, toBoolean)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleAdministration (MonadPerspectivesQuery, NamedFunction(..), Triple(..), TripleGetter)
import Perspectives.TripleGetter (NamedTripleGetter, constructExternalPropertyGetter, constructExternalPropertyLookup, constructInternalPropertyGetter, constructInternalPropertyLookup, constructInverseRolGetter, constructRolGetter, constructRolLookup, constructRolPropertyGetter, constructRolPropertyLookup, constructTripleGetterFromObjectsGetter, constructTripleGetterFromObjectsGetter', putQueryVariable, readQueryVariable)
import Perspectives.Utilities (onNothing, onNothing')
import Prelude (bind, const, pure, ($), (<<<), (<>), (>=>), (>>=), discard)

-- | From a qualified name for a Rol, construct a function that computes the instances of that Rol for a given context.
-- | The Rol may be defined as computed.
rolQuery  :: forall e.
  RolName ->
  MonadPerspectivesQuery (AjaxAvarCache e) (NamedFunction (TripleGetter e))
rolQuery rn = do
  -- Is the type of rolType or one of its ancestors q:Query?
  (isAQuery :: Boolean) <- (toBoolean (contains "model:QueryAst$Query" (closure contextType)) rn)
  if isAQuery
    then constructQueryFunction rn -- TODO: voeg de uitgangscontext toe als de variabele #context aan state.
    else pure $ constructRolLookup $ unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI rn)
  where
   contextType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getContextType
   localName :: RolName -> LocalName
   localName qn = unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI qn)

-- | From the id of a context that is a description of a Query, construct a function that computes the value of that
-- | query from the id of an entity.
constructQueryFunction :: forall e.
  ContextID ->
  MonadPerspectivesQuery (AjaxAvarCache e) (NamedFunction (TripleGetter e))
constructQueryFunction typeDescriptionID = do
  pspType <- lift $ onNothing (errorMessage "no type found" "")
    (firstOnly getContextType typeDescriptionID)
  case pspType of
    "model:QueryAst$constructExternalPropertyGetter" ->
      applyPropertyConstructor constructExternalPropertyGetter pspType
    "model:QueryAst$constructExternalPropertyLookup" ->
      pure $ maybe identity constructExternalPropertyLookup (deconstructLocalNameFromDomeinURI typeDescriptionID)
    "model:QueryAst$constructInternalPropertyGetter" ->
      applyPropertyConstructor constructInternalPropertyGetter pspType
    "model:QueryAst$constructInternalPropertyLookup" ->
      pure $ maybe identity constructInternalPropertyLookup (deconstructLocalNameFromDomeinURI typeDescriptionID)
    "model:QueryAst$constructRolPropertyGetter" ->
      applyPropertyConstructor constructRolPropertyGetter pspType
    "model:QueryAst$constructRolPropertyLookup" ->
      pure $ maybe identity constructRolPropertyLookup (deconstructLocalNameFromDomeinURI typeDescriptionID)
    "model:QueryAst$constructRolGetter" -> do
      ids <- lift $ getRolByLocalName "rol" typeDescriptionID
      pure $ maybe identity constructRolGetter (head ids)
    "model:QueryAst$constructRolLookup" -> do
      pure $ maybe identity constructRolLookup (deconstructLocalNameFromDomeinURI typeDescriptionID)
    "model:QueryAst$constructInverseRolGetter" -> do
      ids <- lift $ getRolByLocalName "rol" pspType
      pure $ maybe identity constructInverseRolGetter (head ids)
    "model:QueryAst$rolesOf" -> do
      ids <- lift $ getRolByLocalName "context" pspType
      pure $ maybe identity rolesOf (head ids)
    "model:QueryAst$notEmpty" -> applyUnaryCombinator notEmpty pspType
    "model:QueryAst$closure" -> applyUnaryCombinator closure pspType
    "model:QueryAst$closure'" -> applyUnaryCombinator closure' pspType
    "model:QueryAst$lastElement'" -> applyUnaryCombinator lastElement pspType
    "model:QueryAst$compose" -> applyBinaryCombinator compose pspType
    "model:QueryAst$concat" -> applyBinaryCombinator concat pspType
    "model:QueryAst$filter" -> do
      criteriumId <- lift $ getRolByLocalName "criterium" typeDescriptionID
      criterium <- traverse constructQueryFunction criteriumId
      candidatesId <-  lift $ getRolByLocalName "candidates" typeDescriptionID
      candidates <- traverse constructQueryFunction candidatesId
      case head criterium of
        Nothing -> pure identity
        (Just cr) -> case head candidates of
          Nothing -> pure identity
          (Just ca) -> pure $ filter cr ca
    "model:QueryAst$Constant" -> do
      constId <- lift $ onNothing (errorMessage "no constant value provided" pspType) (firstOnly (getExternalProperty "model:QueryAst$Constant$value") typeDescriptionID)
      pure $ constant constId
    "model:QueryAst$Variable" -> do
      variableName <- lift $ onNothing (errorMessage "no variable name found" pspType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      pure $ constructTripleGetterFromObjectsGetter' ("model:QueryAst$Variable_" <> variableName) (const $ readQueryVariable variableName)
    "model:QueryAst$setVariable" -> do
      variableName <- lift $ onNothing (errorMessage "no variable name found" pspType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      valueDescriptionID <- lift $ onNothing (errorMessage "no value found" pspType)
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery@(NamedFunction nameOfq q) <- constructQueryFunction valueDescriptionID
      predicateName <- pure ("set_" <> variableName <> "_" <> nameOfq)
      pure $ NamedFunction predicateName
        \id -> do
          (Triple tf@{object : values}) <- q id
          putQueryVariable variableName values
          pure $ Triple tf {predicate = predicateName}

    -- Any other argument will be passed as is, thus implementing that we can create arbitrary contexts.
    _ -> pure $ constant typeDescriptionID
  where
    applyPropertyConstructor :: (PropertyName -> NamedTripleGetter e)
      -> ID
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyPropertyConstructor f pspType = do
      id <- lift $ onNothing (errorMessage "no property found" pspType)
        (firstOnly (getRolByLocalName "property") typeDescriptionID)
      pure $ f id

    applyUnaryCombinator :: (NamedTripleGetter e -> NamedTripleGetter e )
      -> ID
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyUnaryCombinator c pspType = do
      queryId <- lift $ onNothing (errorMessage "no query found" pspType)
        (firstOnly (getRolByLocalName "query") typeDescriptionID)
      ((constructQueryFunction queryId) >>= pure <<< c)

    applyBinaryCombinator :: (NamedTripleGetter e -> NamedTripleGetter e -> NamedTripleGetter e)
      -> ID
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyBinaryCombinator c pspType = do
      (operandIds :: Array ID) <- lift $ getRolByLocalName "operand" typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" pspType) (unsnoc operands)
      pure $ foldl c last init

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructEffectStatement: " <> s <> " for: " <> t <> " " <> typeDescriptionID)

    firstOnly :: ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
    firstOnly g = g >=> (pure <<< head)
