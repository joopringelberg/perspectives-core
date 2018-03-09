module Perspectives.QueryCompiler where

import Data.Array (foldl, unsnoc, head)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolName, PropertyName)
import Perspectives.Identifiers (LocalName, deconstructLocalNameFromDomeinURI)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (getContextType, getRolByLocalName)
import Perspectives.PropertyComposition (compose)
import Perspectives.QueryCombinators (closure, closure', concat, contains, filter, hasValue, lastElement, rolesOf, toBoolean)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleAdministration (NamedFunction, TripleGetter)
import Perspectives.TripleGetter (NamedTripleGetter, constructExternalPropertyGetter, constructExternalPropertyLookup, constructInternalPropertyGetter, constructInternalPropertyLookup, constructInverseRolGetter, constructRolGetter, constructRolLookup, constructRolPropertyGetter, constructRolPropertyLookup, constructTripleGetterFromArbitraryFunction)
import Prelude (bind, pure, ($))

constructRolGetter'  :: forall e.
  RolName ->
  MonadPerspectives (AjaxAvarCache e) (NamedFunction (TripleGetter e))
constructRolGetter' rn = do
  -- Is the type of rolType or one of its ancestors q:Query?
  (isAQuery :: Boolean) <- (toBoolean (contains "model:QueryAst$Query" (closure contextType)) rn)
  if isAQuery
    then constructQueryFunction rn
    else pure $ constructRolLookup $ unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI rn)
  where
   contextType = constructTripleGetterFromArbitraryFunction "model:Perspectives$type" getContextType
   localName :: RolName -> LocalName
   localName qn = unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI qn)

-- | From the id of a context that is a description of a Query, construct a function that computes the value of that
-- | query from the id of an entity.
constructQueryFunction :: forall e.
  ContextID ->
  MonadPerspectives (AjaxAvarCache e) (NamedFunction (TripleGetter e))
constructQueryFunction typeDescriptionID = do
  (pspType :: Array ID) <- getContextType typeDescriptionID
  case head pspType of
    Nothing -> pure identity
    (Just "model:QueryAst$constructExternalPropertyGetter") ->
      applyPropertyConstructor constructExternalPropertyGetter
    (Just "model:QueryAst$constructExternalPropertyLookup") ->
      applyPropertyConstructor constructExternalPropertyLookup
    (Just "model:QueryAst$constructInternalPropertyGetter") ->
      applyPropertyConstructor constructInternalPropertyGetter
    (Just "model:QueryAst$constructInternalPropertyLookup") ->
      applyPropertyConstructor constructInternalPropertyLookup
    (Just "model:QueryAst$constructRolPropertyGetter") ->
      applyPropertyConstructor constructRolPropertyGetter
    (Just "model:QueryAst$constructRolPropertyLookup") ->
      applyPropertyConstructor constructRolPropertyLookup
    (Just "model:QueryAst$constructRolGetter") -> do
      ids <- getRolByLocalName "rol" typeDescriptionID
      pure $ maybe identity constructRolGetter (head ids)
    (Just "model:QueryAst$constructRolLookup") -> do
      ids <- getRolByLocalName "rol" typeDescriptionID
      pure $ maybe identity constructRolLookup (head ids)
    (Just "model:QueryAst$constructInverseRolGetter") -> do
      ids <- getRolByLocalName "rol" typeDescriptionID
      pure $ maybe identity constructInverseRolGetter (head ids)
    (Just "model:QueryAst$rolesOf") -> do
      ids <- getRolByLocalName "context" typeDescriptionID
      pure $ maybe identity rolesOf (head ids)
    (Just "model:QueryAst$hasValue") -> applyUnaryCombinator hasValue
    (Just "model:QueryAst$closure") -> applyUnaryCombinator closure
    (Just "model:QueryAst$closure'") -> applyUnaryCombinator closure'
    (Just "model:QueryAst$lastElement'") -> applyUnaryCombinator lastElement
    (Just "model:QueryAst$compose") -> applyBinaryCombinator compose
    (Just "model:QueryAst$concat") -> applyBinaryCombinator concat
    (Just "model:QueryAst$concat") -> do
      criteriumId <- getRolByLocalName "criterium" typeDescriptionID
      criterium <- traverse constructQueryFunction criteriumId
      candidatesId <-  getRolByLocalName "candidates" typeDescriptionID
      candidates <- traverse constructQueryFunction candidatesId
      case head criterium of
        Nothing -> pure identity
        (Just cr) -> case head candidates of
          Nothing -> pure identity
          (Just ca) -> pure $ filter cr ca
    (Just _) -> pure identity
  where
    applyPropertyConstructor :: (PropertyName -> NamedTripleGetter e)
      -> MonadPerspectives (AjaxAvarCache e) (NamedTripleGetter e)
    applyPropertyConstructor f = do
      ids <- getRolByLocalName "property" typeDescriptionID
      pure $ maybe identity f (head ids)

    applyUnaryCombinator :: (NamedTripleGetter e -> NamedTripleGetter e )
      -> MonadPerspectives (AjaxAvarCache e) (NamedTripleGetter e)
    applyUnaryCombinator c = do
      queryId <- getRolByLocalName "query" typeDescriptionID
      query <- traverse constructQueryFunction queryId
      pure $ maybe identity c (head query)

    applyBinaryCombinator :: (NamedTripleGetter e -> NamedTripleGetter e -> NamedTripleGetter e)
      -> MonadPerspectives (AjaxAvarCache e) (NamedTripleGetter e)
    applyBinaryCombinator c = do
      (operandIds :: Array ID) <- getRolByLocalName "operand" typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      case unsnoc operands of
        (Just {init, last}) -> pure $ foldl c last init
        Nothing -> pure identity
