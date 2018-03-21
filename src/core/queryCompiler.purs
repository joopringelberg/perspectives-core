module Perspectives.QueryCompiler where

import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, unsnoc, head)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolName, PropertyName)
import Perspectives.Identifiers (LocalName, deconstructLocalNameFromDomeinURI)
import Perspectives.Property (getContextType, getInternalProperty, getRolByLocalName)
import Perspectives.PropertyComposition (compose)
import Perspectives.QueryCombinators (closure, closure', concat, contains, filter, notEmpty, lastElement, rolesOf, toBoolean)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleAdministration (NamedFunction, TripleGetter, MonadPerspectivesQuery)
import Perspectives.TripleGetter (NamedTripleGetter, constructExternalPropertyGetter, constructExternalPropertyLookup, constructInternalPropertyGetter, constructInternalPropertyLookup, constructInverseRolGetter, constructRolGetter, constructRolLookup, constructRolPropertyGetter, constructRolPropertyLookup, constructTripleGetterFromObjectsGetter)
import Prelude (bind, pure, ($))

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
  (pspType :: Array ID) <- lift $ getContextType typeDescriptionID
  case head pspType of
    Nothing -> pure identity
    (Just "model:QueryAst$constructExternalPropertyGetter") ->
      applyPropertyConstructor constructExternalPropertyGetter
    (Just "model:QueryAst$constructExternalPropertyLookup") ->
      pure $ maybe identity constructExternalPropertyLookup (deconstructLocalNameFromDomeinURI typeDescriptionID)
    (Just "model:QueryAst$constructInternalPropertyGetter") ->
      applyPropertyConstructor constructInternalPropertyGetter
    (Just "model:QueryAst$constructInternalPropertyLookup") ->
      pure $ maybe identity constructInternalPropertyLookup (deconstructLocalNameFromDomeinURI typeDescriptionID)
    (Just "model:QueryAst$constructRolPropertyGetter") ->
      applyPropertyConstructor constructRolPropertyGetter
    (Just "model:QueryAst$constructRolPropertyLookup") ->
      pure $ maybe identity constructRolPropertyLookup (deconstructLocalNameFromDomeinURI typeDescriptionID)
    (Just "model:QueryAst$constructRolGetter") -> do
      ids <- lift $ getRolByLocalName "rol" typeDescriptionID
      pure $ maybe identity constructRolGetter (head ids)
    (Just "model:QueryAst$constructRolLookup") -> do
      pure $ maybe identity constructRolLookup (deconstructLocalNameFromDomeinURI typeDescriptionID)
    (Just "model:QueryAst$constructInverseRolGetter") -> do
      ids <- lift $ getRolByLocalName "rol" typeDescriptionID
      pure $ maybe identity constructInverseRolGetter (head ids)
    (Just "model:QueryAst$rolesOf") -> do
      ids <- lift $ getRolByLocalName "context" typeDescriptionID
      pure $ maybe identity rolesOf (head ids)
    (Just "model:QueryAst$notEmpty") -> applyUnaryCombinator notEmpty
    (Just "model:QueryAst$closure") -> applyUnaryCombinator closure
    (Just "model:QueryAst$closure'") -> applyUnaryCombinator closure'
    (Just "model:QueryAst$lastElement'") -> applyUnaryCombinator lastElement
    (Just "model:QueryAst$compose") -> applyBinaryCombinator compose
    (Just "model:QueryAst$concat") -> applyBinaryCombinator concat
    (Just "model:QueryAst$concat") -> do
      criteriumId <- lift $ getRolByLocalName "criterium" typeDescriptionID
      criterium <- traverse constructQueryFunction criteriumId
      candidatesId <-  lift $ getRolByLocalName "candidates" typeDescriptionID
      candidates <- traverse constructQueryFunction candidatesId
      case head criterium of
        Nothing -> pure identity
        (Just cr) -> case head candidates of
          Nothing -> pure identity
          (Just ca) -> pure $ filter cr ca
    (Just _) -> pure identity
  where
    applyPropertyConstructor :: (PropertyName -> NamedTripleGetter e)
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyPropertyConstructor f = do
      ids <- lift $ getRolByLocalName "property" typeDescriptionID
      pure $ maybe identity f (head ids)

    applyUnaryCombinator :: (NamedTripleGetter e -> NamedTripleGetter e )
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyUnaryCombinator c = do
      queryId <- lift $ getRolByLocalName "query" typeDescriptionID
      query <- traverse constructQueryFunction queryId
      pure $ maybe identity c (head query)

    applyBinaryCombinator :: (NamedTripleGetter e -> NamedTripleGetter e -> NamedTripleGetter e)
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyBinaryCombinator c = do
      (operandIds :: Array ID) <- lift $ getRolByLocalName "operand" typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      case unsnoc operands of
        (Just {init, last}) -> pure $ foldl c last init
        Nothing -> pure identity
