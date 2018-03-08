module Perspectives.QueryCompiler where

import Data.Array (foldl, unsnoc, head)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (RolName, ID)
import Perspectives.Identifiers (LocalName, deconstructLocalNameFromDomeinURI)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (getContextType, getRolByLocalName)
import Perspectives.PropertyComposition (compose)
import Perspectives.QueryCombinators (closure, contains, toBoolean)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleAdministration (NamedFunction, TripleGetter)
import Perspectives.TripleGetter (constructRolLookup, constructTripleGetterFromArbitraryFunction)
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

constructQueryFunction :: forall e.
  RolName ->
  MonadPerspectives (AjaxAvarCache e) (NamedFunction (TripleGetter e))
constructQueryFunction rn = do
  -- (propDef :: PerspectContext) <- getPerspectEntiteit rn
  (pspType :: Array ID) <- getContextType rn
  case head pspType of
    Nothing -> pure identity
    (Just "model:QueryAst$compose") -> do
      (operandIds :: Array ID) <- getRolByLocalName "operand" rn
      operands <- traverse constructQueryFunction operandIds
      case unsnoc operands of
        (Just {init, last}) -> pure $ foldl compose last init
        -- otherwise -> pure identity
        Nothing -> pure identity
    (Just _) -> pure identity
