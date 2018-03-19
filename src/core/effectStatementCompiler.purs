module Perspectives.EffectStatementCompiler where

import Data.Array (foldl, unsnoc, head)
import Data.Maybe (Maybe(..))
import Data.URI (Query(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, Subject)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (ObjectsGetter, getContextType, getExternalProperty, getProperty)
import Perspectives.QueryEffect (QueryEffect)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.SystemQueries (identity)
import Perspectives.TripleAdministration (NamedFunction(..), MonadPerspectivesQuery)
import Perspectives.TripleGetter (constructExternalPropertyGetter, readQueryVariable, (##))
import Prelude (Unit, bind, const, otherwise, pure, unit, ($), (>=>), (>>=))

type Statement e = ContextID -> MonadPerspectivesQuery (AjaxAvarCache e) Unit

type EffectExpression e = Subject -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String)

constructEffectExpressie :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (EffectExpression e)
constructEffectExpressie typeDescriptionID = do
  (pspType :: Array ID) <- getContextType typeDescriptionID
  case head pspType of
    Nothing -> pure $ const $ pure []
    (Just "model:Effect$Constant") -> do
      arrWithConst <- getExternalProperty "model:Effect$Constant$value" typeDescriptionID
      pure $ const $ pure arrWithConst
    (Just "model:Effect$Effect$variable") -> do
      mVar <- getProperty "model:Effect$Variable$name" typeDescriptionID
      case head mVar of
        Nothing -> pure $ const $ pure []
        Just var -> pure $ \_ -> readQueryVariable var
    (Just _) -> pure $ const $ pure []
