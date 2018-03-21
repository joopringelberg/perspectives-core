module Perspectives.EffectStatementCompiler where

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Perspectives.Deltas (MonadTransactie, addRol)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, Subject)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (getContextType, getExternalProperty, getProperty, getRolByLocalName)
import Perspectives.TripleAdministration (MonadPerspectivesQuery)
import Perspectives.TripleGetter (readQueryVariable, runTripleGetter)
import Prelude (Unit, bind, const, pure, unit, ($), (<>))

type Statement e = ContextID -> MonadTransactie e Unit

type EffectExpression e = Subject -> MonadPerspectivesQuery e (Array String)

constructEffectExpressie :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (EffectExpression (AjaxAvarCache e))
constructEffectExpressie typeDescriptionID = do
  (pspType :: Array ID) <- getContextType typeDescriptionID
  case head pspType of
    Nothing -> emptyResultExpression
    (Just "model:Effect$Constant") -> do
      arrWithConst <- getExternalProperty "model:Effect$Constant$value" typeDescriptionID
      pure $ const $ pure arrWithConst
    (Just "model:Effect$Effect$variable") -> do
      mVar <- getProperty "model:Effect$Variable$name" typeDescriptionID
      case head mVar of
        Nothing -> emptyResultExpression
        Just var -> pure $ \_ -> readQueryVariable var
    (Just _) -> emptyResultExpression
  where
    emptyResultExpression :: MonadPerspectives (AjaxAvarCache e) (EffectExpression (AjaxAvarCache e))
    emptyResultExpression = pure $ const $ pure []

constructEffectStatement :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (Statement e)
constructEffectStatement typeDescriptionID = do
  (pspType :: Array ID) <- getContextType typeDescriptionID
  case head pspType of
    Nothing -> throwError $ error $ "constructEffectExpressie: no type found for: " <> typeDescriptionID
    (Just "model:Effect$addRol") -> do
      valueDescriptionID <- getRolByLocalName "value" typeDescriptionID
      (valueQuery) <- maybe (pure emptyResultExpression) constructEffectExpressie (head valueDescriptionID)
      mRolName <- pure (deconstructLocalNameFromDomeinURI typeDescriptionID)
      case mRolName of
        Nothing -> throwError $ error $ "constructEffectExpressie: no local rolname provided in " <> typeDescriptionID
        (Just rolName) ->
          pure (\cid -> do
                          (values :: Array ID) <- lift $ runTripleGetter cid valueQuery
                          for_ values \rolId -> addRol cid rolName rolId)
    (Just _) -> pure emptyStatement
  where
    emptyStatement :: (Statement e)
    emptyStatement = const $ pure unit

    emptyResultExpression :: (EffectExpression (AjaxAvarCache e))
    emptyResultExpression = const $ pure []
