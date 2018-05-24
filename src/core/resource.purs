module Perspectives.Resource where

import Prelude

import Control.Monad.Aff.AVar (AVar, readVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (kind Effect)
import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, retrieveInternally, getType)
import Perspectives.ResourceRetrieval (fetchPerspectEntiteitFromCouchdb)

-- TODO: DE MAYBE KAN ERAF
getPerspectEntiteit :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) (Maybe a)
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ readVar avar
        pure $ Just pe
      Nothing -> do
        (ent :: a) <- fetchPerspectEntiteitFromCouchdb id
        pure $ Just ent

entiteitType :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) (Array String)
entiteitType id = do
  (ma :: Maybe a) <- getPerspectEntiteit id
  case ma of
    Nothing -> pure []
    (Just a) -> pure $ singleton $ getType a
