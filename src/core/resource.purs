module Perspectives.Resource where

import Prelude

import Control.Monad.Aff.AVar (AVar, readVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (kind Effect)
import Data.Array (singleton)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, retrieveInternally, getType)
import Perspectives.ResourceRetrieval (fetchPerspectEntiteitFromCouchdb)

getPerspectEntiteit :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) a
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ readVar avar
        pure pe
      Nothing -> do
        (ent :: a) <- fetchPerspectEntiteitFromCouchdb id
        pure ent

getAVarRepresentingPerspectEntiteit :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) (AVar a)
getAVarRepresentingPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> pure avar
      Nothing -> do
        (_ :: a) <- fetchPerspectEntiteitFromCouchdb id
        mavar <- retrieveInternally id
        pure $ unsafePartial $ fromJust mavar

entiteitType :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) (Array String)
entiteitType id = do
  (a :: a) <- getPerspectEntiteit id
  pure $ singleton $ getType a
