module Perspectives.Resource where

import Prelude

import Effect.Aff.AVar (AVar, readVar)
import Effect.Aff.Class (liftAff)
import Effect (kind Effect)
import Control.Monad.Error.Class (catchError)
import Data.Array (singleton)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)

import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, getType, removeInternally, retrieveInternally)
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

tryGetPerspectEntiteit :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) (Maybe a)
tryGetPerspectEntiteit id = catchError ((getPerspectEntiteit id) >>= (pure <<< Just))
  \_ -> do
    (_ :: Maybe (AVar a)) <- removeInternally id
    pure Nothing

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
