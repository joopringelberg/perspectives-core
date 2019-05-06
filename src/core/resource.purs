module Perspectives.Resource where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Array (singleton)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff.AVar (AVar, read)
import Effect.Aff.Class (liftAff)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, getType, removeInternally, retrieveInternally)
import Perspectives.ResourceRetrieval (fetchPerspectEntiteitFromCouchdb, removeEntiteit) as RT

getPerspectEntiteit :: forall a. PerspectEntiteit a => ID -> MonadPerspectives a
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ read avar
        pure pe
      Nothing -> do
        (ent :: a) <- RT.fetchPerspectEntiteitFromCouchdb id
        pure ent

tryGetPerspectEntiteit :: forall a. PerspectEntiteit a => ID -> MonadPerspectives (Maybe a)
tryGetPerspectEntiteit id = catchError ((getPerspectEntiteit id) >>= (pure <<< Just))
  \_ -> do
    (_ :: Maybe (AVar a)) <- removeInternally id
    pure Nothing

getAVarRepresentingPerspectEntiteit :: forall a. PerspectEntiteit a => ID -> MonadPerspectives (AVar a)
getAVarRepresentingPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> pure avar
      Nothing -> do
        (_ :: a) <- RT.fetchPerspectEntiteitFromCouchdb id
        mavar <- retrieveInternally id
        pure $ unsafePartial $ fromJust mavar

entiteitType :: forall a. PerspectEntiteit a => ID -> MonadPerspectives (Array String)
entiteitType id = do
  (a :: a) <- getPerspectEntiteit id
  pure $ singleton $ getType a

removeEntiteit :: forall a. PerspectEntiteit a => String -> MonadPerspectives a
removeEntiteit id = do
  ent <- getPerspectEntiteit id
  RT.removeEntiteit id ent
