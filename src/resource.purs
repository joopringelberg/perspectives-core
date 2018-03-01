module Perspectives.Resource where

import Prelude
import Control.Monad.Aff.AVar (AVar, readVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (kind Effect)
import Data.Maybe (Maybe(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, retrieveInternally)
import Perspectives.PerspectivesState (MonadPerspectives)
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
