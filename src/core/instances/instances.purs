module Perspectives.Instances
( saveVersionedEntiteit
, saveEntiteit
, saveEntiteitPreservingVersion
, fetchEntiteit
, removeEntiteit
, getPerspectEntiteit
, tryGetPerspectEntiteit
, getAVarRepresentingPerspectEntiteit
  )
where

import Prelude

import Affjax (Request, request)
import Affjax.RequestBody as RequestBody
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (catchError, throwError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Effect.Aff.AVar (AVar, put, read, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Couchdb (PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (ensureAuthentication, defaultPerspectRequest, retrieveDocumentVersion)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, cacheCachedEntiteit, getRevision', readEntiteitFromCache, removeInternally, representInternally, retrieveInternally, setRevision)
import Perspectives.User (entitiesDatabase)
import Simple.JSON (writeJSON)

getPerspectEntiteit :: forall a. PerspectEntiteit a => ID -> MonadPerspectives a
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ read avar
        pure pe
      Nothing -> fetchEntiteit id

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
        (_ :: a) <- fetchEntiteit id
        mavar <- retrieveInternally id
        pure $ unsafePartial $ fromJust mavar

removeEntiteit :: forall a. PerspectEntiteit a => String -> MonadPerspectives a
removeEntiteit entId = do
  entiteit <- getPerspectEntiteit entId
  ensureAuthentication $ do
    case (getRevision' entiteit) of
      Nothing -> throwError $ error ("removeEntiteit: entiteit has no revision, removal is impossible: " <> entId)
      (Just rev) -> do
        ebase <- entitiesDatabase
        (rq :: (Request String)) <- defaultPerspectRequest
        res <- liftAff $ request $ rq {method = Left DELETE, url = (ebase <> entId <> "?rev=" <> rev)}
        onAccepted res.status [200, 202] "removeEntiteit" $ (removeInternally entId :: MP (Maybe (AVar a))) *> pure entiteit

-- | Fetch the definition of a resource asynchronously.
fetchEntiteit :: forall a. PerspectEntiteit a => ID -> MonadPerspectives a
fetchEntiteit id = ensureAuthentication $ catchError
  do
    v <- representInternally id
    -- _ <- forkAff do
    ebase <- entitiesDatabase
    (rq :: (Request String)) <- defaultPerspectRequest
    res <- liftAff $ request $ rq {url = ebase <> id}
    void $ liftAff $ onAccepted res.status [200, 304] "fetchEntiteit"
      (onCorrectCallAndResponse "fetchEntiteit" res.body \(a :: a) -> put a v)
    liftAff $ read v
  \e -> throwError $ error ("fetchEntiteit: failed to retrieve resource " <> id <> " from couchdb. " <> show e)

-- | Save a user Resource.
saveEntiteitPreservingVersion :: forall a. PerspectEntiteit a => ID -> MonadPerspectives a
-- saveEntiteitPreservingVersion id = catchError do
--     (_ :: a) <- fetchPerspectEntiteitFromCouchdb id
--     saveEntiteit id
--   \e -> saveEntiteit id
saveEntiteitPreservingVersion = saveEntiteit

saveEntiteit :: forall a. PerspectEntiteit a => ID -> MonadPerspectives a
saveEntiteit id = do
  pe <- readEntiteitFromCache id
  case getRevision' pe of
    Nothing -> saveUnversionedEntiteit id
    otherwise -> saveVersionedEntiteit id pe

-- | A Resource may be created and stored locally, but not sent to the couchdb. Send such resources to
-- | couchdb with this function.
saveUnversionedEntiteit :: forall a. PerspectEntiteit a => ID -> MonadPerspectives a
saveUnversionedEntiteit id = ensureAuthentication $ do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("saveUnversionedEntiteit needs a locally stored resource for " <> id)
    (Just avar) -> do
      pe <- liftAff $ take avar
      ebase <- entitiesDatabase
      (rq :: (Request String)) <- defaultPerspectRequest
      res <- liftAff $ request $ rq {method = Left PUT, url = (ebase <> id), content = Just $ RequestBody.string (writeJSON pe)}
      if res.status == (StatusCode 409)
        then retrieveDocumentVersion (ebase <> id) >>= pure <<< (flip setRevision pe) >>= saveVersionedEntiteit id
        else do
          void $ onAccepted res.status [200, 201] "saveUnversionedEntiteit"
            (onCorrectCallAndResponse "saveUnversionedEntiteit" res.body (\(a :: PutCouchdbDocument) -> void $ liftAff $ put (setRevision (unsafePartial $ fromJust (unwrap a).rev) pe) avar))
          pure pe

saveVersionedEntiteit :: forall a. PerspectEntiteit a => ID -> a -> MonadPerspectives a
saveVersionedEntiteit entId entiteit = ensureAuthentication $ do
  case (getRevision' entiteit) of
    Nothing -> throwError $ error ("saveVersionedEntiteit: entiteit has no revision, deltas are impossible: " <> entId)
    (Just rev) -> do
      ebase <- entitiesDatabase
      (rq :: (Request String)) <- defaultPerspectRequest
      res <- liftAff $ request $ rq {method = Left PUT, url = (ebase <> entId <> "?_rev=" <> rev), content = Just $ RequestBody.string  (writeJSON entiteit)}
      if res.status == (StatusCode 409)
        then retrieveDocumentVersion entId >>= pure <<< (flip setRevision entiteit) >>= saveVersionedEntiteit entId
        else do
          void $ onAccepted res.status [200, 201] "saveVersionedEntiteit"
            (onCorrectCallAndResponse "saveVersionedEntiteit" res.body (\(a :: PutCouchdbDocument) -> void $ cacheCachedEntiteit entId (setRevision (unsafePartial $ fromJust (unwrap a).rev) entiteit)))
          pure entiteit
