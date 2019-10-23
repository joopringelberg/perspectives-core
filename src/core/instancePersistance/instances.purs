-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Instances
( saveVersionedEntiteit
, saveEntiteit
, saveEntiteitPreservingVersion
, fetchEntiteit
, removeEntiteit
, getPerspectEntiteit
, getPerspectContext
, tryGetPerspectEntiteit
, getAVarRepresentingPerspectEntiteit
, class PersistentInstance
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
import Foreign.Class (class Decode)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Couchdb (PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (ensureAuthentication, defaultPerspectRequest, retrieveDocumentVersion)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Representation.Class.Persistent (class Persistent, cacheCachedEntiteit, changeRevision, readEntiteitFromCache, removeInternally, representInternally, retrieveInternally, rev)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.User (entitiesDatabase)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)

class (Persistent v i, WriteForeign v, ReadForeign v, Decode v) <= PersistentInstance v i | i -> v,  v -> i

instance persistentInstancePerspectContext :: PersistentInstance PerspectContext ContextInstance

instance persistentInstancePerspectRol :: PersistentInstance PerspectRol RoleInstance

getPerspectEntiteit :: forall a i. PersistentInstance a i => i -> MonadPerspectives a
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ read avar
        pure pe
      Nothing -> fetchEntiteit id

getPerspectContext :: ContextInstance -> MP PerspectContext
getPerspectContext = getPerspectEntiteit

tryGetPerspectEntiteit :: forall a i. PersistentInstance a i => i -> MonadPerspectives (Maybe a)
tryGetPerspectEntiteit id = catchError ((getPerspectEntiteit id) >>= (pure <<< Just))
  \_ -> do
    (_ :: Maybe (AVar a)) <- removeInternally id
    pure Nothing

getAVarRepresentingPerspectEntiteit :: forall a i. PersistentInstance a i  => i -> MonadPerspectives (AVar a)
getAVarRepresentingPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> pure avar
      Nothing -> do
        (_ :: a) <- fetchEntiteit id
        mavar <- retrieveInternally id
        pure $ unsafePartial $ fromJust mavar

removeEntiteit :: forall a i. PersistentInstance a i => i -> MonadPerspectives a
removeEntiteit entId = do
  entiteit <- getPerspectEntiteit entId
  ensureAuthentication $ do
    case (rev entiteit) of
      Nothing -> throwError $ error ("removeEntiteit: entiteit has no revision, removal is impossible: " <> unwrap entId)
      (Just rev) -> do
        ebase <- entitiesDatabase
        (rq :: (Request String)) <- defaultPerspectRequest
        res <- liftAff $ request $ rq {method = Left DELETE, url = (ebase <> unwrap entId <> "?rev=" <> rev)}
        onAccepted res.status [200, 202] "removeEntiteit" $ (removeInternally entId :: MP (Maybe (AVar a))) *> pure entiteit

-- | Fetch the definition of a resource asynchronously.
fetchEntiteit :: forall a i. PersistentInstance a i => i -> MonadPerspectives a
fetchEntiteit id = ensureAuthentication $ catchError
  do
    v <- representInternally id
    -- _ <- forkAff do
    ebase <- entitiesDatabase
    (rq :: (Request String)) <- defaultPerspectRequest
    res <- liftAff $ request $ rq {url = ebase <> unwrap id}
    void $ liftAff $ onAccepted res.status [200, 304] "fetchEntiteit"
      (onCorrectCallAndResponse "fetchEntiteit" res.body \(a :: a) -> put a v)
    liftAff $ read v
  \e -> throwError $ error ("fetchEntiteit: failed to retrieve resource " <> unwrap id <> " from couchdb. " <> show e)

-- | Save a user Resource.
saveEntiteitPreservingVersion :: forall a i. PersistentInstance a i => i -> MonadPerspectives a
-- saveEntiteitPreservingVersion id = catchError do
--     (_ :: a) <- fetchPerspectEntiteitFromCouchdb id
--     saveEntiteit id
--   \e -> saveEntiteit id
saveEntiteitPreservingVersion = saveEntiteit

saveEntiteit :: forall a i. PersistentInstance a i => i -> MonadPerspectives a
saveEntiteit id = do
  pe <- readEntiteitFromCache id
  case rev pe of
    Nothing -> saveUnversionedEntiteit id
    otherwise -> saveVersionedEntiteit id pe

-- | A Resource may be created and stored locally, but not sent to the couchdb. Send such resources to
-- | couchdb with this function.
saveUnversionedEntiteit :: forall a i. PersistentInstance a i => i -> MonadPerspectives a
saveUnversionedEntiteit id = ensureAuthentication $ do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("saveUnversionedEntiteit needs a locally stored resource for " <> unwrap id)
    (Just avar) -> do
      pe <- liftAff $ take avar
      ebase <- entitiesDatabase
      (rq :: (Request String)) <- defaultPerspectRequest
      res <- liftAff $ request $ rq {method = Left PUT, url = (ebase <> unwrap id), content = Just $ RequestBody.string (writeJSON pe)}
      if res.status == (StatusCode 409)
        then retrieveDocumentVersion (ebase <> unwrap id) >>= pure <<< (flip changeRevision pe) <<< Just >>= saveVersionedEntiteit id
        else do
          void $ onAccepted res.status [200, 201] "saveUnversionedEntiteit"
            (onCorrectCallAndResponse "saveUnversionedEntiteit" res.body (\(a :: PutCouchdbDocument) -> void $ liftAff $ put (changeRevision (unwrap a).rev pe) avar))
          pure pe

saveVersionedEntiteit :: forall a i. PersistentInstance a i => i -> a -> MonadPerspectives a
saveVersionedEntiteit entId entiteit = ensureAuthentication $ do
  case (rev entiteit) of
    Nothing -> throwError $ error ("saveVersionedEntiteit: entiteit has no revision, deltas are impossible: " <> unwrap entId)
    (Just rev) -> do
      ebase <- entitiesDatabase
      (rq :: (Request String)) <- defaultPerspectRequest
      res <- liftAff $ request $ rq {method = Left PUT, url = (ebase <> unwrap entId <> "?_rev=" <> rev), content = Just $ RequestBody.string  (writeJSON entiteit)}
      if res.status == (StatusCode 409)
        then retrieveDocumentVersion (unwrap entId) >>= pure <<< (flip changeRevision entiteit) <<< Just >>= saveVersionedEntiteit entId
        else do
          void $ onAccepted res.status [200, 201] "saveVersionedEntiteit"
            (onCorrectCallAndResponse "saveVersionedEntiteit" res.body (\(a :: PutCouchdbDocument) -> void $ cacheCachedEntiteit entId (changeRevision (unwrap a).rev entiteit)))
          pure entiteit
