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

-- | Instances of contexts and roles (represented as PerspectContext and PerspectRol respectively) are stored in
-- | Couchdb. Before they are stored, and when they are retrieved, they are cached.
-- | On saving a document to Couchdb, it receives a Couchdb version parameter `_rev`.
-- | However, because of the way we serialise our data, we do not 'see' that parameter in our types when they are
-- | fetched and deserialized (Couchdb adds them to the outer JSON value, which is thrown away by generic
-- | deserialisation).
-- |
-- | Therefore we have added a `_rev` parameter to our types and on receiving a JSON document from Couchdb, we set
-- | the actual revision value (sent in the HTTP headers as well) in our data.
-- |
-- | Warning to implementers: in Couchdb you will therefore see **two** _rev parameters: an outer one with the
-- | correct version, and an inner one that is always one step behind.

module Perspectives.Persistent
( saveEntiteit
, saveEntiteit_
, removeEntiteit
, getPerspectEntiteit
, getPerspectContext
, getPerspectRol
, tryGetPerspectEntiteit
, class Persistent
, database
, entitiesDatabaseName
, updateRevision
  )
where

import Prelude

import Affjax (Request, request)
import Affjax.RequestBody as RequestBody
import Control.Monad.Except (catchError, throwError)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Effect.Aff.AVar (AVar, kill, put, read)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (encodeJSON)
import Foreign.Generic.Class (class GenericEncode)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MP)
import Perspectives.Couchdb (PutCouchdbDocument, onAccepted, onCorrectCallAndResponse, version)
import Perspectives.Couchdb.Databases (defaultPerspectRequest, ensureAuthentication, retrieveDocumentVersion)
import Perspectives.CouchdbState (CouchdbUser, UserName)
import Perspectives.DomeinFile (DomeinFile, DomeinFileId)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Representation.Class.Cacheable (class Cacheable, cacheEntity, changeRevision, removeInternally, representInternally, retrieveInternally, rev, setRevision, takeEntiteitFromCache)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.User (getCouchdbBaseURL, getSystemIdentifier)

class (Cacheable v i, Encode v, Decode v) <= Persistent v i | i -> v,  v -> i where
  database :: i -> MP String

instance persistentInstancePerspectContext :: Persistent PerspectContext ContextInstance where
  database _ = do
    sysId <- getSystemIdentifier
    cdbUrl <- getCouchdbBaseURL
    pure $ cdbUrl <> sysId <> "_entities/"

instance persistentInstancePerspectRol :: Persistent PerspectRol RoleInstance where
  database _ = do
    sysId <- getSystemIdentifier
    cdbUrl <- getCouchdbBaseURL
    pure $ cdbUrl <> sysId <> "_entities/"

instance persistentInstanceDomeinFile :: Persistent DomeinFile DomeinFileId where
  database _ = do
    sysId <- getSystemIdentifier
    cdbUrl <- getCouchdbBaseURL
    pure $ cdbUrl <> sysId <> "_models/"

instance persistentCouchdbUser :: Persistent CouchdbUser UserName where
  database _ = do
    cdbUrl <- getCouchdbBaseURL
    pure $ cdbUrl <> "localusers/"

getPerspectEntiteit :: forall a i. Persistent a i => i -> MonadPerspectives a
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ read avar
        pure pe
      Nothing -> fetchEntiteit id

entitiesDatabaseName :: MonadPerspectives String
entitiesDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_entities/")

getPerspectContext :: ContextInstance -> MP PerspectContext
getPerspectContext = getPerspectEntiteit

getPerspectRol :: RoleInstance -> MP PerspectRol
getPerspectRol = getPerspectEntiteit

tryGetPerspectEntiteit :: forall a i. Persistent a i => i -> MonadPerspectives (Maybe a)
tryGetPerspectEntiteit id = catchError ((getPerspectEntiteit id) >>= (pure <<< Just))
  \_ -> pure Nothing

getAVarRepresentingPerspectEntiteit :: forall a i. Persistent a i  => i -> MonadPerspectives (AVar a)
getAVarRepresentingPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> pure avar
      Nothing -> do
        (_ :: a) <- fetchEntiteit id
        mavar <- retrieveInternally id
        -- NOTE: this may fail if the Entiteit was not stored in Couchdb.
        pure $ unsafePartial $ fromJust mavar

-- | Remove from Couchdb if possible and remove from the cache, too.
removeEntiteit :: forall a i. Persistent a i => i -> MonadPerspectives a
removeEntiteit entId = do
  entiteit <- getPerspectEntiteit entId
  ensureAuthentication $ do
    case (rev entiteit) of
      Nothing -> pure entiteit
      (Just rev) -> do
        ebase <- database entId
        (rq :: (Request String)) <- defaultPerspectRequest
        res <- liftAff $ request $ rq {method = Left DELETE, url = (ebase <> unwrap entId <> "?rev=" <> rev)}
        onAccepted res.status [200, 202] "removeEntiteit" $ (removeInternally entId :: MP (Maybe (AVar a))) *> pure entiteit

-- | Fetch the definition of a resource asynchronously. It will have the same version in cache as in Couchdb.
fetchEntiteit :: forall a i. Persistent a i => i -> MonadPerspectives a
fetchEntiteit id = ensureAuthentication $ catchError
  do
    v <- representInternally id
    -- _ <- forkAff do
    ebase <- database id
    (rq :: (Request String)) <- defaultPerspectRequest
    res <- liftAff $ request $ rq {url = ebase <> unwrap id}
    void $ liftAff $ onAccepted res.status [200, 304] "fetchEntiteit"
      (onCorrectCallAndResponse "fetchEntiteit" res.body \a -> put a v)
    liftAff $ read v
  \e -> do
    (mav :: Maybe (AVar a)) <- removeInternally id
    case mav of
      Nothing -> pure unit
      Just av -> liftAff $ kill (error ("Cound not find " <> unwrap id)) av
    throwError $ error ("fetchEntiteit: failed to retrieve resource " <> unwrap id <> " from couchdb. " <> show e)

saveEntiteit :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> MonadPerspectives a
saveEntiteit id = saveEntiteit' id Nothing

saveEntiteit_ :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectives a
saveEntiteit_ entId entiteit = saveEntiteit' entId (Just entiteit)

-- | Save an Entiteit and set its new _rev parameter in the cache.
saveEntiteit' :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> Maybe a -> MonadPerspectives a
saveEntiteit' entId mentiteit = ensureAuthentication $ do
  entityFromCache <- takeEntiteitFromCache entId
  entiteit <- pure case mentiteit of
    Nothing -> entityFromCache
    Just e -> e
  revParam <- pure case (rev entityFromCache) of
    Nothing -> ""
    Just rev -> "?rev=" <> rev
  ebase <- database entId
  (rq :: (Request String)) <- defaultPerspectRequest
  res <- liftAff $ request $ rq {method = Left PUT, url = (ebase <> unwrap entId <> revParam), content = Just $ RequestBody.string  (encodeJSON entiteit)}
  void $ onAccepted res.status [200, 201] ("saveEntiteit_ for " <> (unwrap entId))
    (onCorrectCallAndResponse ("saveEntiteit_ for " <> (unwrap entId)) res.body (\(a :: PutCouchdbDocument) -> do
      v <- version res.headers
      void $ cacheEntity entId (changeRevision v entiteit)))
  pure entiteit

updateRevision :: forall a i. Persistent a i => i -> MonadPerspectives Unit
updateRevision entId = do
  ebase <- database entId
  revision <- retrieveDocumentVersion (ebase <> (unwrap entId))
  setRevision entId revision
