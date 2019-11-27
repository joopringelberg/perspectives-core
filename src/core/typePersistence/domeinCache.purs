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

module Perspectives.DomeinCache
-- ( retrieveDomeinResourceDefinition )

where

import Affjax (Request, request, put) as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Aff (Aff, catchError)
import Effect.Aff.AVar (AVar, empty, put, read, take)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Effect.Exception (error)
import Foreign.Generic (defaultOptions, genericEncodeJSON)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (DocReference(..), GetCouchdbAllDocs(..), PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (defaultPerspectRequest, retrieveDocumentVersion, documentExists, version)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (Namespace, escapeCouchdbDocumentName)
import Perspectives.PerspectivesState (domeinCacheInsert, domeinCacheLookup, domeinCacheRemove)
import Perspectives.Representation.Class.Revision (Revision_)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (*>), (<$>), (<>), (==), (>>=), (<<<))

-- | as in [Perspectives.Instances](Perspectives.Instances.html#t:x), a DomeinFile stored
-- | in Couchdb has a tag `_rev` in its serialisation that is filled by Couchb, and
-- | a similar member in its type, that is always one step behind.
-- | We therefore actualise the version number of a DomeinFile in cache on receiving
-- | it from Couchdb. We also actualise it upon storing a new version. In this way
-- | we make sure that the 'internal' _rev equals the current 'external' _rev in Couchdb.
-- |
-- |
-- |

type URL = String

storeDomeinFileInCache :: Namespace -> DomeinFile -> MonadPerspectives (AVar DomeinFile)
storeDomeinFileInCache ns df= do
  ev <- (liftAff empty) >>= domeinCacheInsert ns
  liftAff $ put df ev
  pure ev

removeDomeinFileFromCache :: Namespace -> MonadPerspectives Unit
removeDomeinFileFromCache = void <<< domeinCacheRemove

-- | Change the domeinfile in cache. NOTA BENE: does not store the modified file in Couchdb!
modifyDomeinFileInCache :: (DomeinFile -> DomeinFile) -> Namespace -> MonadPerspectives Unit
modifyDomeinFileInCache modifier ns = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> throwError $ error $ "modifyDomeinFileInCache cannot find domeinfile in cache: " <> ns
    (Just avar) -> do
      df <- liftAff $ take avar
      liftAff $ put (modifier df) avar

-----------------------------------------------------------
--
-----------------------------------------------------------
-- | Retrieve a domain file. First looks in the cache. If not found, retrieves it from the database and caches it.
retrieveDomeinFile :: Namespace -> MonadPerspectives DomeinFile
retrieveDomeinFile ns = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> do
      ev <- (liftAff empty) >>= domeinCacheInsert ns
      -- forkAff hinders catchError.
      -- _ <- forkAff do
      res <- catchError
        (liftAff $ AX.request $ domeinRequest {url = modelsURL <> escapeCouchdbDocumentName ns})
        \e -> throwError $ error $ "Failure in retrieveDomeinFile for: " <> ns <> ". " <> show e
      onAccepted res.status [200, 304] "retrieveDomeinFile"
        (onCorrectCallAndResponse "retrieveDomeinFile" res.body (\(DomeinFile a) -> do
          v <- version res.headers
          liftAff $ put (DomeinFile (a {_rev = v})) ev
        ))
    (Just avar) -> liftAff $ read avar

retrieveDomeinFileFromCache :: Namespace -> MonadPerspectives (Maybe DomeinFile)
retrieveDomeinFileFromCache ns = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> pure Nothing
    (Just avar) -> liftAff (read avar) >>= pure <<< Just

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

documentsInDatabase :: DatabaseName -> Aff GetCouchdbAllDocs
documentsInDatabase database = do
  res <- AX.request $ domeinRequest {url = baseURL <> escapeCouchdbDocumentName database <> "/_all_docs"}
  onAccepted res.status [200] "documentsInDatabase"
    (onCorrectCallAndResponse "documentsInDatabase" res.body \(a :: GetCouchdbAllDocs) -> pure unit)

documentNamesInDatabase :: DatabaseName -> Aff (Array String)
documentNamesInDatabase database = do
  (GetCouchdbAllDocs cad) <- documentsInDatabase database
  pure $ (\(DocReference{id}) -> id) <$> cad.rows

saveCachedDomeinFile :: ID -> MonadPerspectives Unit
saveCachedDomeinFile ns = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> throwError $ error $ "saveCachedDomeinFile: cannot find domeinfile in cache: " <> ns
    (Just avar) -> do
      df <- liftAff $ read avar
      modifyDomeinFileInCouchdb df avar

-- | Either create or modify the DomeinFile in couchdb. Caches.
-- | Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
-- | If the model is not found in the cache, assumes it is not in the database either.
storeDomeinFileInCouchdb :: DomeinFile -> MonadPerspectives Unit
storeDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) = do
  mAvar <- domeinCacheLookup _id
  -- mAvar <- liftEffect $ peek domeinCache _id
  case mAvar of
    Nothing -> do
      rev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName _id)
      if isJust rev
        then do
          versionedDf <- pure (DomeinFile dfr {_rev = rev})
          ev <- storeDomeinFileInCache _id versionedDf
          modifyDomeinFileInCouchdb versionedDf ev
        else createDomeinFileInCouchdb df
    (Just avar) -> modifyDomeinFileInCouchdb df avar

createDomeinFileInCouchdb :: DomeinFile -> MonadPerspectives Unit
createDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) = do
  ev <- (liftAff empty) >>= domeinCacheInsert _id
  res <- liftAff $ AX.put ResponseFormat.string (modelsURL <> escapeCouchdbDocumentName _id) (RequestBody.string (genericEncodeJSON defaultOptions df))

  if res.status == (StatusCode 409)
    then do
      rev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName _id)
      setRevision rev ev
      updatedDomeinFile <- liftAff $ read ev
      modifyDomeinFileInCouchdb updatedDomeinFile ev
    else onAccepted res.status [200, 201] "createDomeinFileInCouchdb"
      (void $ onCorrectCallAndResponse "createDomeinFileInCouchdb" res.body \(a :: PutCouchdbDocument) -> do
        v <- version res.headers
        void $ setRevision v ev)

  where
    setRevision :: Revision_ -> (AVar DomeinFile) -> MonadPerspectives Unit
    setRevision s av = liftAff $ put (DomeinFile (dfr {_rev = s})) av

modifyDomeinFileInCouchdb :: DomeinFile -> (AVar DomeinFile) -> MonadPerspectives Unit
modifyDomeinFileInCouchdb df@(DomeinFile dfr@{_id, _rev}) av = do
  oldDf <- liftAff $ take av
  res <- liftAff $ AX.put
    ResponseFormat.string
    (modelsURL <> escapeCouchdbDocumentName _id <> "?rev=" <> (unsafePartial $ fromJust _rev))
    (RequestBody.string (genericEncodeJSON defaultOptions df))
  if res.status == (StatusCode 409)
    then do
      rev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName _id)
      setRevision rev
      updatedDomeinFile <- liftAff $ read av
      modifyDomeinFileInCouchdb updatedDomeinFile av
    else onAccepted res.status [200, 201] "modifyDomeinFileInCouchdb"
      (void (onCorrectCallAndResponse "modifyDomeinFileInCouchdb" res.body \(a :: PutCouchdbDocument)-> do
        rev <- version res.headers
        setRevision rev))
  where
    setRevision :: Revision_ -> MonadPerspectives Unit
    setRevision s = liftAff $ put (DomeinFile (dfr {_rev = s})) av

-- | Remove the file from couchb. Removes the model from cache.
removeDomeinFileFromCouchdb :: Namespace -> MonadPerspectives Unit
removeDomeinFileFromCouchdb ns = do
  mrev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName ns)
  case mrev of
    Nothing -> throwError (error "removeDomeinFileFromCouchdb needs a revision of the document to be removed.")
    Just rev -> do
      (rq :: (AX.Request String)) <- defaultPerspectRequest
      res <- liftAff $ AX.request $ rq {method = Left DELETE, url = (modelsURL <> escapeCouchdbDocumentName ns <> "?rev=" <> rev)}
      onAccepted res.status [200, 202] "removeDomeinFileFromCouchdb" $ domeinCacheRemove ns *> pure unit

modelsURL :: URL
modelsURL = "http://localhost:5984/perspect_models/"

baseURL :: URL
baseURL = "http://localhost:5984/"

domeinRequest :: AX.Request String
domeinRequest =
  { method: Left GET
  , url: "http://localhost:5984/models2model_SysteemDomein_"
  , headers: []
  , content: Nothing
  , username: Just "cor"
  , password: Just "geheim"
  , withCredentials: true
  , responseFormat: ResponseFormat.string
  }
