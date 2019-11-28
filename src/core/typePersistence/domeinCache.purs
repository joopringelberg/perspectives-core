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

import Affjax (Request, request) as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (lift, throwError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.AVar (AVar, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (DocReference(..), GetCouchdbAllDocs(..), onAccepted, onCorrectCallAndResponse)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (Namespace, escapeCouchdbDocumentName)
import Perspectives.Persistent (getPerspectEntiteit, removeEntiteit, saveEntiteit)
import Perspectives.PerspectivesState (domeinCacheRemove)
import Perspectives.Representation.Class.Cacheable (cacheOverwritingRevision, cachePreservingRevision, retrieveInternally)
import Perspectives.User (getCouchdbBaseURL)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (*>), (<$>), (<>), (<<<))

storeDomeinFileInCache :: Namespace -> DomeinFile -> MonadPerspectives (AVar DomeinFile)
storeDomeinFileInCache ns df= cachePreservingRevision (DomeinFileId ns) df

removeDomeinFileFromCache :: Namespace -> MonadPerspectives Unit
removeDomeinFileFromCache = void <<< domeinCacheRemove

-- | Change the domeinfile in cache. NOTA BENE: does not store the modified file in Couchdb!
modifyDomeinFileInCache :: (DomeinFile -> DomeinFile) -> Namespace -> MonadPerspectives Unit
modifyDomeinFileInCache modifier ns =
  do
    mAvar <- retrieveInternally (DomeinFileId ns)
    case mAvar of
      Nothing -> throwError $ error $ "modifyDomeinFileInCache cannot find domeinfile in cache: " <> ns
      (Just avar) -> do
        df <- liftAff $ take avar
        -- Because we modify the existing Entiteit, we do not overwrite the version number -
        -- unless that is what our modifier does.
        _ <- cacheOverwritingRevision (DomeinFileId ns) (modifier df)
        pure unit

-----------------------------------------------------------
--
-----------------------------------------------------------
-- | Retrieve a domain file. First looks in the cache. If not found, retrieves it from the database and caches it.
retrieveDomeinFile :: Namespace -> MonadPerspectives DomeinFile
retrieveDomeinFile ns = getPerspectEntiteit (DomeinFileId ns)

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

documentsInDatabase :: DatabaseName -> MonadPerspectives GetCouchdbAllDocs
documentsInDatabase database = do
  baseUrl <- getCouchdbBaseURL
  res <- lift $ AX.request $ domeinRequest {url = baseUrl <> escapeCouchdbDocumentName database <> "/_all_docs"}
  onAccepted res.status [200] "documentsInDatabase"
    (onCorrectCallAndResponse "documentsInDatabase" res.body \(a :: GetCouchdbAllDocs) -> pure unit)

documentNamesInDatabase :: DatabaseName -> MonadPerspectives (Array String)
documentNamesInDatabase database = do
  (GetCouchdbAllDocs cad) <- documentsInDatabase database
  pure $ (\(DocReference{id}) -> id) <$> cad.rows

saveCachedDomeinFile :: ID -> MonadPerspectives Unit
saveCachedDomeinFile ns = saveEntiteit (DomeinFileId ns) *> pure unit

-- | Either create or modify the DomeinFile in couchdb. Caches.
-- | Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
-- | If the model is not found in the cache, assumes it is not in the database either.
storeDomeinFileInCouchdb :: DomeinFile -> MonadPerspectives Unit
storeDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) = do
  void $ storeDomeinFileInCache _id df
  saveCachedDomeinFile _id

-- | Remove the file from couchb. Removes the model from cache.
removeDomeinFileFromCouchdb :: Namespace -> MonadPerspectives Unit
removeDomeinFileFromCouchdb ns = removeEntiteit (DomeinFileId ns) *> pure unit

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
