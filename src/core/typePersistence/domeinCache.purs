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

import Affjax (Request) as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (catchError, throwError)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.AVar (AVar, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.Identifiers (Namespace)
import Perspectives.Persistent (getPerspectEntiteit, removeEntiteit, saveEntiteit, tryGetPerspectEntiteit, tryRemoveEntiteit, updateRevision)
import Perspectives.PerspectivesState (domeinCacheRemove)
import Perspectives.Representation.Class.Cacheable (cacheEntity, retrieveInternally)
import Perspectives.User (getCouchdbPassword, getUser)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (*>), (<<<), (<>), (<$>))

storeDomeinFileInCache :: Namespace -> DomeinFile -> MonadPerspectives (AVar DomeinFile)
storeDomeinFileInCache ns df= cacheEntity (DomeinFileId ns) df

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
        _ <- cacheEntity (DomeinFileId ns) (modifier df)
        pure unit

-----------------------------------------------------------
-- RETRIEVE A DOMAINFILE
-----------------------------------------------------------
-- | Retrieve a domain file. First looks in the cache. If not found, retrieves it from the database and caches it.
retrieveDomeinFile :: Namespace -> MonadPerspectives DomeinFile
retrieveDomeinFile ns = catchError (getPerspectEntiteit (DomeinFileId ns))
  \e -> throwError $ error ("retrieveDomeinFile: " <> show e)

tryRetrieveDomeinFile :: Namespace -> MonadPerspectives (Maybe DomeinFile)
tryRetrieveDomeinFile id = catchError (Just <$> (getPerspectEntiteit (DomeinFileId id)))
  \_ -> pure Nothing

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

saveCachedDomeinFile :: DomeinFileId -> MonadPerspectives Unit
saveCachedDomeinFile ns = do
  updateRevision ns
  saveEntiteit ns *> pure unit

-- | Either create or modify the DomeinFile in couchdb. Caches.
-- | Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
-- | If the model is not found in the cache, assumes it is not in the database either.
storeDomeinFileInCouchdb :: DomeinFile -> MonadPerspectives Unit
storeDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) = do
  void $ storeDomeinFileInCache _id df
  saveCachedDomeinFile (DomeinFileId _id)

-- | Remove the file from couchb. Removes the model from cache.
removeDomeinFileFromCouchdb :: Namespace -> MonadPerspectives Unit
removeDomeinFileFromCouchdb ns = removeEntiteit (DomeinFileId ns) *> pure unit

domeinRequest :: MonadPerspectives (AX.Request String)
domeinRequest = do
  username <- getUser
  password <- getCouchdbPassword
  pure { method: Left GET
  , url: "http://localhost:5984/models2model_SysteemDomein_"
  , headers: []
  , content: Nothing
  , username: Just username
  , password: Just password
  , withCredentials: true
  , responseFormat: ResponseFormat.string
  }

-----------------------------------------------------------
-- CASCADING DELETION OF A DOMAINFILE
-----------------------------------------------------------
-- | Delete the DomeinFile from cache and from the local models store.
-- | (First) delete all dependencies.
-- | Notice that this may remove files that are dependencies of other locally stored DomeinFiles!
cascadeDeleteDomeinFile :: DomeinFileId -> MonadPerspectives Unit
cascadeDeleteDomeinFile dfid = do
  df <- tryGetPerspectEntiteit dfid
  case df of
    Just (DomeinFile{referredModels}) -> do
      for_ referredModels cascadeDeleteDomeinFile
      -- One of the dependencies may have removed dfid.
      void $ tryRemoveEntiteit dfid
    otherwise -> pure unit
