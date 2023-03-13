-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.DomeinCache
-- ( retrieveDomeinResourceDefinition )

where

import Control.Monad.Except (catchError, throwError)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Effect.Aff.AVar (AVar, put, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Object (insert)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (DomeinFileName, ModelUri)
import Perspectives.Persistence.API (addAttachment, getAttachment)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (getPerspectEntiteit, removeEntiteit, saveEntiteit, tryGetPerspectEntiteit, tryRemoveEntiteit, updateRevision)
import Perspectives.PerspectivesState (domeinCacheRemove)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity, retrieveInternally)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Prelude (Unit, bind, discard, pure, unit, void, ($), (*>), (<<<), (<>), (<$>), (>>=))

storeDomeinFileInCache :: DomeinFileName -> DomeinFile -> MonadPerspectives (AVar DomeinFile)
storeDomeinFileInCache domeinFileName df= cacheEntity (DomeinFileId domeinFileName) df

removeDomeinFileFromCache :: DomeinFileName -> MonadPerspectives Unit
removeDomeinFileFromCache = void <<< domeinCacheRemove

-- | Change the domeinfile in cache. NOTA BENE: does not store the modified file in Couchdb!
modifyDomeinFileInCache :: (DomeinFile -> DomeinFile) -> ModelUri -> MonadPerspectives Unit
modifyDomeinFileInCache modifier modelUri =
  do
    mAvar <- retrieveInternally (DomeinFileId modelUri)
    case mAvar of
      Nothing -> throwError $ error $ "modifyDomeinFileInCache cannot find domeinfile in cache: " <> modelUri
      (Just avar) -> do
        df <- liftAff $ take avar
        -- Because we modify the existing Entiteit, we do not overwrite the version number -
        -- unless that is what our modifier does.
        liftAff $ put (modifier df) avar

-----------------------------------------------------------
-- MODIFY ELEMENTS OF A DOMEINFILE
-----------------------------------------------------------
modifyCalculatedRoleInDomeinFile :: ModelUri -> CalculatedRole -> MonadPerspectives CalculatedRole
modifyCalculatedRoleInDomeinFile modeluri cr@(CalculatedRole {_id}) = modifyDomeinFileInCache modifier modeluri *> pure cr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{calculatedRoles}) = DomeinFile dff {calculatedRoles = insert (unwrap _id) cr calculatedRoles}

modifyEnumeratedRoleInDomeinFile :: ModelUri -> EnumeratedRole -> MonadPerspectives EnumeratedRole
modifyEnumeratedRoleInDomeinFile modeluri er@(EnumeratedRole {_id}) = modifyDomeinFileInCache modifier modeluri *> pure er
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{enumeratedRoles}) = DomeinFile dff {enumeratedRoles = insert (unwrap _id) er enumeratedRoles}

modifyCalculatedPropertyInDomeinFile :: ModelUri -> CalculatedProperty -> MonadPerspectives CalculatedProperty
modifyCalculatedPropertyInDomeinFile modeluri cr@(CalculatedProperty {_id}) = modifyDomeinFileInCache modifier modeluri *> pure cr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{calculatedProperties}) = DomeinFile dff {calculatedProperties = insert (unwrap _id) cr calculatedProperties}

modifyStateInDomeinFile :: ModelUri -> State -> MonadPerspectives State
modifyStateInDomeinFile modeluri sr@(State {id}) = modifyDomeinFileInCache modifier modeluri *> pure sr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{states}) = DomeinFile dff {states = insert (unwrap id) sr states}

-----------------------------------------------------------
-- DOMEINFILE PERSISTENCE
-----------------------------------------------------------
-- | Retrieve a domain file. First looks in the cache. If not found, retrieves it from the local models database and caches it.

-- | Retrieve a domain file.
-- | Searches the cache with the local model name.
-- | If not found, tries to fetch the file from its repository and add it to the local models and the cache.
retrieveDomeinFile :: ModelUri -> MonadPerspectives DomeinFile
retrieveDomeinFile modeluri = tryGetPerspectEntiteit (DomeinFileId modeluri) >>= case _ of 
  -- Now retrieve the DomeinFile from a remote repository and store it in the local "models" database of this user.
  -- Nothing -> addModelToLocalStore' modeluri true
  Nothing -> throwError (error $ "Unknown model " <> modeluri)
  Just df -> pure df

tryRetrieveDomeinFile :: DomeinFileId -> MonadPerspectives (Maybe DomeinFile)
tryRetrieveDomeinFile dfId = catchError (Just <$> (getPerspectEntiteit dfId))
  \_ -> pure Nothing

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

saveCachedDomeinFile :: DomeinFileId -> MonadPerspectives DomeinFile
saveCachedDomeinFile dfid = do
  updateRevision dfid
  saveEntiteit dfid

-- | Either create or modify the DomeinFile in couchdb. Caches.
-- | Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
-- | If the model is not found in the cache, assumes it is not in the database either.
storeDomeinFileInCouchdb :: DomeinFile -> MonadPerspectives DomeinFile
storeDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) = do
  void $ storeDomeinFileInCache _id df
  saveCachedDomeinFile (DomeinFileId _id)

storeDomeinFileInCouchdbPreservingAttachments :: DomeinFile -> MonadPerspectives Unit
storeDomeinFileInCouchdbPreservingAttachments df@(DomeinFile dfr@{_id}) = do
  mattachment <- getDomeinFileScreens df
  void $ storeDomeinFileInCache _id df
  (DomeinFile {_rev}) <- saveCachedDomeinFile (DomeinFileId _id)
  case mattachment of
    Nothing -> pure unit
    Just attachment -> do
      db <- getSystemIdentifier >>= pure <<< (_ <> "_models")
      void $ addAttachment db _id _rev "screens.js" attachment (MediaType "text/ecmascript")
      updateRevision (DomeinFileId _id)


getDomeinFileScreens :: DomeinFile -> MonadPerspectives (Maybe String)
getDomeinFileScreens (DomeinFile {_id}) = do
  user <- getSystemIdentifier
  getAttachment (user <> "_models") _id "screens.js"

-- | Remove the file from couchb. Removes the model from cache.
removeDomeinFileFromCouchdb :: DomeinFileName -> MonadPerspectives Unit
removeDomeinFileFromCouchdb domeinFileName = removeEntiteit (DomeinFileId domeinFileName) *> pure unit

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
