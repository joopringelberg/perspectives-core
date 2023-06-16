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

import Control.Monad.Except (throwError)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Effect.Aff.AVar (AVar, put, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (JustInTimeModelLoad(..), MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (DomeinFileName, ModelUri, buitenRol, modelUri2ManifestUrl, modelUri2ModelUrl, modelUriVersion, unversionedModelUri)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.ModelDependencies (versionToInstall)
import Perspectives.Persistence.API (addAttachment, getAttachment, tryGetDocument)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (removeEntiteit, saveEntiteit, tryGetPerspectEntiteit, tryRemoveEntiteit, updateRevision)
import Perspectives.PerspectivesState (domeinCacheRemove, getModelToLoad)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity, retrieveInternally)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Prelude (Unit, bind, discard, identity, pure, unit, void, ($), (*>), (<<<), (<>), (>>=))

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
modifyCalculatedRoleInDomeinFile modelUri cr@(CalculatedRole {_id}) = modifyDomeinFileInCache modifier modelUri *> pure cr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{calculatedRoles}) = DomeinFile dff {calculatedRoles = insert (unwrap _id) cr calculatedRoles}

modifyEnumeratedRoleInDomeinFile :: ModelUri -> EnumeratedRole -> MonadPerspectives EnumeratedRole
modifyEnumeratedRoleInDomeinFile modelUri er@(EnumeratedRole {_id}) = modifyDomeinFileInCache modifier modelUri *> pure er
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{enumeratedRoles}) = DomeinFile dff {enumeratedRoles = insert (unwrap _id) er enumeratedRoles}

modifyCalculatedPropertyInDomeinFile :: ModelUri -> CalculatedProperty -> MonadPerspectives CalculatedProperty
modifyCalculatedPropertyInDomeinFile modelUri cr@(CalculatedProperty {_id}) = modifyDomeinFileInCache modifier modelUri *> pure cr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{calculatedProperties}) = DomeinFile dff {calculatedProperties = insert (unwrap _id) cr calculatedProperties}

modifyStateInDomeinFile :: ModelUri -> State -> MonadPerspectives State
modifyStateInDomeinFile modelUri sr@(State {id}) = modifyDomeinFileInCache modifier modelUri *> pure sr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{states}) = DomeinFile dff {states = insert (unwrap id) sr states}

-----------------------------------------------------------
-- DOMEINFILE PERSISTENCE
-----------------------------------------------------------
-- | Retrieve a domain file. First looks in the cache. If not found, retrieves it from the local models database and caches it.
-- | If not found, tries to fetch the file from its repository and add it to the local models and the cache.
-- | Tries to get the version recommended by the Author and otherwise the version with the highest version number.
-- | If that cannot be established, settles for the document without version.
-- | The modelUri parameter may be bound to a Versioned ModelURI.
retrieveDomeinFile :: ModelUri -> MonadPerspectives DomeinFile
retrieveDomeinFile modelUri = tryGetPerspectEntiteit (DomeinFileId $ unversionedModelUri modelUri) >>= case _ of 
  -- Now retrieve the DomeinFile from a remote repository and store it in the local "models" database of this user.
  Nothing -> do 
    version <- case modelUriVersion modelUri of 
      Nothing -> getVersionToInstall modelUri
      Just v -> pure $ Just v
    modelToLoadAVar <- getModelToLoad
    liftAff $ put (LoadModel (DomeinFileId ((unversionedModelUri modelUri) <> (maybe "" ((<>) "@") version)))) modelToLoadAVar
    result <- liftAff $ take modelToLoadAVar
    -- Now the forking process waits (blocks) untiel retrieveFromDomeinFile fills it with another LoadModel request.
    case result of 
      -- We now take up communication with the forked process that actually loads the model:
      HotLine hotline -> do 
        loadingResult <- liftAff $ take hotline
        case loadingResult of
          -- The stop condition for this recursion is tryGetPerspectEntiteit!
          ModelLoaded -> retrieveDomeinFile modelUri
          LoadingFailed reason -> throwError (error $ "Cannot get " <> modelUri <> " from a repository. Reason: " <> reason)
          _ -> throwError (error $ "Model retrieval from repository was not executed for " <> modelUri <> ".")
      -- This should not happen
      _ -> throwError (error $ "Wrong communication from the forked model loading process!")
  Just df -> pure df

-- | Retrieve a string that is a Semantic Version Number.
-- | It is either the version number designated by the Author of the model to be installed,
-- | or it is the latest version.
-- | If no manifest is found, the empty string is returned. This causes retrieveDomainFile to retrieve a versionless document!
-- | This function must be able to run without any type information, as it is run on system install, too.
getVersionToInstall :: ModelUri -> MonadPerspectives (Maybe String)
getVersionToInstall modelUri = case unsafePartial modelUri2ManifestUrl modelUri of 
  -- Retrieve the property from the external role of the manifest that indicates the version we should install.
  -- To achieve this, we have an Enumerated property that reflects the version to install in the external role of the Manifest.
  -- We retrieve this role as a Couchdb document and read that value directly from its structure.
  {repositoryUrl, manifestName} -> do
    mRol <- tryGetDocument repositoryUrl (buitenRol manifestName)
    case mRol of 
      Just (PerspectRol{properties}) -> case head $ maybe [] identity (lookup versionToInstall properties) of 
        Nothing -> pure Nothing
        Just v -> pure $ Just $ unwrap v
      _ -> pure Nothing

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
  {repositoryUrl, documentName} <- pure $ unsafePartial modelUri2ModelUrl _id
  mAttachment <- getAttachment repositoryUrl documentName "screens.js"
  void $ storeDomeinFileInCache _id df
  (DomeinFile {_rev}) <- saveCachedDomeinFile (DomeinFileId _id)
  case mAttachment of
    Nothing -> pure unit
    Just attachment -> do
      perspect_models <- getSystemIdentifier >>= pure <<< (_ <> "_models")
      void $ addAttachment perspect_models documentName _rev "screens.js" attachment (MediaType "text/ecmascript")
      updateRevision (DomeinFileId documentName)

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
