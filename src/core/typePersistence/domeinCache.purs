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

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, execStateT, get, put) as State
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (AVar, put, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign (Foreign)
import Foreign.Object (Object, empty, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (JustInTimeModelLoad(..), MonadPerspectives, retrieveInternally)
import Perspectives.Couchdb (DeleteCouchdbDocument(..))
import Perspectives.Couchdb.Revision (Revision_)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError, warnModeller)
import Perspectives.Identifiers (DomeinFileName, buitenRol, deconstructBuitenRol, modelUri2ManifestUrl, modelUri2ModelUrl, modelUriVersion, unversionedModelUri)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.ModelDependencies (build, patch, versionToInstall)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (addAttachment, fromBlob, getAttachment, tryGetDocument)
import Perspectives.Persistent (forceSaveDomeinFile, getPerspectRol, removeEntiteit, saveEntiteit, tryGetPerspectEntiteit, tryRemoveEntiteit, updateRevision)
import Perspectives.PerspectivesState (domeinCacheRemove, getModelToLoad, getTranslationTable, setTranslationTable)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity, setRevision, tryReadEntiteitFromCache)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value)
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.ResourceIdentifiers (resourceIdentifier2DocLocator)
import Perspectives.Warning (PerspectivesWarning(..))
import Prelude (Unit, bind, discard, identity, map, pure, unit, void, ($), (*>), (<$>), (<<<), (<>), (>>=))
import Simple.JSON (readJSON)

storeDomeinFileInCache :: DomeinFileId -> DomeinFile -> MonadPerspectives (AVar DomeinFile)
storeDomeinFileInCache id df= cacheEntity id df

removeDomeinFileFromCache :: DomeinFileId -> MonadPerspectives Unit
removeDomeinFileFromCache = void <<< domeinCacheRemove <<< unwrap

-- | Change the domeinfile in cache. NOTA BENE: does not store the modified file in Couchdb!
modifyDomeinFileInCache :: (DomeinFile -> DomeinFile) -> DomeinFileId -> MonadPerspectives Unit
modifyDomeinFileInCache modifier domeinFileId@(DomeinFileId modelUri) =
  do
    mAvar <- retrieveInternally domeinFileId
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
modifyCalculatedRoleInDomeinFile :: DomeinFileId -> CalculatedRole -> MonadPerspectives CalculatedRole
modifyCalculatedRoleInDomeinFile modelUri cr@(CalculatedRole {id}) = modifyDomeinFileInCache modifier modelUri *> pure cr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{calculatedRoles}) = DomeinFile dff {calculatedRoles = insert (unwrap id) cr calculatedRoles}

modifyEnumeratedRoleInDomeinFile :: DomeinFileId -> EnumeratedRole -> MonadPerspectives EnumeratedRole
modifyEnumeratedRoleInDomeinFile modelUri er@(EnumeratedRole {id}) = modifyDomeinFileInCache modifier modelUri *> pure er
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{enumeratedRoles}) = DomeinFile dff {enumeratedRoles = insert (unwrap id) er enumeratedRoles}

modifyCalculatedPropertyInDomeinFile :: DomeinFileId -> CalculatedProperty -> MonadPerspectives CalculatedProperty
modifyCalculatedPropertyInDomeinFile modelUri cr@(CalculatedProperty {id}) = modifyDomeinFileInCache modifier modelUri *> pure cr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{calculatedProperties}) = DomeinFile dff {calculatedProperties = insert (unwrap id) cr calculatedProperties}

modifyStateInDomeinFile :: DomeinFileId -> State -> MonadPerspectives State
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
-- | Also loads the translations table for the namespace of the DomeinFile.
-- | The modelUri parameter may be bound to a Versioned ModelURI.
retrieveDomeinFile :: DomeinFileId -> MonadPerspectives DomeinFile
retrieveDomeinFile domeinFileId@(DomeinFileId modelUri) = do
  mdf <- tryReadEntiteitFromCache domeinFileId
  case mdf of
    -- The DomeinFile is in the cache, meaning we have loaded the translations table before.
    Just df -> pure df
    -- The DomeinFile is not in the cache, so we have to load the translations table.
    Nothing -> tryGetPerspectEntiteit (DomeinFileId $ unversionedModelUri modelUri) >>= case _ of 
      -- the DomeinFile is not available in the local database. Retrieve it from a remote repository and store it in the local "models" database of this user.
      Nothing -> do 
        version <- case modelUriVersion modelUri of 
          Nothing -> map _.semver <$> getVersionToInstall domeinFileId
          Just v -> pure $ Just v
        modelToLoadAVar <- getModelToLoad
        liftAff $ put (LoadModel (DomeinFileId ((unversionedModelUri modelUri) <> (maybe "" ((<>) "@") version)))) modelToLoadAVar
        result <- liftAff $ take modelToLoadAVar
        -- Now the forking process waits (blocks) until retrieveFromDomeinFile fills it with another LoadModel request.
        case result of 
          -- We now take up communication with the forked process that actually loads the model:
          HotLine hotline -> do 
            loadingResult <- liftAff $ take hotline
            case loadingResult of
              -- The stop condition for this recursion is tryGetPerspectEntiteit!
              -- It will now find the model in the local database (but not yet in cache)
              -- the recursive call will then load the translations table.
              ModelLoaded -> retrieveDomeinFile domeinFileId
              LoadingFailed reason -> throwError (error $ "Cannot get " <> modelUri <> " from a repository. Reason: " <> reason)
              _ -> throwError (error $ "Model retrieval from repository was not executed for " <> modelUri <> ".")
          -- This should not happen
          _ -> throwError (error $ "Wrong communication from the forked model loading process!")
      -- The model was available in the local database, but as it was not in cache, we have to load the translations.
      Just df -> fetchTranslations' df

  where
    fetchTranslations' :: DomeinFile -> MonadPerspectives DomeinFile
    fetchTranslations' df = do
      fetchTranslations (identifier df)
      pure df

fetchTranslations :: DomeinFileId -> MonadPerspectives Unit
fetchTranslations (DomeinFileId namespace) = do
  mtranslations <- getTranslationTable namespace
  case mtranslations of 
    Just translations -> pure unit
    Nothing -> do 
      -- Retrieve the translations and store in state.
      {database, documentName} <- resourceIdentifier2DocLocator $ unversionedModelUri namespace
      mtranslationsFromDb <- getAttachment database documentName "translationtable.json"
      case mtranslationsFromDb of 
        Nothing -> warnModeller (NoTranslations namespace)
        Just f -> do 
          x <- liftAff $ fromBlob f
          case readJSON x of
            Left e -> logPerspectivesError (Custom $ "retrieveDomeinFile. Cannot parse translations table: " <> show e)
            Right translations -> setTranslationTable namespace translations

-- | Retrieves a string that is a Semantic Version Number. Also returns the external role of the VersionedModelManifest.
-- | The version is either the version number designated by the Author of the model to be installed,
-- | or it is the latest version.
-- | If no manifest is found, the empty string is returned. This causes retrieveDomainFile to retrieve a versionless document!
-- | This function must be able to run without any type information, as it is run on system install, too.
-- | The modelURI should not include a version.
getVersionToInstall :: DomeinFileId -> MonadPerspectives (Maybe {semver :: String, versionedModelManifest :: RoleInstance})
getVersionToInstall (DomeinFileId modelUri) = case unsafePartial modelUri2ManifestUrl modelUri of 
  -- Retrieve the property from the external role of the manifest that indicates the version we should install.
  -- To achieve this, we have an Enumerated property that reflects the version to install in the external role of the Manifest.
  -- We retrieve this role as a Couchdb document and read that value directly from its structure.
  {repositoryUrl, manifestName} -> do
    mRol <- tryGetDocument repositoryUrl (buitenRol manifestName)
    case mRol of 
      Just (PerspectRol{id, properties}) -> case head $ maybe [] identity (lookup versionToInstall properties) of 
        Nothing -> pure Nothing
        -- TODO dit klopt niet, want dit is de externe rol van het ModelManifest zelf. We willen het VersionedModelManifest.
        Just v -> pure $ Just {semver: unwrap v, versionedModelManifest: makeVersionedModelManifest (unwrap v) id}
      _ -> pure Nothing
  where
    makeVersionedModelManifest :: String -> RoleInstance -> RoleInstance
    makeVersionedModelManifest semver (RoleInstance modelManifest) = RoleInstance $ buitenRol ((deconstructBuitenRol modelManifest) <> "@" <> semver)

getPatchAndBuild :: RoleInstance -> MonadPerspectives {patch :: String, build :: String}
getPatchAndBuild rid = do
  (PerspectRol{id, properties}) <- getPerspectRol rid
  case lookup patch properties, lookup build properties of
    Just p, Just b -> pure {patch: firstOrDefault p, build: firstOrDefault b}
    Just p, Nothing -> pure {patch: firstOrDefault p, build: default}
    Nothing, Just b -> pure {patch: default, build: firstOrDefault b}
    Nothing, Nothing -> pure {patch: default, build: default}
  where 
    default :: String
    default = "0"
    firstOrDefault :: Array Value -> String
    firstOrDefault = maybe default unwrap <<< head
-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

-- NOTE: the revision of the returned DomeinFile will be outdated!
saveCachedDomeinFile :: DomeinFileId -> MonadPerspectives DomeinFile
saveCachedDomeinFile dfid = do
  updateRevision dfid
  saveEntiteit dfid

-- | Either create or modify the DomeinFile in couchdb. Caches.
-- | Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
-- | If the model is not found in the cache, assumes it is not in the database either.
storeDomeinFileInCouchdb :: DomeinFile -> MonadPerspectives DomeinFile
storeDomeinFileInCouchdb df@(DomeinFile dfr@{id}) = do
  void $ storeDomeinFileInCache id df
  saveCachedDomeinFile id

-- | Guarantees a correct revision.
storeDomeinFileInCouchdbPreservingAttachments :: DomeinFile -> MonadPerspectives Unit
storeDomeinFileInCouchdbPreservingAttachments df@(DomeinFile dfr@{id, _attachments}) = do
  {repositoryUrl, documentName} <- pure $ unsafePartial modelUri2ModelUrl (unwrap id)
  attachments <- case _attachments of
    Nothing -> pure empty
    Just atts ->  traverseWithIndex
      (\attName {content_type} -> Tuple (MediaType content_type) <$> getAttachment repositoryUrl documentName attName)
      atts
  void $ storeDomeinFileInCache id df
  (DomeinFile {_rev}) <- saveCachedDomeinFile id
  -- Unless we actually store in the database, adding attachments will go wrong.
  forceSaveDomeinFile id
  newRev <- State.execStateT (addAttachments repositoryUrl documentName attachments) _rev
  setRevision id newRev
  
-- As each attachment that we add will bump the document version, we have to catch it and use it on the
-- next attachment.
addAttachments :: String -> String -> Object (Tuple MediaType (Maybe Foreign)) -> State.StateT Revision_ MonadPerspectives Unit
addAttachments documentUrl documentName attachments = do 
  forWithIndex_
    attachments 
    (\attName (Tuple mimetype mattachment) -> do 
      case mattachment of
        Nothing -> pure unit
        Just attachment -> do
          newRev <- State.get
          DeleteCouchdbDocument {rev} <- lift $ addAttachment documentUrl documentName newRev attName attachment mimetype
          State.put rev)

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
