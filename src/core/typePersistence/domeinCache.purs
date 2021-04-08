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
import Effect.Aff.AVar (AVar, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Object (insert)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.Identifiers (Namespace)
import Perspectives.Persistence.API (addAttachment, getAttachment)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (getPerspectEntiteit, removeEntiteit, saveEntiteit, tryGetPerspectEntiteit, tryRemoveEntiteit, updateRevision)
import Perspectives.PerspectivesState (domeinCacheRemove)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity, retrieveInternally)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (*>), (<<<), (<>), (<$>), (>>=))

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
-- MODIFY ELEMENTS OF A DOMEINFILE
-----------------------------------------------------------
modifyCalculatedRoleInDomeinFile :: Namespace -> CalculatedRole -> MonadPerspectives CalculatedRole
modifyCalculatedRoleInDomeinFile ns cr@(CalculatedRole {_id}) = modifyDomeinFileInCache modifier ns *> pure cr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{calculatedRoles}) = DomeinFile dff {calculatedRoles = insert (unwrap _id) cr calculatedRoles}

modifyCalculatedPropertyInDomeinFile :: Namespace -> CalculatedProperty -> MonadPerspectives CalculatedProperty
modifyCalculatedPropertyInDomeinFile ns cr@(CalculatedProperty {_id}) = modifyDomeinFileInCache modifier ns *> pure cr
  where
    modifier :: DomeinFile -> DomeinFile
    modifier (DomeinFile dff@{calculatedProperties}) = DomeinFile dff {calculatedProperties = insert (unwrap _id) cr calculatedProperties}

-----------------------------------------------------------
-- RETRIEVE A DOMAINFILE
-----------------------------------------------------------
-- | Retrieve a domain file. First looks in the cache. If not found, retrieves it from the database and caches it.
retrieveDomeinFile :: Namespace -> MonadPerspectives DomeinFile
retrieveDomeinFile ns = getPerspectEntiteit (DomeinFileId ns)

tryRetrieveDomeinFile :: Namespace -> MonadPerspectives (Maybe DomeinFile)
tryRetrieveDomeinFile id = catchError (Just <$> (getPerspectEntiteit (DomeinFileId id)))
  \_ -> pure Nothing

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

saveCachedDomeinFile :: DomeinFileId -> MonadPerspectives DomeinFile
saveCachedDomeinFile ns = do
  updateRevision ns
  saveEntiteit ns

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
removeDomeinFileFromCouchdb :: Namespace -> MonadPerspectives Unit
removeDomeinFileFromCouchdb ns = removeEntiteit (DomeinFileId ns) *> pure unit

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
