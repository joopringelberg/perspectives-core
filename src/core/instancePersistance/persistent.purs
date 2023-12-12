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
  ( entitiesDatabaseName
  , entityExists
  , fetchEntiteit
  , getDomeinFile
  , getPerspectContext
  , getPerspectEntiteit
  , getPerspectRol
  , modelDatabaseName
  , postDatabaseName
  , removeEntiteit
  , saveCachedEntiteit
  , saveEntiteit
  , saveEntiteit_
  , saveMarkedResources
  , tryFetchEntiteit
  , tryGetPerspectEntiteit
  , tryRemoveEntiteit
  , updateRevision
  )
  where

import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Except (catchError, lift, throwError)
import Data.Array (cons, elemIndex)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Aff.AVar (AVar, kill, put, read)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Persistence.Attachment (class Attachment)
import Perspectives.CoreTypes (class Persistent, MP, MonadPerspectives, ResourceToBeStored(..), addPublicResource, dbLocalName, removeInternally, representInternally, resourceToBeStored, retrieveInternally)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (AuthoritySource(..), MonadPouchdb, addDocument, deleteDocument, ensureAuthentication, getDocument, retrieveDocumentVersion)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Representation.Class.Cacheable (class Revision, Revision_, cacheEntity, changeRevision, readEntiteitFromCache, rev, setRevision, takeEntiteitFromCache)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.ResourceIdentifiers (isInPublicScheme, resourceIdentifier2DocLocator, resourceIdentifier2WriteDocLocator)
import Simple.JSON (class WriteForeign)

getPerspectEntiteit :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives a
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ read avar
        pure pe
      Nothing -> do
        if isInPublicScheme (unwrap id)
          then addPublicResource id
          else pure unit
        fetchEntiteit id

entitiesDatabaseName :: forall f. MonadPouchdb f String
entitiesDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_entities")

postDatabaseName :: forall f. MonadPouchdb f String
postDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_post")

modelDatabaseName :: MonadPerspectives String
modelDatabaseName = dbLocalName (DomeinFileId "model://perspectives.domains#System")

getPerspectContext :: ContextInstance -> MP PerspectContext
getPerspectContext = getPerspectEntiteit

getPerspectRol :: RoleInstance -> MP PerspectRol
getPerspectRol = getPerspectEntiteit

-- | Argument should have form model:ModelName (not the new model:some.domain/ModelName form).
getDomeinFile :: DomeinFileId -> MP DomeinFile
getDomeinFile = getPerspectEntiteit

tryGetPerspectEntiteit :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives (Maybe a)
tryGetPerspectEntiteit id = catchError ((getPerspectEntiteit id) >>= (pure <<< Just))
  \e -> do 
    -- logPerspectivesError (Custom $ show e)
    pure Nothing

entityExists :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives Boolean
entityExists id = catchError ((getPerspectEntiteit id) >>= (pure <<< const true))
  \e ->  do 
    -- logPerspectivesError (Custom $ show e)
    pure false

-- | Remove from Couchdb if possible and remove from the cache, too.
removeEntiteit :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives a
removeEntiteit entId = do
  entiteit <- getPerspectEntiteit entId
  removeEntiteit_ entId entiteit

removeEntiteit_ :: forall a i. Persistent a i => i -> a -> MonadPerspectives a
removeEntiteit_ entId entiteit =
  ensureAuthentication (Resource $ unwrap entId) $ \_ ->
    case (rev entiteit) of
      Nothing -> pure entiteit
      (Just rev) -> do
        void $ removeInternally entId

        -- TODO: DIT IS DE NIEUWE STIJL RESOURCE IDENTIFIER
        {database, documentName} <- resourceIdentifier2WriteDocLocator (unwrap entId)
        void $ deleteDocument database documentName (Just rev)
        pure entiteit

tryRemoveEntiteit :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives Unit
tryRemoveEntiteit entId = do
  mentiteit <- tryGetPerspectEntiteit entId
  case mentiteit of
    Nothing -> pure unit
    Just entiteit -> void $ removeEntiteit_ entId entiteit

-- | Fetch the definition of a resource asynchronously. It will have the same version in cache as in Couchdb.
fetchEntiteit :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives a
fetchEntiteit id = ensureAuthentication (Resource $ unwrap id) $ \_ -> catchError
  do
    v <- representInternally id

    {database, documentName} <- resourceIdentifier2DocLocator (unwrap id)
    doc <- getDocument database documentName

    -- Returns either the local database name or a URL.
    -- dbName <- dbLocalName id
    -- doc <- getDocument dbName (couchdbResourceIdentifier $ unwrap id)
    lift $ put doc v
    pure doc

  \e -> do
    (mav :: Maybe (AVar a)) <- removeInternally id
    case mav of
      Nothing -> pure unit
      Just av -> liftAff $ kill (error ("Cound not find " <> unwrap id)) av
    if test decodingErrorRegex (show e)
      then logPerspectivesError (Custom ("fetchEntiteit: failed to retrieve resource " <> unwrap id <> " from couchdb. " <> show e))
      else pure unit
    throwError $ error ("fetchEntiteit: failed to retrieve resource " <> unwrap id <> " from couchdb. " <> show e)
  where
    decodingErrorRegex :: Regex
    decodingErrorRegex = unsafeRegex "error in decoding result" noFlags


-- | Fetch the definition of a document if it can be found. DOES NOT CACHE THE ENTITY!
tryFetchEntiteit :: forall a i. Attachment a => Revision a => Persistent a i => i -> MonadPerspectives (Maybe a)
tryFetchEntiteit id = do
  {database, documentName} <- resourceIdentifier2DocLocator (unwrap id)
  catchError (Just <$> getDocument database documentName)
    \e -> pure Nothing

  -- dbName <- dbLocalName id
  -- catchError (Just <$> getDocument dbName (couchdbResourceIdentifier $ unwrap id))
  --   \e -> pure Nothing

-- | Saves a previously cached entity.
-- | NOTE: the entity may not be saved immediately, due to the scheme of periodic saving.
saveEntiteit :: forall a i. Attachment a => WriteForeign a => Persistent a i => i -> MonadPerspectives a
saveEntiteit id = saveEntiteit__ id Nothing

-- | Saves the given entity to the database and puts it, with a correct (new) revision,
-- | in the cache, assuming the current revision of the entiteit equals that in the database.
-- | NOTE: the entity may not be saved immediately, due to the scheme of periodic saving.
saveEntiteit_ :: forall a i. Attachment a => WriteForeign a => Persistent a i => i -> a -> MonadPerspectives a
saveEntiteit_ entId entiteit = saveEntiteit__ entId (Just entiteit)

-- | Ensures that the entity is in cache and schedules it for saving to the database.
saveEntiteit__ :: forall a i. Attachment a => WriteForeign a => Persistent a i => i -> Maybe a -> MonadPerspectives a
saveEntiteit__ entId mentiteit = do
  case mentiteit of 
    -- In this case, we might have an entity in cache. As we want to return the entity,
    -- try to retrieve it. If not present, fails with an error.
    Nothing -> do
      a <- readEntiteitFromCache entId 
      modify \s@{entitiesToBeStored} -> if isJust $ elemIndex (resourceToBeStored a) entitiesToBeStored
        then s
        else s { entitiesToBeStored = cons (resourceToBeStored a) entitiesToBeStored}
      -- The version might be updated in the caching process.
      pure a
    Just a -> do
    -- In this case, we want to replace the entity in cache with the one passed in as the second argument
    -- and schedule it to be saved to the database.
      a' <- cacheEntity entId a
      -- Now add to the items to be saved.
      modify \s@{entitiesToBeStored} -> if isJust $ elemIndex (resourceToBeStored a) entitiesToBeStored
        then s
        else s { entitiesToBeStored = cons (resourceToBeStored a) entitiesToBeStored}
      -- The version might be updated in the caching process.
      liftAff $ read a'

saveMarkedResources :: MonadPerspectives Unit
saveMarkedResources = do 
  (toBeSaved :: Array ResourceToBeStored) <- gets _.entitiesToBeStored
  modify \s -> s {entitiesToBeStored = []}
  for_ toBeSaved \(rs :: ResourceToBeStored) -> case rs of 
    Ctxt c -> void $ saveCachedEntiteit (c :: ContextInstance) 
    Rle r -> void $ saveCachedEntiteit (r :: RoleInstance)
    Dfile d -> void $ saveCachedEntiteit (d :: DomeinFileId) 

-- | Assumes the entity a has been cached. 
saveCachedEntiteit :: forall a i. Attachment a => WriteForeign a => Persistent a i => i -> MonadPerspectives a
saveCachedEntiteit entId = do 
  entiteit <- takeEntiteitFromCache entId
  {database, documentName} <- resourceIdentifier2WriteDocLocator (unwrap $ identifier entiteit)
  (rev :: Revision_) <- addDocument database entiteit documentName
  
  -- -- couchdbResourceIdentifier is either a local identifier in the model:User namespace, or a segmented name (in the case of a public resource).
  -- (rev :: Revision_) <- addDocument dbName entiteit (couchdbResourceIdentifier $ unwrap entId)
  entiteit' <- pure (changeRevision rev entiteit)
  void $ cacheEntity (identifier entiteit) entiteit'
  pure entiteit'

-- | Updates the revision in cache (no change to the version in database).
-- | Version is taken from the local models database, not from the repository!
updateRevision :: forall a i. Persistent a i => i -> MonadPerspectives Unit
updateRevision entId = do
  {database, documentName} <- resourceIdentifier2WriteDocLocator (unwrap entId)
  revision <- retrieveDocumentVersion database documentName
  setRevision entId revision 
