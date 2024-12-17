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
  ( addAttachment
  , entitiesDatabaseName
  , entityExists
  , fetchEntiteit
  , getDomeinFile
  , getPerspectContext
  , getPerspectEntiteit
  , getPerspectRol
  , invertedQueryDatabaseName
  , modelDatabaseName
  , postDatabaseName
  , removeEntiteit
  , saveCachedEntiteit
  , saveEntiteit
  , saveEntiteit_
  , saveMarkedResources
  , forceSaveRole
  , forceSaveDomeinFile
  , forceSaveContext
  , tryGetPerspectContext
  , tryGetPerspectEntiteit
  , tryGetPerspectRol
  , tryRemoveEntiteit
  , updateRevision
  )
  where

import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (catchError, lift, throwError)
import Data.Array (cons, delete, elemIndex)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Aff.AVar (AVar, put, read, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Persistence.Attachment (class Attachment)
import Perspectives.CoreTypes (class Persistent, IntegrityFix(..), MP, MonadPerspectives, ResourceToBeStored(..), addPublicResource, dbLocalName, removeInternally, representInternally, resourceToBeStored, retrieveInternally, typeOfInstance)
import Perspectives.Couchdb (DeleteCouchdbDocument(..))
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (AttachmentName, AuthoritySource(..), MonadPouchdb, addDocument, deleteDocument, ensureAuthentication, getDocument, retrieveDocumentVersion)
import Perspectives.Persistence.API (addAttachment) as P
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.PerspectivesState (getMissingResource)
import Perspectives.Representation.Class.Cacheable (Revision_, cacheEntity, changeRevision, readEntiteitFromCache, rev, setRevision, takeEntiteitFromCache, tryReadEntiteitFromCache)
import Perspectives.Representation.Class.Identifiable (identifier, identifier_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.ResourceIdentifiers (isInPublicScheme, resourceIdentifier2DocLocator, resourceIdentifier2WriteDocLocator)
import Simple.JSON (class WriteForeign)
 
fix :: Boolean
fix = true

doNotFix :: Boolean
doNotFix = false

getPerspectEntiteit :: forall a i. Attachment a => Persistent a i => Boolean -> i -> MonadPerspectives a
getPerspectEntiteit tryToFix id =
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
        fetchEntiteit tryToFix id

entitiesDatabaseName :: forall f. MonadPouchdb f String
entitiesDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_entities")

postDatabaseName :: forall f. MonadPouchdb f String
postDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_post")

modelDatabaseName :: MonadPerspectives String
modelDatabaseName = dbLocalName (DomeinFileId "model://perspectives.domains#System")

invertedQueryDatabaseName :: MonadPerspectives String
invertedQueryDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_invertedqueries")

getPerspectContext :: ContextInstance -> MP PerspectContext
getPerspectContext = getPerspectEntiteit fix

tryGetPerspectContext :: ContextInstance -> MonadPerspectives (Maybe PerspectContext)
tryGetPerspectContext id = tryGetPerspectEntiteit id

tryGetPerspectRol :: RoleInstance -> MonadPerspectives (Maybe  PerspectRol)
tryGetPerspectRol id = tryGetPerspectEntiteit id

getPerspectRol :: RoleInstance -> MP PerspectRol
getPerspectRol = getPerspectEntiteit fix

-- | Argument should have form model:ModelName (not the new model:some.domain/ModelName form).
getDomeinFile :: DomeinFileId -> MP DomeinFile
getDomeinFile = getPerspectEntiteit doNotFix

tryGetPerspectEntiteit :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives (Maybe a)
tryGetPerspectEntiteit id = catchError ((getPerspectEntiteit doNotFix id) >>= (pure <<< Just))
  \e -> do 
    -- logPerspectivesError (Custom $ show e)
    pure Nothing

entityExists :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives Boolean
entityExists id = catchError ((getPerspectEntiteit doNotFix id) >>= (pure <<< const true))
  \e ->  do 
    -- logPerspectivesError (Custom $ show e)
    pure false

-- | Remove from Couchdb if possible and remove from the cache, too.
removeEntiteit :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives a
removeEntiteit entId = do
  entiteit <- getPerspectEntiteit fix entId
  removeEntiteit_ entId entiteit

removeEntiteit_ :: forall a i. Persistent a i => i -> a -> MonadPerspectives a
removeEntiteit_ entId entiteit =
  ensureAuthentication (Resource $ unwrap entId) $ \_ -> do
    -- If on the list of items to be saved, remove!
    modify \s@{entitiesToBeStored} -> s { entitiesToBeStored = delete (resourceToBeStored entiteit) entitiesToBeStored}
    case (rev entiteit) of
      Nothing -> removeInternally entId *> pure entiteit
      (Just rev) -> do
        void $ removeInternally entId
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
-- | This function must only be called when there is no AVar in cache to represent the resource.
-- | As an invariant side effect: there is either an AVar that holds the resource, or there is no AVar.
fetchEntiteit :: forall a i. Attachment a => Persistent a i => Boolean -> i -> MonadPerspectives a
fetchEntiteit tryToFix id = ensureAuthentication (Resource $ unwrap id) $ \_ -> 
  catchError
    do
      {database, documentName} <- resourceIdentifier2DocLocator (unwrap id)
      doc <- getDocument database documentName
      -- Returns either the local database name or a URL.
      v <- representInternally id
      lift $ put doc v
      pure doc

    \e -> do
      if test decodingErrorRegex (show e)
        then logPerspectivesError (Custom ("fetchEntiteit: failed to decode resource " <> unwrap id <> ". Parser message: " <> show e))
        -- Try to fix referential integrity
        else if tryToFix
          then do 
            logPerspectivesError 
              (Custom ("fetchEntiteit: failed to retrieve resource " <> unwrap id <> " from couchdb: " <> show e))
            missingResourceAVar <- getMissingResource
            liftAff $ put (Missing $ typeOfInstance id) missingResourceAVar
            response <- liftAff $ take missingResourceAVar
            case response of 
              FixingHotLine av -> do 
                result <- liftAff $ take av
                case result of 
                  FixFailed s -> logPerspectivesError (Custom "fetchEntiteit: cannot fix references.")
                  FixSucceeded -> logPerspectivesError (Custom "fetchEntiteit: succesfully removed all references to missing resource.")
                  _ -> logPerspectivesError (Custom "fetchEntiteit: received unexpected message from fixer.")
              _ -> logPerspectivesError (Custom "fetchEntiteit: received unexpected message from fixer.")
          else pure unit
      throwError $ error ("fetchEntiteit: failed to retrieve resource " <> unwrap id <> " from couchdb.")
  where
    decodingErrorRegex :: Regex
    decodingErrorRegex = unsafeRegex "error in decoding result" noFlags

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
-- | This is the only function that adds to `entitiesToBeStored`.
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

forceSaveRole :: RoleInstance -> MonadPerspectives Unit
forceSaveRole rid = forceSaveEntiteit (Rle rid) rid

forceSaveContext :: ContextInstance -> MonadPerspectives Unit
forceSaveContext cid = forceSaveEntiteit (Ctxt cid) cid

forceSaveDomeinFile :: DomeinFileId -> MonadPerspectives Unit
forceSaveDomeinFile dfid = forceSaveEntiteit (Dfile dfid) dfid

-- | Guarantees that
-- | There is no entity in cache or if it is, it is equal to the entity in the database, and
-- | The entity is not waiting to be saved.
forceSaveEntiteit :: forall a i. Attachment a => WriteForeign a => Persistent a i => ResourceToBeStored -> i -> MonadPerspectives Unit
forceSaveEntiteit r entId = do 
  mentiteit <- tryReadEntiteitFromCache entId
  case mentiteit of 
    Nothing -> modify \s -> s {entitiesToBeStored = delete r s.entitiesToBeStored}
    Just entiteit -> void $ saveCachedEntiteit r entId
  

-- | All items will be removed from `entitiesToBeStored`, whether succesfully stored or not..
saveMarkedResources :: MonadPerspectives Unit
saveMarkedResources = do 
  (toBeSaved :: Array ResourceToBeStored) <- gets _.entitiesToBeStored
  for_ toBeSaved \(rs :: ResourceToBeStored) -> try (case rs of 
    Ctxt c -> void $ saveCachedEntiteit rs (c :: ContextInstance)
    Rle r -> void $ saveCachedEntiteit rs (r :: RoleInstance)
    Dfile d -> void $ saveCachedEntiteit rs (d :: DomeinFileId)) >>= case _ of 
      Left e -> logPerspectivesError (Custom ("Could not save resource " <> show rs <> " because: " <> show e)) 
      _ -> pure unit

-- | Assumes the entity a has been cached. 
-- | In case saving the resource fails, the cache is left intact.
-- | If the entity is not in cache, it remains in the queue waiting to be saved (it may arrive from a peer).
-- | This is the only function that removes items from `entitiesToBeStored`.
saveCachedEntiteit :: forall a i. Attachment a => WriteForeign a => Persistent a i => ResourceToBeStored -> i -> MonadPerspectives a
saveCachedEntiteit r entId = do 
  entiteit <- takeEntiteitFromCache entId
  -- The cache is now blocked, so there is no way to modify the entity. It may be decached; but we have the modified entity in our hands, here.
  modify \s -> s {entitiesToBeStored = delete r s.entitiesToBeStored}
  {database, documentName} <- resourceIdentifier2WriteDocLocator (unwrap $ identifier entiteit)
  mresult <- try $ addDocument database entiteit documentName
  case mresult of 
    -- Restore the avar holding the resource to its filled state, to prevent the main fiber from blocking.
    -- Notice we do not put it back into entitiesToBeStared.
    Left e -> cacheEntity (identifier entiteit) entiteit *> pure entiteit
    Right (rev :: Revision_) -> do 
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

-----------------------------------------------------------
-- ADDATTACHMENT
-----------------------------------------------------------
-- | Requires a revision if the document exists prior to adding the attachment.
-- | Side effects:
-- |    * all cached resources that are marked for storing in the database are actually stored;
-- |    * the resource is removed from its cache.
-- | The revision of the document changes if an attachment is added succesfully. By removing it from the cache,
-- | we make sure to take that into account.
addAttachment :: forall a i attachmentType. Attachment a => Persistent a i =>  i -> AttachmentName -> attachmentType -> MediaType -> MonadPerspectives Boolean
addAttachment i attachmentName attachment mimetype = do
  saveMarkedResources
  -- The resource identified by i is now in Couchdb and the revision number in cache equals that in couchdb.
  a :: a <- getPerspectEntiteit fix i
  {database, documentName} <- resourceIdentifier2DocLocator (identifier_ a)
  DeleteCouchdbDocument {ok} <- P.addAttachment database documentName (rev a) attachmentName attachment mimetype
  -- The document in Couchdb now has a higher revision (unless the operation failed, which throws an exception not caught here).
  -- Remove the document from the cache, so it will be retrieved again before it can be used - including the new revision and attachments.
  void $ removeInternally i
  pure $ maybe false identity ok

