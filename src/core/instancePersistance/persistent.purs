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
  ( class Persistent
  , dbLocalName
  , writeDbName
  , entitiesDatabaseName
  , entityExists
  , fetchEntiteit
  , getDomeinFile
  , getPerspectContext
  , getPerspectEntiteit
  , getPerspectRol
  , modelDatabaseName
  , postDatabaseName
  , removeEntiteit
  , saveEntiteit
  , saveEntiteit_
  , tryFetchEntiteit
  , tryGetPerspectEntiteit
  , tryRemoveEntiteit
  , updateRevision
  )
  where

import Prelude

import Control.Monad.Except (catchError, lift, throwError)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Aff.AVar (AVar, kill, put, read)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic.Class (class GenericEncode)
import Persistence.Attachment (class Attachment)
import Perspectives.CoreTypes (MP, MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (AuthoritySource(..), MonadPouchdb, addDocument, deleteDocument, ensureAuthentication, getDocument, retrieveDocumentVersion)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Representation.Class.Cacheable (class Cacheable, class Revision, Revision_, cacheEntity, changeRevision, removeInternally, representInternally, retrieveInternally, rev, setRevision, tryTakeEntiteitFromCache)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.ResourceIdentifiers (pouchdbDatabaseName, resourceIdentifier2DocLocator, resourceIdentifier2WriteDocLocator, writeUrl)

class (Cacheable v i, Encode v, Decode v) <= Persistent v i | i -> v,  v -> i where
  -- | Either a local database name, or a URL that identifies a database to read from, in some Couchdb installation on the internet.
  dbLocalName :: i -> MP String
  -- | Either a local database name, or a URL that identifies a database to write to, in some Couchdb installation on the internet.
  writeDbName :: i -> MP String

instance persistentInstancePerspectContext :: Persistent PerspectContext ContextInstance where
  dbLocalName (ContextInstance id) = pouchdbDatabaseName id
  -- dbLocalName id = if isUrl (unwrap id) 
  --   then pure $ unsafePartial publicResourceIdentifier2repository_ (unwrap id)
  --   else do
  --     sysId <- getSystemIdentifier
  --     pure $ sysId <> "_entities"
  writeDbName (ContextInstance id) = writeUrl id

instance persistentInstancePerspectRol :: Persistent PerspectRol RoleInstance where
  dbLocalName (RoleInstance id) = pouchdbDatabaseName id
  -- dbLocalName id = if isUrl (unwrap id) 
  --   then pure $ unsafePartial publicResourceIdentifier2repository_ (unwrap id)
  --   else do
  --     sysId <- getSystemIdentifier
  --     pure $ sysId <> "_entities"
  writeDbName (RoleInstance id) = writeUrl id

instance persistentInstanceDomeinFile :: Persistent DomeinFile DomeinFileId where
  dbLocalName (DomeinFileId id) = pouchdbDatabaseName id
  -- dbLocalName _ = do
  --   sysId <- getSystemIdentifier
  --   pure $ sysId <> "_models"
  -- -- When saving a DomeinFile through Persistent, we only save it in the local models database.
  -- -- It's only through uploadToRepository that we write to remote and public databases (Repositories).
  writeDbName (DomeinFileId id) = writeUrl id

getPerspectEntiteit :: forall a i. Attachment a => Persistent a i => i -> MonadPerspectives a
getPerspectEntiteit id =
  do
    (av :: Maybe (AVar a)) <- retrieveInternally id
    case av of
      (Just avar) -> do
        pe <- liftAff $ read avar
        pure pe
      Nothing -> fetchEntiteit id

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

        -- Returns either the local database name or a URL.
        -- dbName <- writeDbName entId
        -- -- couchdbResourceIdentifier is either a local identifier in the model:User namespace, or a segmented name (in the case of a public resource).
        -- void $ deleteDocument dbName (couchdbResourceIdentifier $ unwrap entId) (Just rev)
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

saveEntiteit :: forall a i r. Attachment a => GenericEncode r => Generic a r => Persistent a i => i -> MonadPerspectives a
saveEntiteit id = saveEntiteit' id Nothing

-- | Save the given entity to the database and puts it, with a correct (new) revision,
-- | in the cache, assuming the current revision of the entiteit equals that in the database.
saveEntiteit_ :: forall a i r. Attachment a => GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectives a
saveEntiteit_ entId entiteit = saveEntiteit' entId (Just entiteit)

-- | Save an Entiteit and set its new _rev parameter in the cache.
saveEntiteit' :: forall a i r. Attachment a => GenericEncode r => Generic a r => Persistent a i => i -> Maybe a -> MonadPerspectives a
saveEntiteit' entId mentiteit = ensureAuthentication (Resource $ unwrap entId) $ \_ -> do
  mentityFromCache <- tryTakeEntiteitFromCache entId
  entiteit <- case mentiteit of
    Nothing -> case mentityFromCache of
      Nothing -> throwError $ error ("saveEntiteit' needs either an entity as parameter, or a locally stored resource for " <>  unwrap entId)
      Just e -> pure e
    Just e -> pure e

  {database, documentName} <- resourceIdentifier2WriteDocLocator (unwrap entId)
  (rev :: Revision_) <- addDocument database entiteit documentName
  
  -- Returns either the local database name or a URL.
  -- dbName <- writeDbName entId
  -- -- couchdbResourceIdentifier is either a local identifier in the model:User namespace, or a segmented name (in the case of a public resource).
  -- (rev :: Revision_) <- addDocument dbName entiteit (couchdbResourceIdentifier $ unwrap entId)
  entiteit' <- pure (changeRevision rev entiteit)
  void $ cacheEntity entId entiteit'
  pure entiteit'

-- | Updates the revision in cache (no change to the version in database).
-- | Version is taken from the local models database, not from the repository!
updateRevision :: forall a i. Persistent a i => i -> MonadPerspectives Unit
updateRevision entId = do
  {database, documentName} <- resourceIdentifier2WriteDocLocator (unwrap entId)
  revision <- retrieveDocumentVersion database documentName
  setRevision entId revision 
