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

-- | This module defines External Core functions for model:Couchdb.

module Perspectives.Extern.Couchdb where

import Affjax (Request, URL, printResponseFormatError, request)
import Affjax.RequestBody (string) as RequestBody
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.State (StateT, execStateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Generic (defaultOptions, genericEncodeJSON)
import Foreign.Generic.Class (class GenericEncode)
import Foreign.Object (Object, empty, fromFoldable, insert, lookup)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeContext_me, changeRol_isMe, rol_binding, rol_pspType)
import Perspectives.CoreTypes (MP, MPQ, MonadPerspectivesTransaction, MonadPerspectives, assumption)
import Perspectives.Couchdb (PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (addAttachment, createDatabase, defaultPerspectRequest, getViewOnDatabase, retrieveDocumentVersion, version)
import Perspectives.Couchdb.Revision (changeRevision)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription, hiddenFunctionInsert)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (isMe)
import Perspectives.Persistent (class Persistent, entitiesDatabaseName, getPerspectEntiteit, saveEntiteit, saveEntiteit_)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheInitially, cacheOverwritingRevision, cachePreservingRevision)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.User (getUserIdentifier)
import Prelude (class Monad, Unit, bind, discard, map, pure, show, unit, void, ($), (<<<), (<>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- | Retrieve from the repository the external roles of instances of sys:Model.
-- | These are kept in the field "modelDescription" of DomeinFile.
-- | TODO: provide a repository parameter, so the URL is taken from the repository rather than hardcoded.
models :: MPQ RoleInstance
models = ArrayT do
  tell [assumption "model:User$MijnSysteem" ophaalTellerName]
  lift $ getExternalRoles

  where
    getListOfModels :: MP (Array RoleInstance)
    getListOfModels = catchError (modelsDatabaseName >>= documentNamesInDatabase >>= pure <<< map (RoleInstance <<< (_ <> "$External"))) \_ -> pure []

    ophaalTellerName :: String
    ophaalTellerName = "model:System$PerspectivesSystem$External$ModelOphaalTeller"

    getExternalRoles :: MP (Array RoleInstance)
    getExternalRoles = do
      (roles :: Array PerspectRol) <- getViewOnDatabase "repository" "defaultViews" "modeldescriptions" Nothing
      for roles \r@(PerspectRol{_id}) -> do
        void $ cachePreservingRevision _id r
        pure _id

modelsDatabaseName :: MonadPerspectives String
modelsDatabaseName = getUserIdentifier >>= pure <<< (_ <> "_models/")

-- | Retrieves all instances of a particular role type from Couchdb.
-- | For example: `user: Users = callExternal cdb:RoleInstances("model:System$PerspectivesSystem$User") returns: model:System$PerspectivesSystem$User`
-- | Notice that only the first element of the array argument is actually used.
roleInstances :: Array String -> MPQ RoleInstance
roleInstances roleTypes = ArrayT $ lift $ do
  (roles :: Array PerspectRol) <- entitiesDatabaseName >>= \db -> getViewOnDatabase db "defaultViews" "roleView" (head roleTypes)
  for roles \r@(PerspectRol{_id}) -> do
    void $ cachePreservingRevision _id r
    pure _id

-- | Retrieve the model(s) from the url(s) and add them to the local couchdb installation.
-- | Load the acompanying instances, too.
-- | Notice that the urls should be the full path to the relevant documents.
-- TODO. Authentication at the repository urls.
addModelToLocalStore :: Array String -> MonadPerspectivesTransaction Unit
addModelToLocalStore urls = do
  for_ urls addModelToLocalStore'
  where
    addModelToLocalStore' :: String -> MonadPerspectivesTransaction Unit
    addModelToLocalStore' url = do
        -- Retrieve the DomeinFile from the URL.
      (rq :: (Request String)) <- lift $ lift $ defaultPerspectRequest
      res <- liftAff $ request $ rq {url = url}
      (df@(DomeinFile{_id, contextInstances, roleInstances: roleInstances', modelDescription}) :: DomeinFile) <- liftAff $ onAccepted res.status [200, 304] "addModelToLocalStore"
        (onCorrectCallAndResponse "addModelToLocalStore" res.body \a -> pure unit)
      rev <- version res.headers
      -- Store the model in Couchdb. Remove the revision: it belongs to the repository,
      -- not the local perspect_models.
      save (identifier df :: DomeinFileId) (changeRevision Nothing df)
      -- Copy the attachment
      lift $ lift $ addA url _id
      -- Take the role- and contextinstances from it and add them to the user (instances) database.

      -- We have to cache all roleInstances (except the external role of the modeldescription) first,
      -- otherwise we cannot see what its `isMe` must be.
      forWithIndex_ roleInstances' \i a -> void $ lift $ lift $ try (cacheInitially (RoleInstance i) a)
      cis <- lift2 $ execStateT (saveRoleInstances roleInstances') contextInstances
      forWithIndex_ cis \i a -> save (ContextInstance i) a
      -- For each role instance with a binding that is not one of the other imported role instances,
      -- set the inverse binding administration on that binding.
      forWithIndex_ roleInstances' \i a -> case rol_binding a of
        Nothing -> pure unit
        Just newBindingId -> if (isJust $ lookup (unwrap newBindingId) roleInstances')
          then pure unit
          else do
            -- set the inverse binding
            newBinding <- lift2 $ getPerspectEntiteit newBindingId
            lift2 $ void $ cacheOverwritingRevision newBindingId (addRol_gevuldeRollen newBinding (rol_pspType a) (RoleInstance i))
            lift2 $ void $ saveEntiteit newBindingId
            -- There can be no queries that use binder <type of a> on newBindingId, since the model is new.
            -- So we need no action for QUERY UPDATES or RULE TRIGGERING.

    saveRoleInstances :: Object PerspectRol -> StateT (Object PerspectContext) MonadPerspectives Unit
    saveRoleInstances ris = forWithIndex_ ris \i a@(PerspectRol{context}) -> do
      me <- lift $ isMe (RoleInstance i)
      if me
        then do
          void $ lift $ saveEntiteit_ (RoleInstance i) (changeRol_isMe a true)
          void $ modify \cis -> case lookup (unwrap context) cis of
            Nothing -> cis
            Just c -> insert (unwrap context) (changeContext_me c (Just (RoleInstance i))) cis
        else void $ lift $ saveEntiteit_ (RoleInstance i) a

    save :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectivesTransaction Unit
    save i a = do
      void $ lift $ lift $ cacheInitially i a
      void $ lift $ lift $ saveEntiteit i

    -- url is the path to the document in the repository.
    addA :: String -> String -> MP Unit
    addA url modelName = do
      (rq :: (Request String)) <-  defaultPerspectRequest
      res <- liftAff $ request $ rq {url = url <> "/screens.js"}
      -- res <- liftAff $ request $ rq {url = docUrl <> (maybe "" ((<>) "?rev=") rev) <> "/screens.js"}
      result <- onAccepted res.status [200, 304] "uploadToRepository_" (pure res.body)
      void $ case result of
        Left e -> throwError $ error ("uploadToRepository: Errors on retrieving attachment: " <> (printResponseFormatError e))
        Right attachment -> do
          perspect_models <- modelsDatabaseName
          void $ addAttachment (perspect_models <> modelName) "screens.js" attachment (MediaType "text/ecmascript")

-- | Take a DomeinFile from the local perspect_models database and upload it to the repository database at url.
-- | Notice that url should include the name of the repository database within the couchdb installation. We do
-- | not assume anything about that name here.
uploadToRepository :: DomeinFileId -> URL -> MPQ Unit
uploadToRepository dfId url = do
  df <- lift $ lift $ getPerspectEntiteit dfId
  uploadToRepository_ dfId url df

-- | As uploadToRepository, but provide the DomeinFile as argument.
uploadToRepository_ :: DomeinFileId -> URL -> DomeinFile -> MPQ Unit
uploadToRepository_ dfId url df = lift $ lift $ do
  docUrl <- pure (url <> "/" <> (show dfId))
  (rq :: (Request String)) <- defaultPerspectRequest
  -- Try to get the revision, so we can overwrite.
  mVersion <- retrieveDocumentVersion docUrl
  case mVersion of
    Nothing -> do
    -- If not available, store without revision
      res <- liftAff $ request $ rq {method = Left PUT, url = docUrl, content = Just $ RequestBody.string (genericEncodeJSON defaultOptions df)}
      void $ onAccepted res.status [200, 201] "uploadToRepository_"
        (onCorrectCallAndResponse "uploadToRepository_" res.body (\(a :: PutCouchdbDocument) -> pure unit))
      -- Now add the attachment.
    -- If available, store with revision
    Just rev -> do
      res <- liftAff $ request $ rq {method = Left PUT, url = (docUrl <> "?rev=" <> rev), content = Just $ RequestBody.string  (genericEncodeJSON defaultOptions df)}
      void $ onAccepted res.status [200, 201] "uploadToRepository_"
        (onCorrectCallAndResponse "uploadToRepository_" res.body (\(a :: PutCouchdbDocument) -> pure unit))

-- | Create a new database for the communication between `me` and another user.
-- | Create an instance of sys:Channel. Bind `me` in the role ConnectedPartner. Set the value of ChannelDatabaseName to
-- | that of the new database.
-- | Bind the new Channel to usr:MijnSysteem in the role Channels.
createChannel :: MonadPerspectivesTransaction ContextInstance
createChannel = do
  channelName <- pure ("channel_" <> (show $ guid unit))
  log ("Will create " <> channelName)
  lift2 $ createDatabase channelName
  eChannel <- constructContext $ ContextSerialization
    { id: "model:User$" <> channelName
    , prototype: Nothing
    , ctype: "sys:Channel"
    , rollen: fromFoldable [(Tuple "model:System$Channel$ConnectedPartner"
      [ RolSerialization
        { properties: PropertySerialization empty,
        binding: Just "usr:Me" }
       ])]
    , externeProperties: PropertySerialization $ fromFoldable [Tuple "model:System$Channel$External$ChannelDatabaseName" [channelName]]
    }
  -- TODO: dit is eigenlijk niet nodig.
  case eChannel of
    Left e -> throwError (error ("createChannel could not create channel: " <> show e))
    Right (channel :: ContextInstance) -> do
      void $ createAndAddRoleInstance (EnumeratedRoleType "model:System$PerspectivesSystem$Channels")
        "model:User$MijnSysteem"
        (RolSerialization
          { properties: PropertySerialization empty,
          binding: Just (buitenRol $ unwrap channel)})
      pure channel

addUserToChannel :: RoleInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
addUserToChannel (RoleInstance usr) (ContextInstance channel) = void $ createAndAddRoleInstance (EnumeratedRoleType "model:System$Channel$ConnectedPartner")
  channel
  (RolSerialization
    { properties: PropertySerialization empty,
    binding: Just usr})

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "couchdb_Models" {func: unsafeCoerce models, nArgs: 0}
  , Tuple "couchdb_AddModelToLocalStore" {func: unsafeCoerce addModelToLocalStore, nArgs: 1}
  , Tuple "couchdb_UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 2}
  , Tuple "couchdb_RoleInstances" {func: unsafeCoerce roleInstances, nArgs: 1}
]

addExternalFunctions :: forall m. Monad m => m Unit
addExternalFunctions = for_ externalFunctions \(Tuple n f) -> pure $ hiddenFunctionInsert n f.func f.nArgs
