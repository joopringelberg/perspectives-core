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
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.AvarMonadAsk (modify) as AMA
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (StateT, execStateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (elemIndex, head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Generic (encodeJSON)
import Foreign.Generic.Class (class GenericEncode)
import Foreign.Object (Object, empty, fromFoldable, insert, lookup, union)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeContext_me, changeRol_isMe, rol_binding, rol_pspType)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (MP, MPQ, MonadPerspectives, MonadPerspectivesTransaction, assumption, type (~~>))
import Perspectives.Couchdb (DocWithAttachmentInfo(..), PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (addAttachment, addAttachmentToUrl, defaultPerspectRequest, documentNamesInDatabase, getAttachmentsFromUrl, getDocumentAsStringFromUrl, getViewOnDatabase, retrieveDocumentVersion, version)
import Perspectives.Couchdb.Revision (changeRevision)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (getFirstMatch, namespaceFromUrl)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Indexed (replaceIndexedNames)
import Perspectives.Instances.ObjectGetters (isMe)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Persistent (class Persistent, entitiesDatabaseName, getPerspectEntiteit, saveEntiteit, saveEntiteit_, tryGetPerspectEntiteit, updateRevision)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.User (getSystemIdentifier)
import Prelude (Unit, append, bind, discard, map, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>>=), (||))
import Unsafe.Coerce (unsafeCoerce)

-- | Retrieve from the repository the external roles of instances of sys:Model.
-- | These are kept in the field "modelDescription" of DomeinFile.
-- | TODO: provide a repository parameter, so the URL is taken from the repository rather than hardcoded.
models :: ContextInstance -> MPQ RoleInstance
models _ = ArrayT do
  sysId <- lift getSystemIdentifier
  tell [assumption sysId ophaalTellerName]
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
        -- If the model is already in use, this role has been saved before.
        (savedRole :: Maybe PerspectRol) <- tryGetPerspectEntiteit _id
        case savedRole of
          Nothing -> void $ cacheEntity _id r
          Just _ -> pure unit
        pure _id

modelsDatabaseName :: MonadPerspectives String
modelsDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_models/")

-- | Retrieves all instances of a particular role type from Couchdb.
-- | For example: `user: Users = callExternal cdb:RoleInstances("model:System$PerspectivesSystem$User") returns: model:System$PerspectivesSystem$User`
-- | Notice that only the first element of the array argument is actually used.
-- | Notice, too, that the second parameter is ignored. We must provide it, however, as the query compiler
-- | will give us an argument for it.
-- TODO. We hebben een mechanisme nodig om te actualiseren als er een instantie bijkomt of afgaat.
roleInstancesFromCouchdb :: Array String -> (ContextInstance ~~> RoleInstance)
roleInstancesFromCouchdb roleTypes _ = ArrayT do
  case head roleTypes of
    Nothing -> pure []
    Just rt -> do
      tell [assumption "AnyContext" rt]
      (roles :: Array PerspectRol) <- (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews" "roleView" (head roleTypes)
      for roles \r@(PerspectRol{_id}) -> do
        void $ lift $ cacheEntity _id r
        pure _id

-- | Retrieve the model(s) from the url(s) and add them to the local couchdb installation.
-- | Load the dependencies first.
-- | Load the acompanying instances, too.
-- | Notice that the urls should be the full path to the relevant documents.
-- | This function is applied with `callEffect`. Accordingly, it will get the Object of the Action as second parameter.
-- TODO. Authentication at the repository urls.
addModelToLocalStore :: Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
addModelToLocalStore urls r = do
  for_ urls addModelToLocalStore'
  where
    addModelToLocalStore' :: String -> MonadPerspectivesTransaction Unit
    addModelToLocalStore' url = do
      case namespaceFromUrl url of
        Nothing -> throwError (error $ "Repository URL does not end on model name: " <> url)
        Just ns -> do
          mdomeinFile <- lift2 $ tryGetPerspectEntiteit (DomeinFileId ns)
          case mdomeinFile of
            Just _ -> pure unit
            Nothing -> do
                -- Retrieve the DomeinFile from the URL.
              (rq :: (Request String)) <- lift $ lift $ defaultPerspectRequest
              res <- liftAff $ request $ rq {url = url}
              df@(DomeinFile{_id, modelDescription, crl, indexedRoles, indexedContexts, referredModels}) <- liftAff $ onAccepted res.status [200, 304] "addModelToLocalStore"
                (onCorrectCallAndResponse "addModelToLocalStore" res.body \a -> pure unit)
              repositoryUrl <- lift2 $ repository url
              addModelToLocalStore (append repositoryUrl <<< unwrap <$> referredModels) r
              rev <- version res.headers
              -- Store the model in Couchdb. Remove the revision: it belongs to the repository,
              -- not the local perspect_models.
              save (identifier df :: DomeinFileId) (changeRevision Nothing df)
              -- Copy the attachment
              lift $ lift $ addA url _id

              -- Add replacements to PerspectivesState for the new indexed names introduced in this model.
              (iroles :: Object RoleInstance) <- pure $ fromFoldable $ (\iRole -> Tuple (unwrap iRole) (RoleInstance ("model:User$" <> show (guid unit)))) <$> indexedRoles
              (icontexts :: Object ContextInstance) <- pure $ fromFoldable $ (\iContext -> Tuple (unwrap iContext) (ContextInstance ("model:User$" <> show (guid unit)))) <$> indexedContexts
              -- Make sure that `sys:Me` and `sys:MySystem` replacements come from perspectivesState.
              mySystem <- lift2 (ContextInstance <$> getMySystem)
              me <- lift2 (RoleInstance <$> getUserIdentifier)
              void $ lift2 $ AMA.modify \ps -> ps {indexedRoles = insert "model:System$Me" me (ps.indexedRoles `union` iroles), indexedContexts = insert "model:System$MySystem" mySystem (ps.indexedContexts `union` icontexts)}

              -- Replace any occurrence of any indexed name in the CRL file holding the instances of this model.
              crl' <- lift2 $ replaceIndexedNames crl

              -- Retrieve the modelDescription from cache: it may have been changed if the user decided to use it in InPlace.
              modelDescription' <- case modelDescription of
                Nothing -> throwError (error ("A model has no description: " <> url))
                Just m -> lift2 $ tryGetPerspectEntiteit (identifier (m :: PerspectRol))

              -- Parse the CRL. This will cache all roleInstances, overwriting the modelDescription.
              parseResult <- lift2 $ parseAndCache crl'
              case parseResult of
                Left e -> throwError (error (show e))
                Right (Tuple contextInstances roleInstances') -> do
                  -- Restore the modelDescription, overwrite the version that came out of the user instances.
                  case modelDescription' of
                    Nothing -> pure unit
                    Just (m :: PerspectRol) -> void $ lift2 $ cacheEntity (identifier m) m
                  -- Save role- and contextinstances.
                  cis <- lift2 $ execStateT (saveRoleInstances roleInstances') contextInstances
                  forWithIndex_ cis \i a -> lift2 $ saveEntiteit_ (ContextInstance i) a
                  -- For each role instance with a binding that is not one of the other imported role instances,
                  -- set the inverse binding administration on that binding.
                  forWithIndex_ roleInstances' \i a -> case rol_binding a of
                    Nothing -> pure unit
                    Just newBindingId -> if (isJust $ lookup (unwrap newBindingId) roleInstances')
                      then pure unit
                      else do
                        -- set the inverse binding
                        newBinding <- lift2 $ getPerspectEntiteit newBindingId
                        lift2 $ void $ cacheEntity newBindingId (addRol_gevuldeRollen newBinding (rol_pspType a) (RoleInstance i))
                        lift2 $ void $ saveEntiteit newBindingId
                        -- There can be no queries that use binder <type of a> on newBindingId, since the model is new.
                        -- So we need no action for QUERY UPDATES or RULE TRIGGERING.

    -- Sets the `me` property on the role instances. Detects the ultimate bottom case: the user instance of sys:PerspectivesSystem. Note that model user instances should never comprise particular other users!
    saveRoleInstances :: Object PerspectRol -> StateT (Object PerspectContext) MonadPerspectives Unit
    saveRoleInstances ris = forWithIndex_ ris \i a@(PerspectRol{context, pspType}) -> do
      me <- lift $ isMe (RoleInstance i)
      if me || pspType == (EnumeratedRoleType "model:System$PerspectivesSystem$User")
        then do
          void $ lift $ saveEntiteit_ (RoleInstance i) (changeRol_isMe a true)
          void $ modify \cis -> case lookup (unwrap context) cis of
            Nothing -> cis
            Just c -> insert (unwrap context) (changeContext_me c (Just (RoleInstance i))) cis
        else void $ lift $ saveEntiteit_ (RoleInstance i) a

    -- Prefer an earlier version of the Context instance.
    save :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectivesTransaction Unit
    save i a = lift $ lift do
      mexistingContext <- tryGetPerspectEntiteit i
      case mexistingContext of
        Nothing -> do
          void $ cacheEntity i a
          -- updateRevision i
          void $ saveEntiteit i
        otherwise -> pure unit

    -- url is the path to the document in the repository.
    addA :: String -> String -> MP Unit
    addA url modelName = do
      (rq :: (Request String)) <-  defaultPerspectRequest
      res <- liftAff $ request $ rq {url = url <> "/screens.js"}
      case elemIndex res.status [StatusCode 200, StatusCode 304] of
        Nothing -> pure unit
        Just _ -> do
          void $ case res.body of
            Left e -> throwError $ error ("addModelToLocalStore: Errors on retrieving attachment: " <> (printResponseFormatError e))
            -- Left e -> pure unit
            Right attachment -> do
              perspect_models <- modelsDatabaseName
              void $ addAttachment (perspect_models <> modelName) "screens.js" attachment (MediaType "text/ecmascript")
              updateRevision (DomeinFileId modelName)

    repository :: String -> MP String
    repository url = case getFirstMatch (unsafeRegex "^(.*/).+$" noFlags) url of
      Nothing -> throwError (error ("Cannot get repository from " <> url))
      Just s -> pure s

-- | Take a DomeinFile from the local perspect_models database and upload it to the repository database at url.
-- | Notice that url should include the name of the repository database within the couchdb installation. We do
-- | not assume anything about that name here.
-- | Attachments are preserved: if they were in the repository before uploading,
-- | they will be in the repository after uploading.
uploadToRepository :: DomeinFileId -> URL -> MPQ Unit
uploadToRepository dfId url = do
  df <- lift $ lift $ getPerspectEntiteit dfId
  uploadToRepository_ dfId url df

-- | As uploadToRepository, but provide the DomeinFile as argument.
uploadToRepository_ :: DomeinFileId -> URL -> DomeinFile -> MPQ Unit
uploadToRepository_ dfId url df = lift $ lift $ do
  docUrl <- pure (url <> "/" <> (show dfId))
  -- Get the attachment info
  (atts :: Maybe DocWithAttachmentInfo) <- getAttachmentsFromUrl docUrl
  attachments <- case atts of
    Nothing -> pure empty
    Just (DocWithAttachmentInfo {_attachments}) -> traverseWithIndex (\attName {content_type} -> Tuple (MediaType content_type) <$> getDocumentAsStringFromUrl (docUrl <> "/" <> attName)) _attachments
  -- Try to get the revision, so we can overwrite.
  mVersion <- retrieveDocumentVersion docUrl
  (rq :: (Request String)) <- defaultPerspectRequest
  case mVersion of
    Nothing -> do
    -- If not available, store without revision
      res <- liftAff $ request $ rq {method = Left PUT, url = docUrl, content = Just $ RequestBody.string (encodeJSON df)}
      void $ onAccepted res.status [200, 201] "uploadToRepository_"
        (onCorrectCallAndResponse "uploadToRepository_" res.body (\(a :: PutCouchdbDocument) -> pure unit))
    -- If available, store with revision
    Just rev -> do
      res <- liftAff $ request $ rq {method = Left PUT, url = (docUrl <> "?rev=" <> rev), content = Just $ RequestBody.string  (encodeJSON df)}
      void $ onAccepted res.status [200, 201] "uploadToRepository_"
        (onCorrectCallAndResponse "uploadToRepository_" res.body (\(a :: PutCouchdbDocument) -> pure unit))
  -- Now add the attachments.
  forWithIndex_ attachments \attName (Tuple mimetype mattachment) -> case mattachment of
    Nothing -> pure unit
    Just attachment -> void $ addAttachmentToUrl docUrl attName attachment mimetype


-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:Couchdb$Models" {func: unsafeCoerce models, nArgs: 0}
  , Tuple "model:Couchdb$AddModelToLocalStore" {func: unsafeCoerce addModelToLocalStore, nArgs: 1}
  , Tuple "model:Couchdb$UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 2}
  , Tuple "model:Couchdb$RoleInstances" {func: unsafeCoerce roleInstancesFromCouchdb, nArgs: 1}
]
