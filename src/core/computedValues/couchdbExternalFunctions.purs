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

-- | This module defines External Core functions for model:Couchdb.

module Perspectives.Extern.Couchdb where

import Control.Monad.AvarMonadAsk (modify) as AMA
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.State (State, StateT, execState, execStateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (cons, head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType (MediaType(..))
import Data.Newtype (over, unwrap)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Generic.Class (class GenericEncode)
import Foreign.Object (Object, empty, fromFoldable, insert, lookup, union)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeContext_me, changeRol_isMe, rol_binding, rol_pspType)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MP, MPQ, MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.Couchdb (DocWithAttachmentInfo(..))
import Perspectives.Couchdb.Databases (getAttachmentsFromUrl, getViewOnDatabase_)
import Perspectives.Couchdb.Revision (Revision_, changeRevision, rev)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..), DomeinFileRecord, SeparateInvertedQuery(..))
import Perspectives.Error.Boundaries (handleDomeinFileError, handlePerspectRolError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (getFirstMatch, namespaceFromUrl)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Indexed (replaceIndexedNames)
import Perspectives.Instances.ObjectGetters (isMe)
import Perspectives.Names (getMySystem, getUserIdentifier, lookupIndexedContext, lookupIndexedRole)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (addAttachment, addDocument, getAttachment, getDocument, getSystemIdentifier, getViewOnDatabase, retrieveDocumentVersion, tryGetDocument)
import Perspectives.Persistent (class Persistent, entitiesDatabaseName, getDomeinFile, getPerspectEntiteit, saveEntiteit, saveEntiteit_, tryFetchEntiteit, tryGetPerspectEntiteit, updateRevision)
import Perspectives.PerspectivesState (publicRepository)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity, overwriteEntity)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Prelude (Unit, append, bind, discard, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>>=), (||))
import Unsafe.Coerce (unsafeCoerce)

-- | Retrieve from the repository the external roles of instances of sys:Model.
-- | These are kept in the field "modelDescription" of DomeinFile.
-- | TODO: provide a repository parameter, so the URL is taken from the repository rather than hardcoded.
models :: ContextInstance -> MPQ RoleInstance
models _ = ArrayT do
  sysId <- lift getSystemIdentifier
  lift $ getExternalRoles

  where

    getExternalRoles :: MP (Array RoleInstance)
    getExternalRoles = do
      repo <- publicRepository
      (roles :: Array PerspectRol) <- catchError (getViewOnDatabase_ repo "" "defaultViews" "modeldescriptions" (Nothing :: Maybe Unit))
        \_ -> do
          logPerspectivesError $ Custom "getExternalRoles failed"
          pure []
      for roles \r@(PerspectRol{_id}) -> do
        -- If the model is already in use, this role has been saved before.
        (savedRole :: Maybe PerspectRol) <- tryGetPerspectEntiteit _id
        case savedRole of
          Nothing -> void $ cacheEntity _id r
          Just _ -> pure unit
        pure _id

modelsDatabaseName :: MonadPerspectives String
modelsDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_models")

-- | Retrieves all instances of a particular role type from Couchdb.
-- | For example: `user: Users = callExternal cdb:RoleInstances("model:System$PerspectivesSystem$User") returns: model:System$PerspectivesSystem$User`
-- | Notice that only the first element of the array argument is actually used.
-- | Notice, too, that the second parameter is ignored. We must provide it, however, as the query compiler
-- | will give us an argument for it.
roleInstancesFromCouchdb :: Array String -> (ContextInstance ~~> RoleInstance)
roleInstancesFromCouchdb roleTypes _ = ArrayT do
  case head roleTypes of
    Nothing -> pure []
    Just rt -> do
      (tell $ ArrayWithoutDoubles [RoleAssumption (ContextInstance "model:System$AnyContext") (EnumeratedRoleType rt)])
      (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/roleView" (head roleTypes)

contextInstancesFromCouchdb :: Array String -> (RoleInstance ~~> ContextInstance)
contextInstancesFromCouchdb contextTypeArr _ = ArrayT do
  case head contextTypeArr of
    Nothing -> pure []
    Just ct -> do
      -- push assumption!
      (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/contextView" (head contextTypeArr)

pendingInvitations :: ContextInstance ~~> RoleInstance
pendingInvitations _ = ArrayT do
  -- tell $ ArrayWithoutDoubles [RoleAssumption (ContextInstance "model:System$AnyContext") (EnumeratedRoleType rt)]
  (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/pendingInvitations" (Nothing :: Maybe Unit)

-- | Retrieve the model(s) from the url(s) and add them to the local couchdb installation.
-- | Load the dependencies first.
-- | Load the acompanying instances, too.
-- | Notice that the urls should be the full path to the relevant documents.
-- | We assume the url to be of the form http(s)://<some-domain>/repository/model:<some-model>
-- | This function is applied with `callEffect`. Accordingly, it will get the ContextInstance of the Action as second parameter.
-- TODO. Authentication at the repository urls.
addModelToLocalStore :: Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
addModelToLocalStore urls r = addModelsToLocalStore_ urls

addModelsToLocalStore_ :: Array String -> MonadPerspectivesTransaction Unit
addModelsToLocalStore_ urls = for_ urls addModelToLocalStore'

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
          repositoryUrl <- repository url
          docName <- documentName url
          df@(DomeinFile{_id, modelDescription, crl, indexedRoles, indexedContexts, referredModels, invertedQueriesInOtherDomains}) <- lift2 $ getDocument repositoryUrl docName
          addModelsToLocalStore_ (append repositoryUrl <<< unwrap <$> referredModels)
          -- Store the model in Couchdb, if none exists in cache or in the database.
          -- Remove the revision before saving: it belongs to the repository,
          -- not the local perspect_models.
          revision <- save (identifier df :: DomeinFileId) (changeRevision Nothing df)

          -- Add replacements to PerspectivesState for the new indexed names introduced in this model,
          -- unless we find existing ones left over from a previous installation of the model.
          (iroles :: Object RoleInstance) <- for indexedRoles (\iRole -> do
            (mexistingReplacement :: Maybe RoleInstance) <- lift $ lift $ lookupIndexedRole (unwrap iRole)
            case mexistingReplacement of
              Just existingReplacement -> pure $ Tuple (unwrap iRole) existingReplacement
              Nothing -> do
                g <- liftEffect guid
                pure $ Tuple (unwrap iRole) (RoleInstance ("model:User$" <> show g))) >>= pure <<< fromFoldable

          (icontexts :: Object ContextInstance) <- for indexedContexts (\iContext -> do
            (mexistingReplacement :: Maybe ContextInstance) <- lift $ lift $ lookupIndexedContext (unwrap iContext)
            case mexistingReplacement of
              Just existingReplacement -> pure $ Tuple (unwrap iContext) existingReplacement
              Nothing -> do
                g <- liftEffect guid
                pure $ Tuple (unwrap iContext) (ContextInstance ("model:User$" <> show g))) >>= pure <<< fromFoldable

          mySystem <- lift2 (ContextInstance <$> getMySystem)
          me <- lift2 (RoleInstance <$> getUserIdentifier)
          -- TODO. Do we really have to reassert Me and MySystem every time? Presumably this is for the
          -- situation where we do not yet have model:System.
          void $ lift2 $ AMA.modify \ps -> ps {indexedRoles = insert "model:System$Me" me (ps.indexedRoles `union` iroles), indexedContexts = insert "model:System$MySystem" mySystem (ps.indexedContexts `union` icontexts)}

          -- Replace any occurrence of any indexed name in the CRL file holding the instances of this model.
          crl' <- lift2 $ replaceIndexedNames crl

          -- Retrieve the modelDescription from cache or database: it may have been changed if the user decided to use it in InPlace.
          (mmodelDescription :: Maybe PerspectRol) <- case modelDescription of
            Nothing -> throwError (error ("A model has no description: " <> url))
            Just m -> lift2 $ tryGetPerspectEntiteit (identifier (m :: PerspectRol))

          -- Parse the CRL. This will cache all roleInstances, overwriting the modelDescription and any other entities
          -- in cache left over from a previous installation.
          parseResult <- lift2 $ parseAndCache crl'
          case parseResult of
            Left e -> throwError (error (show e))
            Right (Tuple contextInstances roleInstances') -> do
              -- Restore the modelDescription, preferring the version left over from a previous installation
              -- over the version that came out of the user instances in the crl file.
              case mmodelDescription of
                Nothing -> pure unit
                Just (m :: PerspectRol) -> void $ lift2 $ cacheEntity (identifier m) m

              -- Save role instances, but prefer a version left over from a previous installation.
              (cis :: Object PerspectContext) <- lift2 $ execStateT
                (forWithIndex_
                  roleInstances'
                  (\i a -> do
                    (mrole :: Maybe PerspectRol) <- lift $  tryFetchEntiteit (RoleInstance i)
                    case mrole of
                      -- If we find a previous version in the database, obviously we don't have to save it to the database.
                      Just role -> void $ lift $ overwriteEntity (RoleInstance i) role
                      -- For each role instance with a binding that is not one of the other imported role instances,
                      -- set the inverse binding administration on that binding.
                      Nothing -> case rol_binding a of
                        Nothing -> saveRoleInstance i a
                        Just newBindingId -> if (isJust $ lookup (unwrap newBindingId) roleInstances')
                          then saveRoleInstance i a
                          else (lift $ try $ getPerspectEntiteit newBindingId) >>=
                            handlePerspectRolError
                              "addModelToLocalStore'"
                              -- set the inverse binding
                              \newBinding -> do
                                void $ lift $ cacheEntity newBindingId (addRol_gevuldeRollen newBinding (rol_pspType a) (RoleInstance i))
                                void $ lift $ saveEntiteit newBindingId
                                saveRoleInstance i a
                                -- There can be no queries that use binder <type of a> on newBindingId, since the model is new.
                                -- So we need no action for QUERY UPDATES or RULE TRIGGERING.
                ))
                contextInstances

              -- Save context instances, but prefer a version left over from a previous installation.
              forWithIndex_ cis \i a -> lift2 do
                (mcontext :: Maybe PerspectContext) <- tryFetchEntiteit (ContextInstance i)
                case mcontext of
                  -- overwrite the entity in cache, including its version!
                  Just context -> void $ overwriteEntity (ContextInstance i) context
                  Nothing -> void $ saveEntiteit_ (ContextInstance i) a

          -- Distribute the SeparateInvertedQueries over the other domains.
          forWithIndex_ invertedQueriesInOtherDomains
            \domainName queries -> do
              (lift2 $ try $ getDomeinFile (DomeinFileId domainName)) >>=
                handleDomeinFileError "addModelToLocalStore'"
                \(DomeinFile dfr) -> do
                  -- Here we must take care to preserve the screens.js attachment.
                  lift2 (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ queries addInvertedQuery) dfr))

          -- Copy the attachment
          lift $ lift $ addA repositoryUrl docName revision
  where

    addInvertedQuery :: SeparateInvertedQuery -> State DomeinFileRecord Unit
    addInvertedQuery (OnContextDelta_context typeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup typeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{onContextDelta_context}) -> dfr {enumeratedRoles = insert typeName (EnumeratedRole rr {onContextDelta_context = cons invertedQuery onContextDelta_context}) enumeratedRoles}

    addInvertedQuery (OnContextDelta_role typeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup typeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{onContextDelta_role}) -> dfr {enumeratedRoles = insert typeName (EnumeratedRole rr {onContextDelta_role = cons invertedQuery onContextDelta_role}) enumeratedRoles}

    addInvertedQuery (OnRoleDelta_binder typeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup typeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{onRoleDelta_binder}) -> dfr {enumeratedRoles = insert typeName (EnumeratedRole rr {onRoleDelta_binder = cons invertedQuery onRoleDelta_binder}) enumeratedRoles}

    addInvertedQuery (OnRoleDelta_binding typeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup typeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{onRoleDelta_binding}) -> dfr {enumeratedRoles = insert typeName (EnumeratedRole rr {onRoleDelta_binding = cons invertedQuery onRoleDelta_binding}) enumeratedRoles}

    addInvertedQuery (OnPropertyDelta typeName invertedQuery) = void $ modify \dfr@{enumeratedProperties} ->
      case lookup typeName enumeratedProperties of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedProperty rr@{onPropertyDelta}) -> dfr {enumeratedProperties = insert typeName (EnumeratedProperty rr {onPropertyDelta = cons invertedQuery onPropertyDelta}) enumeratedProperties}

    -- Sets the `me` property on the role instances. Detects the ultimate bottom case: the user instance of sys:PerspectivesSystem. Note that model user instances should never comprise particular other users!
    saveRoleInstance :: String -> PerspectRol -> StateT (Object PerspectContext) MonadPerspectives Unit
    saveRoleInstance i a@(PerspectRol{context, pspType}) = do
      me <- lift $ isMe (RoleInstance i)
      if me || pspType == (EnumeratedRoleType "model:System$PerspectivesSystem$User")
        then do
          void $ lift $ saveEntiteit_ (RoleInstance i) (changeRol_isMe a true)
          void $ modify \cis -> case lookup (unwrap context) cis of
            Nothing -> cis
            Just c -> insert (unwrap context) (changeContext_me c (Just (RoleInstance i))) cis
        else void $ lift $ saveEntiteit_ (RoleInstance i) a

    -- Safes the entity, unless a version exists in cache or in the database.
    save :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectivesTransaction Revision_
    save i a = lift $ lift do
      mexistingContext <- tryGetPerspectEntiteit i
      case mexistingContext of
        Nothing -> do
          void $ cacheEntity i a
          -- updateRevision i
          e <- saveEntiteit i
          pure $ rev e
        Just e -> pure $ rev e

    -- url is the path to the document in the repository.
    addA :: String -> String -> Revision_ -> MP Unit
    addA repoName modelName rev = do
      mAttachment <- getAttachment repoName modelName "screens.js"
      case mAttachment of
        Nothing -> pure unit
        Just attachment -> do
          perspect_models <- modelsDatabaseName
          void $ addAttachment perspect_models modelName rev "screens.js" attachment (MediaType "text/ecmascript")
          updateRevision (DomeinFileId modelName)

    repository :: String -> MonadPerspectivesTransaction String
    repository url' = case getFirstMatch (unsafeRegex "^(.*/).+$" noFlags) url' of
      Nothing -> throwError (error ("Cannot get repository from " <> url'))
      Just s -> pure s

    documentName :: String -> MonadPerspectivesTransaction String
    documentName url' = case getFirstMatch (unsafeRegex "^.*/(.+)$" noFlags) url' of
      Nothing -> throwError (error ("Cannot get document name from " <> url'))
      Just s -> pure s

-- | Take a DomeinFile from the local perspect_models database and upload it to the repository database at url.
-- | Notice that url should include the name of the repository database within the couchdb installation. We do
-- | not assume anything about that name here.
-- | Attachments are preserved: if they were in the repository before uploading,
-- | they will be in the repository after uploading.
uploadToRepository :: DomeinFileId -> URL -> MPQ Unit
uploadToRepository dfId url = do
  mdf <- lift $ lift $ try $ getPerspectEntiteit dfId
  case mdf of
    Left err -> logPerspectivesError $ DomeinFileErrorBoundary "uploadToRepository" (show err)
    Right df -> uploadToRepository_ dfId url df

-- | As uploadToRepository, but provide the DomeinFile as argument.
uploadToRepository_ :: DomeinFileId -> URL -> DomeinFile -> MPQ Unit
uploadToRepository_ dfId url df = lift $ lift $ do
  -- Get the attachment info
  (atts :: Maybe DocWithAttachmentInfo) <- tryGetDocument url (show dfId)
  attachments <- case atts of
    Nothing -> pure empty
    Just (DocWithAttachmentInfo {_attachments}) -> traverseWithIndex
      (\attName {content_type} -> Tuple (MediaType content_type) <$> getAttachment url (show dfId) attName)
      _attachments
  -- Get the revision (if any) from the remote database, so we can overwrite.
  (mVersion :: Maybe String) <- retrieveDocumentVersion url (show dfId)
  res <- addDocument url (changeRevision mVersion df) (show dfId)
  -- Now add the attachments.
  forWithIndex_ attachments \attName (Tuple mimetype mattachment) -> case mattachment of
    Nothing -> pure unit
    Just attachment -> void $ addAttachment url (show dfId) mVersion attName attachment mimetype

type URL = String

-- | The argument of type Array String contains a model identifier.
removeModelFromLocalStore :: Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
removeModelFromLocalStore rs _ = case head rs of
  Nothing -> pure unit
  Just r -> scheduleDomeinFileRemoval (DomeinFileId r)

scheduleDomeinFileRemoval :: DomeinFileId -> MonadPerspectivesTransaction Unit
scheduleDomeinFileRemoval id = lift $ AMA.modify (over Transaction \t@{modelsToBeRemoved} -> t {modelsToBeRemoved = cons id modelsToBeRemoved})

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:Couchdb$Models" {func: unsafeCoerce models, nArgs: 0}
  , Tuple "model:Couchdb$AddModelToLocalStore" {func: unsafeCoerce addModelToLocalStore, nArgs: 1}
  , Tuple "model:Couchdb$UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 2}
  , Tuple "model:Couchdb$RoleInstances" {func: unsafeCoerce roleInstancesFromCouchdb, nArgs: 1}
  , Tuple "model:Couchdb$PendingInvitations" {func: unsafeCoerce pendingInvitations, nArgs: 0}
  , Tuple "model:Couchdb$RemoveModelFromLocalStore" {func: unsafeCoerce removeModelFromLocalStore, nArgs: 1}
  , Tuple "model:Couchdb$ContextInstances" {func: unsafeCoerce contextInstancesFromCouchdb, nArgs: 1}
]
