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
import Control.Monad.State (State, StateT, execState, execStateT, get, modify, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (cons, head)
import Data.Array as ARR
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (over, unwrap)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (Object, empty, fromFoldable, insert, lookup, union)
import Perspectives.Assignment.StateCache (clearModelStates)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeContext_me, changeRol_binding, changeRol_isMe, rol_binding, rol_gevuldeRollen, rol_id, rol_isMe, rol_pspType, setRol_gevuldeRollen)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MP, MPQ, MonadPerspectives, MonadPerspectivesTransaction, (##=))
import Perspectives.Couchdb (DatabaseName, DeleteCouchdbDocument(..), DocWithAttachmentInfo(..), SecurityDocument(..))
import Perspectives.Couchdb.Revision (Revision_, changeRevision, rev)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..), DomeinFileRecord, SeparateInvertedQuery(..))
import Perspectives.Error.Boundaries (handleDomeinFileError, handlePerspectRolError)
import Perspectives.ErrorLogging (logPerspectivesError, warnModeller)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Guid (guid)
import Perspectives.Identifiers (getFirstMatch, namespaceFromUrl)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Indexed (replaceIndexedNames)
import Perspectives.Instances.ObjectGetters (isMe)
import Perspectives.InvertedQuery (addInvertedQueryIndexedByContext, addInvertedQueryIndexedByRole, deleteInvertedQueryIndexedByContext, deleteInvertedQueryIndexedByRole)
import Perspectives.Models (modelsInUse) as Models
import Perspectives.Names (getMySystem, getUserIdentifier, lookupIndexedContext, lookupIndexedRole)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (addAttachment, addDocument, getAttachment, getDocument, getViewOnDatabase, retrieveDocumentVersion, tryGetDocument)
import Perspectives.Persistence.CouchdbFunctions as CDB
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (entitiesDatabaseName, getDomeinFile, getPerspectEntiteit, getPerspectRol, saveEntiteit, saveEntiteit_, tryFetchEntiteit, tryGetPerspectEntiteit, updateRevision)
import Perspectives.PerspectivesState (publicRepository)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Warning (PerspectivesWarning(..))
import Prelude (Unit, append, bind, discard, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>>=), (||), eq)
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
      (roles :: Array PerspectRol) <- catchError (getViewOnDatabase repo "defaultViews/modeldescriptions" (Nothing :: Maybe Unit))
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

-- | Overwrites the model currently residing in the local models database.
-- | Takes care of inverted queries.
-- | Clears compiled states from cache.
-- | The first argument should contain the Url at which the new model version resides.
-- | The second argument should contain the string version of the model name ("model:Something")
-- | The third argument is an array with an instance of the role ModelsInuse. NOTE: this has to be adapted when we
-- | start using model:CouchbManagement.
updateModel :: Array String -> Array String -> Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
updateModel arrWithurl arrWithModelName arrWithDependencies modelsInUse = case head arrWithModelName of
  Nothing -> do
    descriptionGetter <- lift2 $ getDynamicPropertyGetter "model:System$Model$External$Description" (ST (EnumeratedRoleType "model:System$PerspectivesSystem$ModelsInUse"))
    description <- lift2 (modelsInUse ##= descriptionGetter)
    lift $ lift $ warnModeller Nothing (ModelLacksModelId (maybe "(without a description..)" unwrap (head description)))
  Just modelName -> do
    case head arrWithurl, head arrWithDependencies of
      Nothing, _ -> lift $ lift $ warnModeller Nothing (ModelLacksUrl modelName)
      Just url, mwithDependencies -> do
        repositoryUrl <- repository url
        updateModel' (maybe false (eq "true") mwithDependencies) repositoryUrl (DomeinFileId modelName)
  where
    -- TODO. Note that while we have not yet the model identification as an URL, the parameter url is bound to
    -- the repository location of the model supplied in the call to updateModel.
    updateModel' :: Boolean -> String -> DomeinFileId -> MonadPerspectivesTransaction Unit
    updateModel' withDependencies url dfId@(DomeinFileId modelName) = do
      DomeinFile{invertedQueriesInOtherDomains, referredModels} <- lift2 $ getDomeinFile dfId
      if withDependencies
        then for_ referredModels (updateModel' withDependencies url)
        else pure unit
        -- Untangle the InvertedQueries of the previous model.
      forWithIndex_ invertedQueriesInOtherDomains
        \domainName queries -> do
          (lift2 $ try $ getDomeinFile (DomeinFileId domainName)) >>=
            handleDomeinFileError "updateModel'"
            \(DomeinFile dfr) -> do
              -- Here we must take care to preserve the screens.js attachment.
              lift2 (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ queries removeInvertedQuery) dfr))
      -- Clear the caches of compiled states.
      void $ pure $ clearModelStates (DomeinFileId modelName)
      -- Install the new model, taking care of outgoing InvertedQueries.
      -- TODO. As soon as model identifiers are URLs, do not concatenate the url to the modelName.
      addModelToLocalStore' (url <> modelName)
      DomeinFile dfr <- lift2 $ getDomeinFile $ DomeinFileId modelName
      -- Find all models in use.
      models' <- lift2 (Models.modelsInUse >>= traverse getDomeinFile)
      -- For each model, look up in its invertedQueriesInOtherDomains those for this model (if any) and apply them.
      lift2 (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ models' \(DomeinFile{invertedQueriesInOtherDomains:invertedQueries}) ->
        forWithIndex_ invertedQueries
          \domainName queries -> if domainName == modelName
            then for_ queries addInvertedQuery
            else pure unit) dfr))

-- | Retrieve the model(s) from the url(s) and add them to the local couchdb installation.
-- | Load the dependencies first.
-- | Load the acompanying instances, too.
-- | Notice that the urls should be the full path to the relevant documents.
-- | We assume the url to be of the form http(s)://<some-domain>/repository/model:<some-model>
-- | This function is applied with `callEffect`. Accordingly, it will get the ContextInstance of the Action as second parameter.
-- TODO. Authentication at the repository urls.
addModelToLocalStore :: Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
addModelToLocalStore urls _ = addModelsToLocalStore_ urls

addModelsToLocalStore_ :: Array String -> MonadPerspectivesTransaction Unit
addModelsToLocalStore_ urls = for_ urls addModelToLocalStore'

addModelToLocalStore' :: String -> MonadPerspectivesTransaction Unit
addModelToLocalStore' url = do
  case namespaceFromUrl url of
    Nothing -> throwError (error $ "Repository URL does not end on model name: " <> url)
    Just ns -> do
      -- Retrieve the DomeinFile from the URL.
      repositoryUrl <- repository url
      docName <- documentName url
      df@(DomeinFile{_id, modelDescription, crl, indexedRoles, indexedContexts, referredModels, invertedQueriesInOtherDomains}) <- lift2 $ getDocument repositoryUrl docName
      -- Add new dependencies.
      for_ referredModels \dfid -> do
        mmodel <- lift $ lift $ tryGetPerspectEntiteit dfid
        case mmodel of
          Nothing -> addModelToLocalStore' (append repositoryUrl (unwrap dfid))
          Just _ -> pure unit
      -- Store the model in Couchdb.
      -- Fetch the local revision before saving: it belongs to the repository,
      -- not the local perspect_models.
      lift $ lift $ void $ cacheEntity (DomeinFileId _id) (changeRevision Nothing df)
      lift $ lift $ updateRevision(DomeinFileId _id)
      revision <- lift $ lift $ saveEntiteit (DomeinFileId _id) >>= pure <<< rev

      -- Add replacements to PerspectivesState for the new indexed names introduced in this model,
      -- unless we find existing ones left over from a previous installation of the model.
      (iroles :: Object RoleInstance) <- for indexedRoles (\iRole -> do
        -- iRole is the *indexed name* (not the unique name).
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

          -- Save role instances, overwriting versions left over from a previous installation.
          (cis :: Object PerspectContext) <- lift2 $ execStateT
            (forWithIndex_
              roleInstances'
              (\i newRole' -> do
                -- Fetch from the database, not cache.
                (mrole :: Maybe PerspectRol) <- lift $ tryFetchEntiteit (RoleInstance i)
                case mrole of
                  -- If we find a previous version in the database, overwrite it.
                  -- We will have handled bindings on the previous occasion.
                  -- NOTE. There may be a snag here. Suppose the previous version
                  -- did have a binding while the current does not? Then we'd need
                  -- to readjust the inverse binding administration.
                  -- Also, the new version may not have a binding while the old version does.
                  -- This may trigger automatic effects as indeed it does for state NotInIndexedContexts.
                  -- ME. This operation should preserve the value of `isMe`.
                  Just oldRole -> do
                    newRole <- pure $ changeRol_isMe newRole' (rol_isMe oldRole)
                    case rol_binding newRole of
                      Nothing -> case rol_binding oldRole of
                        -- Neither the old nor the new version have a binding.
                        Nothing -> saveRoleInstance i $ changeRevision (rev oldRole) newRole
                        -- The old role has a binding, the new one does not.
                        -- This code is a temporary solution. For now we choose to restore the binding relations.
                        -- As a consequence, a model update cannot undo binding relations on model instances.
                        -- Give the new role the binding the old role has.
                        Just oldBindingId -> do
                          saveRoleInstance i $ changeRevision (rev oldRole) (changeRol_binding oldBindingId newRole)
                      Just newBindingId -> case rol_binding oldRole of
                        -- New role has binding, old role does not. Update the inverse binding administration for the new binding, if it is not a model instance.
                        Nothing -> if (isJust $ lookup (unwrap newBindingId) roleInstances')
                          -- The binding comes with the other model instances and so must already have the inverse binding administration.
                          then saveRoleInstance i $ changeRevision (rev oldRole) newRole
                          -- The binding is on an instance outside the model. Add the inverse binding administration.
                          else (lift $ try $ getPerspectEntiteit newBindingId) >>=
                            handlePerspectRolError
                              "addModelToLocalStore'"
                              -- set the inverse binding if not already there.
                              \newBinding -> do
                                void $ lift $ cacheEntity newBindingId (addRol_gevuldeRollen newBinding (rol_pspType newRole) (RoleInstance i))
                                void $ lift $ saveEntiteit newBindingId
                                saveRoleInstance i $ changeRevision (rev oldRole) newRole
                        Just oldBindingId -> if newBindingId == oldBindingId
                          -- Old and new role both have the same binding. Nothing needs to be changed.
                          then saveRoleInstance i $ changeRevision (rev oldRole) newRole
                          -- New role has another binding than the old role. TODO.
                          else pure unit
                    -- As with the binding relation, we choose to restore the binder relations.
                    -- This means we cannot change them with a model update.
                    -- Find those binders on the old role that are not on the new role.
                    -- For each such binder, add it to the new role.
                    -- SHORTCUT. We just replace the binders of the new role with those of the old role.
                    -- Notice that this makes it impossible, too, to remove a binder relation between model instances.
                    -- As this is a temporary solution, that is acceptable.
                    -- Fetch the revision again, because it may have changed above.
                    updatedRole <- lift $ getPerspectRol (RoleInstance i)
                    lift $ void $ saveEntiteit_ (rol_id newRole) (changeRevision (rev updatedRole) (setRol_gevuldeRollen newRole (rol_gevuldeRollen oldRole)))

                  Nothing -> case rol_binding newRole' of
                    Nothing -> saveRoleInstance i newRole'
                    Just newBindingId -> if (isJust $ lookup (unwrap newBindingId) roleInstances')
                      then saveRoleInstance i newRole'
                      else (lift $ try $ getPerspectEntiteit newBindingId) >>=
                        handlePerspectRolError
                          "addModelToLocalStore'"
                          -- set the inverse binding if not already there.
                          \newBinding -> do
                            void $ lift $ cacheEntity newBindingId (addRol_gevuldeRollen newBinding (rol_pspType newRole') (RoleInstance i))
                            void $ lift $ saveEntiteit newBindingId
                            saveRoleInstance i newRole'
                            -- There can be no queries that use binder <type of a> on newBindingId, since the model is new.
                            -- So we need no action for QUERY UPDATES or RULE TRIGGERING.
            ))
            contextInstances

          -- Save context instances, but prefer a version left over from a previous installation.
          forWithIndex_ cis \(i :: String) newVersion -> do
            -- We *must* get the revision from the database. On doing an update, the context will be put into
            -- cache by the CRL parser. It then has no revision.
            moldCtxt <- lift $ lift $ tryFetchEntiteit (ContextInstance i)
            case moldCtxt of
              Nothing -> lift2 $ saveEntiteit_ (ContextInstance i) newVersion
              Just oldCtxt  -> do
                void $ lift2 $ cacheEntity (ContextInstance i) oldCtxt
                pure oldCtxt

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

    -- Sets the `me` property on the role instances. Detects the ultimate bottom case: the user instance of sys:PerspectivesSystem. Note that model user instances should never comprise particular other users!
    -- Saves the role instance both in cache and the database.
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

    documentName :: String -> MonadPerspectivesTransaction String
    documentName url' = case getFirstMatch (unsafeRegex "^.*/(.+)$" noFlags) url' of
      Nothing -> throwError (error ("Cannot get document name from " <> url'))
      Just s -> pure s

-- Returns string ending on forward slash (/).
repository :: String -> MonadPerspectivesTransaction String
repository url' = case getFirstMatch (unsafeRegex "^(.*/).+$" noFlags) url' of
  Nothing -> throwError (error ("Cannot get repository from " <> url'))
  Just s -> pure s

addInvertedQuery :: SeparateInvertedQuery -> State DomeinFileRecord Unit
addInvertedQuery = modifyInvertedQuery true

removeInvertedQuery :: SeparateInvertedQuery -> State DomeinFileRecord Unit
removeInvertedQuery = modifyInvertedQuery false

modifyInvertedQuery :: Boolean -> SeparateInvertedQuery -> State DomeinFileRecord Unit
modifyInvertedQuery add = modifyInvertedQuery'
  where
    modifyInvertedQuery' :: SeparateInvertedQuery -> State DomeinFileRecord Unit
    modifyInvertedQuery' (OnContextDelta_context embeddingContext typeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup typeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{onContextDelta_context}) -> dfr {enumeratedRoles = insert typeName (EnumeratedRole rr {onContextDelta_context =
          (if add then addInvertedQueryIndexedByContext else deleteInvertedQueryIndexedByContext)
            invertedQuery
            embeddingContext
            onContextDelta_context
          }) enumeratedRoles}

    modifyInvertedQuery' (OnContextDelta_role embeddingContext typeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup typeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{onContextDelta_role}) -> dfr {enumeratedRoles = insert typeName (EnumeratedRole rr {onContextDelta_role = (if add then addInvertedQueryIndexedByContext else deleteInvertedQueryIndexedByContext) invertedQuery embeddingContext onContextDelta_role}) enumeratedRoles}

    modifyInvertedQuery' (OnRoleDelta_binder embeddingContext typeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup typeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{onRoleDelta_binder}) -> dfr {enumeratedRoles = insert typeName (EnumeratedRole rr {onRoleDelta_binder = (if add then addInvertedQueryIndexedByContext else deleteInvertedQueryIndexedByContext) invertedQuery embeddingContext onRoleDelta_binder}) enumeratedRoles}

    modifyInvertedQuery' (OnRoleDelta_binding embeddingContext typeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup typeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{onRoleDelta_binding}) -> dfr {enumeratedRoles = insert typeName (EnumeratedRole rr {onRoleDelta_binding = (if add then addInvertedQueryIndexedByContext else deleteInvertedQueryIndexedByContext) invertedQuery embeddingContext onRoleDelta_binding}) enumeratedRoles}

    modifyInvertedQuery' (OnPropertyDelta eroleType typeName invertedQuery) = void $ modify \dfr@{enumeratedProperties} ->
      case lookup typeName enumeratedProperties of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedProperty rr@{onPropertyDelta}) -> dfr {enumeratedProperties = insert typeName (EnumeratedProperty rr {onPropertyDelta = if add
          then addInvertedQueryIndexedByRole invertedQuery eroleType onPropertyDelta
          else deleteInvertedQueryIndexedByRole invertedQuery eroleType onPropertyDelta}) enumeratedProperties}

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
  (newRev :: Revision_) <- addDocument url (changeRevision mVersion df) (show dfId)
  -- Now add the attachments.
  void $ execStateT (go attachments) newRev

  where
    -- As each attachment that we add will bump the document version, we have to catch it and use it on the
    -- next attachment.
    go :: Object (Tuple MediaType (Maybe String)) -> StateT Revision_ MonadPerspectives Unit
    go attachments = forWithIndex_ attachments \attName (Tuple mimetype mattachment) -> case mattachment of
      Nothing -> pure unit
      Just attachment -> do
        newRev <- get
        DeleteCouchdbDocument {rev} <- lift $ addAttachment url (show dfId) newRev attName attachment mimetype
        put rev

type URL = String

-- | The argument of type Array String contains a model identifier.
removeModelFromLocalStore :: Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
removeModelFromLocalStore rs _ = case head rs of
  Nothing -> pure unit
  Just r -> scheduleDomeinFileRemoval (DomeinFileId r)

scheduleDomeinFileRemoval :: DomeinFileId -> MonadPerspectivesTransaction Unit
scheduleDomeinFileRemoval id = lift $ AMA.modify (over Transaction \t@{modelsToBeRemoved} -> t {modelsToBeRemoved = cons id modelsToBeRemoved})

type Url = String
-- | The RoleInstance is an instance of CouchdbServer$Repositories.
-- | Fails silently if either the url or the name is missing.
-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
createCouchdbDatabase :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
createCouchdbDatabase databaseUrls databaseNames _ = case head databaseUrls, head databaseNames of
  -- NOTE: misschien moet er een slash tussen
  Just databaseUrl, Just databaseName -> lift2 $ CDB.createDatabase (databaseUrl <> databaseName)
  _, _ -> pure unit

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
deleteCouchdbDatabase :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
deleteCouchdbDatabase databaseUrls databaseNames _ = case head databaseUrls, head databaseNames of
  -- NOTE: misschien moet er een slash tussen
  Just databaseUrl, Just databaseName -> lift2 $ CDB.deleteDatabase (databaseUrl <> databaseName)
  _, _ -> pure unit

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
replicateContinuously :: Array Url -> Array String -> Array DatabaseName -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
replicateContinuously databaseUrls names sources targets _ = case head databaseUrls, head names, head sources, head targets of
  Just databaseUrl, Just name, Just source, Just target -> lift2 $ CDB.replicateContinuously
    databaseUrl
    name
    (databaseUrl <> source)
    (databaseUrl <> target)
    Nothing
  _, _, _, _ -> pure unit

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
endReplication :: Array Url -> Array DatabaseName -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
endReplication databaseUrls sources targets _ = case head databaseUrls, head sources, head targets of
  Just databaseUrl, Just source, Just target -> void $ lift2 $ CDB.endReplication databaseUrl source target
  _, _, _ -> pure unit

type UserName = String
type Password = String

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Accounts.
createUser :: Array Url -> Array UserName -> Array Password -> RoleInstance -> MonadPerspectivesTransaction Unit
createUser databaseUrls userNames passwords _ = case head databaseUrls, head userNames, head passwords of
  Just databaseurl, Just userName, Just password -> lift2 $ CDB.createUser databaseurl userName password []
  _, _, _ -> pure unit

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Accounts.
deleteUser :: Array Url -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
deleteUser databaseUrls userNames _ = case head databaseUrls, head userNames of
  Just databaseurl, Just userName -> lift2 $ void $ CDB.deleteUser databaseurl userName
  _, _ -> pure unit

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
updateSecurityDocument :: (String -> SecurityDocument -> SecurityDocument) -> Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
updateSecurityDocument updater databaseUrls databaseNames userNames _ = case head databaseUrls, head databaseNames, head userNames of
  Just databaseUrl, Just databaseName, Just userName -> lift2 $ do
    sdoc <- CDB.ensureSecurityDocument databaseUrl databaseName
    CDB.setSecurityDocument databaseUrl databaseName (updater userName sdoc)
  _, _, _ -> pure unit

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
makeAdminOfDb :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeAdminOfDb = updateSecurityDocument \userName (SecurityDocument r) -> SecurityDocument r {admins = {names: Just $ maybe [userName] (ARR.union [userName]) r.admins.names, roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
removeAsAdminFromDb :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
removeAsAdminFromDb = updateSecurityDocument \userName (SecurityDocument r) -> SecurityDocument r {admins = {names: ARR.delete userName <$> r.admins.names, roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
makeMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeMemberOf = updateSecurityDocument \userName (SecurityDocument r) -> SecurityDocument r {members = {names: Just (maybe [userName] (ARR.union [userName]) r.members.names), roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
removeAsMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
removeAsMemberOf = updateSecurityDocument \userName (SecurityDocument r) -> SecurityDocument r {members = {names: ARR.delete userName <$> r.members.names, roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Accounts
resetPassword :: Array Url -> Array UserName -> Array Password -> RoleInstance -> MonadPerspectivesTransaction Unit
resetPassword databaseUrls userNames passwords _ = case head databaseUrls, head userNames, head passwords of
  Just databaseUrl, Just userName, Just password -> lift2 $ CDB.setPassword databaseUrl userName password
  _, _, _ -> pure unit

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
  , Tuple "model:Couchdb$UpdateModel" {func: unsafeCoerce updateModel, nArgs: 3}
  , Tuple "model:Couchdb$CreateCouchdbDatabase" {func: unsafeCoerce createCouchdbDatabase, nArgs: 2}
  , Tuple "model:Couchdb$DeleteCouchdbDatabase" {func: unsafeCoerce deleteCouchdbDatabase, nArgs: 2}
  , Tuple "model:Couchdb$ReplicateContinuously" {func: unsafeCoerce replicateContinuously, nArgs: 4}
  , Tuple "model:Couchdb$EndReplication" {func: unsafeCoerce replicateContinuously, nArgs: 3}
  , Tuple "model:Couchdb$CreateUser" {func: unsafeCoerce createUser, nArgs: 3}
  , Tuple "model:Couchdb$DeleteUser" {func: unsafeCoerce deleteUser, nArgs: 2}
  , Tuple "model:Couchdb$MakeAdminOfDb" {func: unsafeCoerce makeAdminOfDb, nArgs: 3}
  , Tuple "model:Couchdb$RemoveAsAdminFromDb" {func: unsafeCoerce removeAsAdminFromDb, nArgs: 3}
  , Tuple "model:Couchdb$MakeMemberOf" {func: unsafeCoerce makeMemberOf, nArgs: 3}
  , Tuple "model:Couchdb$MakeMemberOf" {func: unsafeCoerce makeMemberOf, nArgs: 3}
  , Tuple "model:Couchdb$RemoveAsMemberOf" {func: unsafeCoerce removeAsMemberOf, nArgs: 3}
  , Tuple "model:Couchdb$ResetPassword" {func: unsafeCoerce resetPassword, nArgs: 3}
  ]
