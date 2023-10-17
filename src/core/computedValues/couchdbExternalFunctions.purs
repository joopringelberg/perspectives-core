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

-- | This module defines External Core functions for model://perspectives.domains#Couchdb.

module Perspectives.Extern.Couchdb where

import Control.Monad.AvarMonadAsk (modify) as AMA
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (State, StateT, execState, execStateT, get, modify, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (cons, foldl, head)
import Data.Array (union, delete) as ARR
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (over, unwrap)
import Data.String (Replacement(..), replace, Pattern(..))
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Generic (Foreign, encodeJSON)
import Foreign.Object (Object, empty, fromFoldable, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.StateCache (clearModelStates)
import Perspectives.Assignment.Update (addRoleInstanceToContext, cacheAndSave, getAuthor, getSubject)
import Perspectives.Authenticate (sign)
import Perspectives.ContextAndRole (changeRol_isMe, rol_context)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MP, MonadPerspectivesTransaction, MonadPerspectives, (##=))
import Perspectives.Couchdb (DatabaseName, DeleteCouchdbDocument(..), DocWithAttachmentInfo(..), SecurityDocument(..))
import Perspectives.Couchdb.Revision (Revision_, changeRevision, rev)
import Perspectives.Deltas (addCreatedContextToTransaction)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (getPatchAndBuild, getVersionToInstall, saveCachedDomeinFile, storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, SeparateInvertedQuery(..), addDownStreamAutomaticEffect, addDownStreamNotification, removeDownStreamAutomaticEffect, removeDownStreamNotification)
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (getFirstMatch, isModelUri, modelUri2ManifestUrl, modelUri2ModelUrl, modelUriVersion, newModelRegex, typeUri2LocalName_, unversionedModelUri)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.CreateContext (constructEmptyContext)
import Perspectives.Instances.CreateRole (constructEmptyRole)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.InvertedQuery (addInvertedQueryIndexedByContext, addInvertedQueryIndexedByRole, addInvertedQueryToPropertyIndexedByRole, deleteInvertedQueryFromPropertyTypeIndexedByRole, deleteInvertedQueryIndexedByContext, deleteInvertedQueryIndexedByRole)
import Perspectives.ModelDependencies as DEP
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (MonadPouchdb, addAttachment, addDocument, addDocument_, deleteDatabase, getAttachment, getDocument, getViewOnDatabase, retrieveDocumentVersion, splitRepositoryFileUrl, tryGetDocument_, withDatabase, DesignDocument(..))
import Perspectives.Persistence.API (deleteDocument) as Persistence
import Perspectives.Persistence.Authentication (addCredentials) as Authentication
import Perspectives.Persistence.CouchdbFunctions (addRoleToUser, concatenatePathSegments, removeRoleFromUser)
import Perspectives.Persistence.CouchdbFunctions as CDB
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (UserName, Password)
import Perspectives.Persistent (entitiesDatabaseName, getDomeinFile, getPerspectEntiteit, saveEntiteit_, tryGetPerspectEntiteit, updateRevision)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.Identifiable (identifier, identifier_)
import Perspectives.Representation.Context (Context(..)) as CTXT
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), addInvertedQueryIndexedByTripleKeys, deleteInvertedQueryIndexedByTripleKeys)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), ResourceType(..))
import Perspectives.ResourceIdentifiers (createDefaultIdentifier, createResourceIdentifier', stripNonPublicIdentifiers)
import Perspectives.RoleAssignment (filledPointsTo, fillerPointsTo, roleIsMe)
import Perspectives.SaveUserData (scheduleContextRemoval)
import Perspectives.SetupCouchdb (setContextView, setCredentialsView, setFilledRolesView, setPendingInvitationView, setRoleFromContextView, setRoleView)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..), stripResourceSchemes)
import Prelude (Unit, bind, const, discard, eq, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

modelsDatabaseName :: MonadPerspectives String
modelsDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_models")

-- | Retrieves all instances of a particular role type from Couchdb.
-- | For example: `user: Users = callExternal cdb:RoleInstances(sysUser) returns: model://perspectives.domains#System$PerspectivesSystem$User`
-- | Notice that only the first element of the array argument is actually used.
-- | Notice, too, that the second parameter is ignored. We must provide it, however, as the query compiler
-- | will give us an argument for it.
roleInstancesFromCouchdb :: Array String -> (ContextInstance ~~> RoleInstance)
roleInstancesFromCouchdb roleTypes _ = ArrayT do
  case head roleTypes of
    Nothing -> pure []
    Just rt -> do
      (tell $ ArrayWithoutDoubles [RoleAssumption (ContextInstance "def:AnyContext") (EnumeratedRoleType rt)])
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
  -- tell $ ArrayWithoutDoubles [RoleAssumption (ContextInstance "def:AnyContext") (EnumeratedRoleType rt)]
  (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/pendingInvitations" (Nothing :: Maybe Unit)

-- | Overwrites the model currently residing in the local models database.
-- | Takes care of inverted queries.
-- | Clears compiled states from cache.
-- | The first argument should contain the model name ("model://some.domain#Something@<SemVer>"), 
-- | The second argument should contain the string representation of a boolean value.
-- | The third argument is an array with an instance of the role ModelsInuse.
-- | If no SemVer is given, will try to load the unversioned model (if any).
updateModel :: Array String -> Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
updateModel arrWithModelName arrWithDependencies versions = case head arrWithModelName of
  -- fail silently
  Nothing -> pure unit
  -- TODO: add a check on the form of the modelName.
  Just modelName -> updateModel' (maybe false (eq "true") (head arrWithDependencies)) (DomeinFileId modelName)
  where
    updateModel' :: Boolean -> DomeinFileId -> MonadPerspectivesTransaction Unit
    updateModel' withDependencies (DomeinFileId modelName) = do
      {repositoryUrl, documentName} <- pure $ unsafePartial modelUri2ModelUrl modelName
      DomeinFile{invertedQueriesInOtherDomains, upstreamStateNotifications, upstreamAutomaticEffects, referredModels} <- lift $ getDocument repositoryUrl documentName
      if withDependencies
        then for_ referredModels (updateModel' withDependencies)
        else pure unit
        -- Untangle the InvertedQueries of the previous model.
      unversionedModelname <- pure $ unversionedModelUri modelName
      forWithIndex_ invertedQueriesInOtherDomains
        \domainName queries -> do
          (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
            handleDomeinFileError "updateModel'"
            \(DomeinFile dfr) -> do
              -- Here we must take care to preserve the screens.js attachment.
              lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ queries removeInvertedQuery) dfr))
      forWithIndex_ upstreamStateNotifications
        \domainName notifications -> do
          (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
            handleDomeinFileError "updateModel'"
            \(DomeinFile dfr) -> do
              -- Here we must take care to preserve the screens.js attachment.
              lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ notifications removeDownStreamNotification) dfr))
      forWithIndex_ upstreamAutomaticEffects
        \domainName automaticEffects -> do
          (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
            handleDomeinFileError "updateModel'"
            \(DomeinFile dfr) -> do
              -- Here we must take care to preserve the screens.js attachment.
              lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ automaticEffects removeDownStreamAutomaticEffect) dfr))
      -- Clear the caches of compiled states.
      void $ pure $ clearModelStates (DomeinFileId unversionedModelname)
      -- Install the new model, taking care of outgoing InvertedQueries.
      addModelToLocalStore (DomeinFileId modelName) isUpdate
      DomeinFile dfr <- lift $ getDomeinFile $ (DomeinFileId unversionedModelname)
      -- Find all models in use.
      models' <- lift (allModelsInUse >>= traverse getDomeinFile)
      -- For each model, look up in its invertedQueriesInOtherDomains those for this model (if any) and apply them.
      lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ models' \(DomeinFile{invertedQueriesInOtherDomains:invertedQueries}) ->
        forWithIndex_ invertedQueries
          \domainName queries -> if domainName == unversionedModelname
            then for_ queries addInvertedQuery
            else pure unit) dfr))
    
    allModelsInUse :: MonadPerspectives (Array DomeinFileId)
    allModelsInUse = do
      system <- getMySystem
      propertyGetter <- getDynamicPropertyGetter
        DEP.versionedDomeinFileName
        (ST $ EnumeratedRoleType DEP.modelsInUse)
      values <- (ContextInstance system) ##= (getEnumeratedRoleInstances (EnumeratedRoleType DEP.modelsInUse) >=> propertyGetter)
      pure $ DomeinFileId <<< unwrap <$> values


-- | Retrieve the model(s) from the modelName(s) and add them to the local couchdb installation.
-- | Load the dependencies first.
-- | This function is applied with `callEffect`. Accordingly, it will get the ContextInstance of the Action as second parameter.
-- | Requires the right to write to the Repository database (SERVERADMIN, DATABASEADMIN, WRITINGMEMBER)
addModelToLocalStore_ :: Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
addModelToLocalStore_ modelNames _ = for_ modelNames (addModelToLocalStore' <<< DomeinFileId)

addModelToLocalStore' :: DomeinFileId -> MonadPerspectivesTransaction Unit
addModelToLocalStore' dfid@(DomeinFileId domeinFileName) = if test newModelRegex domeinFileName 
  then addModelToLocalStore dfid isInitialLoad
  else throwError (error $ "Not a valid model name: " <> domeinFileName)

isUpdate :: Boolean
isUpdate = false

isInitialLoad :: Boolean
isInitialLoad = true

-- | Parameter `isUpdate` should be true iff the model has been added to the local installation before.
addModelToLocalStore :: DomeinFileId -> Boolean -> MonadPerspectivesTransaction Unit
addModelToLocalStore (DomeinFileId modelname) isInitialLoad' = do
  unversionedModelname <- pure $ unversionedModelUri modelname
  x :: (Maybe {semver :: String, versionedModelManifest :: RoleInstance}) <- lift $ getVersionToInstall unversionedModelname
  {patch, build} <- case x of 
    Nothing -> pure {patch: "0", build: "0"}
    Just {versionedModelManifest} -> lift $ getPatchAndBuild versionedModelManifest
  version <- case modelUriVersion modelname of
    Just v -> pure $ Just v
    Nothing -> pure $ _.semver <$> x
  -- If we can find a version at all, this is it.
  versionedModelName <- pure (unversionedModelname <> (maybe "" ((<>) "@") version))
  {repositoryUrl, documentName} <- pure $ unsafePartial modelUri2ModelUrl versionedModelName
  df@(DomeinFile
  { _id
  -- , indexedRoles
  -- , indexedContexts
  , referredModels
  , invertedQueriesInOtherDomains
  , upstreamStateNotifications
  , upstreamAutomaticEffects}) <- lift $ getDocument repositoryUrl documentName

  -- Store the model in Couchdb, that is: in the local store of models.
  -- Save it with the revision of the local version that we have, if any (do not use the repository version).
  lift $ void $ cacheEntity (DomeinFileId _id) (changeRevision Nothing df)
  revision <- lift $ saveCachedDomeinFile (DomeinFileId _id)  >>= pure <<< rev

  -- Copy the attachment
  lift $ addA repositoryUrl documentName revision

  if isInitialLoad'
    then do
      -- If and only if the model we load is model:System, create both the system context and the system user.
      -- This is part of the installation routine.
      if unversionedModelname == DEP.systemModelName
        then initSystem
        else pure unit
      
      -- Create the model instance
      cid <- pure $ createDefaultIdentifier ((unsafePartial modelUri2ManifestUrl unversionedModelname).manifestName <> "_modelRootContext")
      r <- runExceptT $ constructEmptyContext 
        (ContextInstance cid)
        unversionedModelname
        "model root context"
        (PropertySerialization empty)
        Nothing
      case r of 
        Left e -> logPerspectivesError (Custom (show e))
        Right (ctxt :: PerspectContext) -> do 
          lift $ void $ saveEntiteit_ (identifier ctxt) ctxt
          addCreatedContextToTransaction (identifier ctxt)

      -- Now create the Installer user role IN THE DOMAIN INSTANCE (it is cached automatically)
      -- What follows below is a simplified version of createAndAddRoleInstance. We cannot use that because
      -- it would introduce module imnport circularity.
      (installerRole :: PerspectRol) <- constructEmptyRole
        (ContextInstance cid)
        (EnumeratedRoleType DEP.installer)
        0
        (RoleInstance (cid <> "$" <> (typeUri2LocalName_ DEP.installer) <> "_0000"))

      -- Fill the installerRole with sys:Me (all these operations cache the roles that are involved).
      me <- RoleInstance <$> (lift $ getUserIdentifier)
      lift ((identifier installerRole) `filledPointsTo` me)
      lift (me `fillerPointsTo` (identifier installerRole))
      roleIsMe (identifier installerRole) (rol_context installerRole)

      subject <- getSubject
      author <- getAuthor
      delta@(RoleBindingDelta _) <- pure $ RoleBindingDelta
        { filled : (identifier installerRole)
        , filledType: (EnumeratedRoleType DEP.installer)
        , filler: Just me
        , fillerType: Just (EnumeratedRoleType DEP.sysUser)
        , oldFiller: Nothing
        , oldFillerType: Nothing
        , deltaType: SetFirstBinding
        , subject
        }
      signedDelta <- pure $ SignedDelta
        { author: stripNonPublicIdentifiers author
        , encryptedDelta: sign $ encodeJSON $ stripResourceSchemes $ delta}

      -- Retrieve the PerspectRol with accumulated modifications to add the binding delta.
      (installerRole' :: PerspectRol) <- lift $ getPerspectEntiteit (identifier installerRole)
      lift $ cacheAndSave (identifier installerRole) 
        (over PerspectRol (\rl -> rl {bindingDelta = Just signedDelta}) installerRole')

      -- And now add to the context.
      void $ addRoleInstanceToContext (ContextInstance cid) (EnumeratedRoleType DEP.installer) (Tuple (identifier installerRole) Nothing)

      if unversionedModelname == DEP.systemModelName
        then pure unit
        else do
          mySystem <- lift getMySystem
          -- When we update a model M, we search all ModelsInUse for inverted queries that apply to M, and reapply them.
          -- Therefore, the model we load here on demand should be in ModelsInUse.
          -- Create a role instance filled with the VersionedModelManifest.
          -- Add the versionedModelName as the value of the property ModelToRemove.
          -- Set the property InstalledPatch.
          void $ createAndAddRoleInstance (EnumeratedRoleType DEP.modelsInUse) mySystem
            (RolSerialization 
              { id: Nothing
              , properties: PropertySerialization (fromFoldable
                  [ Tuple DEP.modelToRemove [versionedModelName]
                  , Tuple DEP.installedPatch [patch]
                  , Tuple DEP.installedBuild [build]])
              , binding: unwrap <<< _.versionedModelManifest <$> x})

      -- Add new dependencies.
      for_ referredModels \dfid -> do
        mmodel <- lift $ tryGetPerspectEntiteit dfid
        case mmodel of
          Nothing -> addModelToLocalStore' dfid
          Just _ -> pure unit
    
    else pure unit

  -- Distribute the SeparateInvertedQueries over the other domains.
  forWithIndex_ invertedQueriesInOtherDomains
    \domainName queries -> do
      (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
        handleDomeinFileError "addModelToLocalStore'"
        \(DomeinFile dfr) -> do
          -- Here we must take care to preserve the screens.js attachment.
          lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ queries addInvertedQuery) dfr))
  
  -- Distribute upstream state notifications over the other domains.
  forWithIndex_ upstreamStateNotifications
    \domainName notifications -> do
      (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
        handleDomeinFileError "addModelToLocalStore'"
        \(DomeinFile dfr) -> do
          -- Here we must take care to preserve the screens.js attachment.
          lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ notifications addDownStreamNotification) dfr))

  -- Distribute upstream automatic effects over the other domains.
  forWithIndex_ upstreamAutomaticEffects
    \domainName automaticEffects -> do
      (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
        handleDomeinFileError "addModelToLocalStore'"
        \(DomeinFile dfr) -> do
          -- Here we must take care to preserve the screens.js attachment.
          lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ automaticEffects addDownStreamAutomaticEffect) dfr))

  where

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
    
    initSystem :: MonadPerspectivesTransaction Unit
    initSystem = do
      -- Create the model instance
      sysId <- lift getSystemIdentifier
      cid <- createResourceIdentifier' (CType $ ContextType DEP.theSystem) sysId
      r <- runExceptT $ constructEmptyContext 
        (ContextInstance cid)
        DEP.theSystem
        "My System"
        (PropertySerialization empty)
        Nothing
      case r of 
        Left e -> logPerspectivesError (Custom (show e))
        Right (system :: PerspectContext) -> do 
          lift $ void $ saveEntiteit_ (identifier system) system
          addCreatedContextToTransaction (identifier system)
          -- Now create the user role (it is cached automatically).
          -- What follows below is a simplified version of createAndAddRoleInstance. We cannot use that because
          -- it would introduce module imnport circularity.
          (me :: PerspectRol) <- constructEmptyRole
            (identifier system)
            (EnumeratedRoleType DEP.sysUser)
            0
            (RoleInstance (identifier_ system <> "$" <> (typeUri2LocalName_ DEP.sysUser)))
          lift $ void $ saveEntiteit_ (identifier me) (changeRol_isMe me true)
          -- The user role is not filled.
          -- And now add to the context.
          addRoleInstanceToContext (identifier system) (EnumeratedRoleType DEP.sysUser) (Tuple (identifier me) Nothing)
          void $ lift $ AMA.modify \ps -> ps 
            { indexedContexts = insert DEP.mySystem (identifier system) ps.indexedContexts
            , indexedRoles = insert DEP.sysMe (identifier me) ps.indexedRoles
            }

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
    -- Type of the context of the role instance; Type of the role instance to store on; InvertedQuery
    modifyInvertedQuery' :: SeparateInvertedQuery -> State DomeinFileRecord Unit
    modifyInvertedQuery' (ContextInvertedQuery embeddingContext roleTypeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup roleTypeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed roleTypeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{contextInvertedQueries}) -> dfr {enumeratedRoles = insert roleTypeName (EnumeratedRole rr {contextInvertedQueries = if add
          then addInvertedQueryIndexedByContext
            invertedQuery
            embeddingContext
            contextInvertedQueries
            -- On adding inverted queries in runtime (we're importing a model) we don't execute a synchronization
            -- test. Hence it is sufficient to just pass on an empty Array.
            []
            (EnumeratedRoleType roleTypeName)
          else deleteInvertedQueryIndexedByContext invertedQuery embeddingContext contextInvertedQueries
          }) enumeratedRoles}

    modifyInvertedQuery' (RoleInvertedQuery roleType contextTypeName invertedQuery) = void $ modify \dfr@{contexts} ->
      case lookup contextTypeName contexts of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        -- Just (Context cr@{}) -> dfr
        Just (CTXT.Context cr@{roleInvertedQueries}) ->
          dfr {contexts = insert
            contextTypeName
            (CTXT.Context cr {roleInvertedQueries = if add
              then addInvertedQueryIndexedByRole
                invertedQuery
                roleType
                roleInvertedQueries
                []
                (ContextType contextTypeName)
              else deleteInvertedQueryIndexedByRole invertedQuery roleType roleInvertedQueries
              })
            contexts}

    modifyInvertedQuery' (FilledInvertedQuery keys roleTypeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup roleTypeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{filledInvertedQueries}) -> dfr {enumeratedRoles = insert
          roleTypeName
          (EnumeratedRole rr {filledInvertedQueries = if add
            then addInvertedQueryIndexedByTripleKeys
              invertedQuery
              keys
              filledInvertedQueries
              []
              (EnumeratedRoleType roleTypeName)
            else deleteInvertedQueryIndexedByTripleKeys invertedQuery keys filledInvertedQueries})
          enumeratedRoles}

    modifyInvertedQuery' (FillerInvertedQuery keys roleTypeName invertedQuery) = void $ modify \dfr@{enumeratedRoles} ->
      case lookup roleTypeName enumeratedRoles of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedRole rr@{fillerInvertedQueries}) -> dfr {enumeratedRoles = insert
          roleTypeName
          (EnumeratedRole rr {fillerInvertedQueries = if add
            then addInvertedQueryIndexedByTripleKeys
              invertedQuery
              keys
              fillerInvertedQueries
              []
              (EnumeratedRoleType roleTypeName)
            else deleteInvertedQueryIndexedByTripleKeys invertedQuery keys fillerInvertedQueries})
          enumeratedRoles}

    modifyInvertedQuery' (OnPropertyDelta keys typeName invertedQuery) = void $ modify \dfr@{enumeratedProperties} ->
      case lookup typeName enumeratedProperties of
        -- It should be there! But it seems possible that the author of this model removed typeName
        -- after the author of the imported model referenced it.
        Nothing -> dfr
        Just (EnumeratedProperty pr@{onPropertyDelta}) -> dfr {enumeratedProperties = insert typeName
          (EnumeratedProperty pr { onPropertyDelta = foldl
            (\iqs roleType -> if add
              then addInvertedQueryToPropertyIndexedByRole
                invertedQuery
                roleType
                iqs
                Map.empty
                (EnumeratedPropertyType typeName)
              else deleteInvertedQueryFromPropertyTypeIndexedByRole invertedQuery roleType iqs)
            onPropertyDelta
            keys})
          enumeratedProperties }

-- | Take a DomeinFile from the local perspect_models database and upload it to the repository database.
-- | Notice that repositoryUrl is derived from the DomeinFileId.
-- | Attachments are preserved: if they were in the repository before uploading,
-- | they will be in the repository after uploading.
uploadToRepository :: DomeinFileId -> MonadPerspectives Unit
uploadToRepository dfId@(DomeinFileId domeinFileName) = do
  if isModelUri domeinFileName
    then do
      mdf <- try $ getPerspectEntiteit dfId
      case mdf of
        Left err -> logPerspectivesError $ DomeinFileErrorBoundary "uploadToRepository" (show err)
        Right df -> uploadToRepository_ (unsafePartial modelUri2ModelUrl domeinFileName) df
    else logPerspectivesError $ DomeinFileErrorBoundary "uploadToRepository" ("This modelURI is not well-formed: " <> domeinFileName)

-- | As uploadToRepository, but provide the DomeinFile as argument.
uploadToRepository_ :: {repositoryUrl :: String, documentName :: String} -> DomeinFile -> MonadPerspectives Unit
uploadToRepository_ splitName df = do 
  -- Get the attachment info
  (atts :: Maybe DocWithAttachmentInfo) <- tryGetDocument_ splitName.repositoryUrl splitName.documentName
  attachments <- case atts of
    Nothing -> pure empty
    Just (DocWithAttachmentInfo {_attachments}) -> traverseWithIndex
      (\attName {content_type} -> Tuple (MediaType content_type) <$> getAttachment splitName.repositoryUrl splitName.documentName attName)
      _attachments
  -- Get the revision (if any) from the remote database, so we can overwrite.
  (mVersion :: Maybe String) <- retrieveDocumentVersion splitName.repositoryUrl splitName.documentName
  (newRev :: Revision_) <- addDocument splitName.repositoryUrl (changeRevision mVersion df) splitName.documentName
  -- Now add the attachments.
  void $ execStateT (go splitName.repositoryUrl splitName.documentName attachments) newRev

  where
    -- As each attachment that we add will bump the document version, we have to catch it and use it on the
    -- next attachment.
    go :: URL -> String -> Object (Tuple MediaType (Maybe Foreign)) -> StateT Revision_ MonadPerspectives Unit
    go documentUrl documentName attachments = forWithIndex_ attachments \attName (Tuple mimetype mattachment) -> case mattachment of
      Nothing -> pure unit
      Just attachment -> do
        newRev <- get
        DeleteCouchdbDocument {rev} <- lift $ addAttachment documentUrl documentName newRev attName attachment mimetype
        put rev

removeFromRepository_ :: {repositoryUrl :: String, documentName :: String} -> MonadPerspectives Boolean
removeFromRepository_ splitName = Persistence.deleteDocument splitName.repositoryUrl splitName.documentName Nothing

type URL = String

-- | The argument may be a versioned or unversioned modelURI, e.g. model://perspectives.domains#System@1.1.0
removeModelFromLocalStore :: Array String ->  RoleInstance -> MonadPerspectivesTransaction Unit
removeModelFromLocalStore versionedModelURIA rid = case head versionedModelURIA of 
  Just versionedModelURI -> do 
    let unversionedURI = unversionedModelUri versionedModelURI
    let cid = createDefaultIdentifier ((unsafePartial modelUri2ManifestUrl unversionedURI).manifestName <> "_modelRootContext")
    scheduleContextRemoval Nothing (ContextInstance cid)
    scheduleDomeinFileRemoval (DomeinFileId unversionedURI)
  _ -> pure unit

scheduleDomeinFileRemoval :: DomeinFileId -> MonadPerspectivesTransaction Unit
scheduleDomeinFileRemoval id = AMA.modify (over Transaction \t@{modelsToBeRemoved} -> t {modelsToBeRemoved = cons id modelsToBeRemoved})

type Url = String
-- | The RoleInstance is an instance of CouchdbServer$Repositories.
-- | Fails silently if either the url or the name is missing.
-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
-- | Execution of this function requires the user to have a SERVERADMIN account.
createCouchdbDatabase :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
createCouchdbDatabase databaseUrls databaseNames _ = case head databaseUrls, head databaseNames of
  -- NOTE: misschien moet er een slash tussen
  Just databaseUrl, Just databaseName -> lift $ withDatabase (databaseUrl <> databaseName) (pure <<< const unit)
  _, _ -> pure unit

-- | Create a database with all views that are useful for retrieving role- and context instances
createEntitiesDatabase  :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
createEntitiesDatabase databaseUrls databaseNames _ = case head databaseUrls, head databaseNames of
  -- NOTE: misschien moet er een slash tussen
  Just databaseUrl, Just databaseName -> lift $ withDatabase (databaseUrl <> databaseName) 
    (\_ -> do
      dbName <- pure (databaseUrl <> databaseName)
      setRoleView dbName
      setRoleFromContextView dbName
      setPendingInvitationView dbName
      setContextView dbName
      setCredentialsView dbName
      setFilledRolesView dbName
      )
  _, _ -> pure unit


-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
-- | Execution of this function requires the user to have a SERVERADMIN account.
deleteCouchdbDatabase :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
deleteCouchdbDatabase databaseUrls databaseNames _ = case head databaseUrls, head databaseNames of
  -- NOTE: misschien moet er een slash tussen
  Just databaseUrl, Just databaseName -> lift $ deleteDatabase (databaseUrl <> databaseName)
  _, _ -> pure unit

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
replicateContinuously :: Array Url -> Array Url -> Array DatabaseName -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
replicateContinuously databaseUrls couchdbUrls sources targets _ = case head databaseUrls, head couchdbUrls, head sources, head targets of
  Just databaseUrl, Just couchdbUrl, Just source, Just target -> do 
    -- The replication may have been configured before. Overwriting the document will lead to unexpected results.
    -- Stop replication first, then reconfigure.
    void $ lift $ CDB.endReplication databaseUrl source target
    lift $ CDB.replicateContinuously
      databaseUrl
      (source <> "_" <> target)
      (couchdbUrl <> source)
      (couchdbUrl <> target)
      Nothing
  _, _, _, _ -> pure unit

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
endReplication :: Array Url -> Array DatabaseName -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
endReplication databaseUrls sources targets _ = case head databaseUrls, head sources, head targets of
  Just databaseUrl, Just source, Just target -> void $ lift $ CDB.endReplication databaseUrl source target
  _, _, _ -> pure unit

deleteDocument :: Array Url -> RoleInstance -> MonadPerspectivesTransaction Unit
deleteDocument url_ _ = case head url_ of
  Just url -> case splitRepositoryFileUrl url of
    Nothing -> pure unit
    Just {database, document} -> lift $ void $ Persistence.deleteDocument database document Nothing
  _ -> pure unit

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Accounts.
-- | Execution of this function requires the user to have a SERVERADMIN account.
createUser :: Array Url -> Array UserName -> Array Password -> RoleInstance -> MonadPerspectivesTransaction Unit
createUser databaseUrls userNames passwords _ = case head databaseUrls, head userNames, head passwords of
  Just databaseurl, Just userName, Just password -> lift $ CDB.createUser databaseurl userName password []
  _, _, _ -> pure unit

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Accounts.
-- | Execution of this function requires the user to have a SERVERADMIN account.
deleteUser :: Array Url -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
deleteUser databaseUrls userNames _ = case head databaseUrls, head userNames of
  Just databaseurl, Just userName -> lift $ void $ CDB.deleteUser databaseurl userName
  _, _ -> pure unit

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
updateSecurityDocument :: (String -> SecurityDocument -> SecurityDocument) -> Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
updateSecurityDocument updater databaseUrls databaseNames userNames _ = case head databaseUrls, head databaseNames, head userNames of
  Just databaseUrl, Just databaseName, Just userName -> lift $ do
    sdoc <- CDB.ensureSecurityDocument databaseUrl databaseName
    CDB.setSecurityDocument databaseUrl databaseName (updater userName sdoc)
  _, _, _ -> pure unit

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
makeAdminOfDb :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeAdminOfDb = updateSecurityDocument \userName (SecurityDocument r) -> SecurityDocument r {admins = {names: Just $ maybe [userName] (ARR.union [userName]) r.admins.names, roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
removeAsAdminFromDb :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
removeAsAdminFromDb = updateSecurityDocument \userName (SecurityDocument r) -> SecurityDocument r {admins = {names: ARR.delete userName <$> r.admins.names, roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
makeMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeMemberOf = updateSecurityDocument \userName (SecurityDocument r) -> SecurityDocument r {members = {names: Just (maybe [userName] (ARR.union [userName]) r.members.names), roles: r.members.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
removeAsMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
removeAsMemberOf = updateSecurityDocument 
  \userName (SecurityDocument r) -> 
    SecurityDocument r 
      { members = 
        { names: ARR.delete userName <$> r.members.names
        , roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a SERVERADMIN account.
-- | Adds the database name as role name to the user document.
makeWritingMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeWritingMemberOf databaseUrls databaseNames userNames _ = case head databaseUrls, head databaseNames, head userNames of
  Just databaseUrl, Just databaseName, Just userName -> lift $ addRoleToUser databaseUrl userName databaseName
  _, _, _ -> pure unit

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a SERVERADMIN account.
-- | Removes the database name as role name from the user document.
removeAsWritingMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
removeAsWritingMemberOf databaseUrls databaseNames userNames _ = case head databaseUrls, head databaseNames, head userNames of
  Just databaseUrl, Just databaseName, Just userName -> lift $ removeRoleFromUser databaseUrl userName databaseName
  _, _, _ -> pure unit

-- | Any user can read the documents (and write them too, though we can restrict this by applying makeDatabaseWriteProtected).
-- | This involves removing both the `names` and the `roles` field from the members section (the admin section need not be changed).
-- | First execution of this function requires the user to have SERVERADMIN account.
-- | After that, a DATABASEADMIN may change it.
makeDatabasePublic :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeDatabasePublic databaseUrls databaseNames roleId = lift $
  case head databaseUrls, head databaseNames of
    Just databaseUrl, Just databaseName -> do 
      SecurityDocument sdoc <- CDB.ensureSecurityDocument databaseUrl databaseName
      CDB.setSecurityDocument 
        databaseUrl 
        databaseName 
        (SecurityDocument $ sdoc { members = 
          { names: Just []
          , roles: []}})
    _, _ -> pure unit

-- | Only users who have a role that equals the name of the database can create, update or delete documents in it.
-- | This involves adding a validate_doc_update function.
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
makeDatabaseWriteProtected :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeDatabaseWriteProtected databaseUrls databaseNames roleId = lift $
  case head databaseUrls, head databaseNames of
    Just databaseUrl, Just databaseName -> void $ ensureDesignDoc (databaseUrl `concatenatePathSegments` databaseName) "writeprotection" (replace (Pattern "$$DATABASENAME$$") (Replacement databaseName) validate_doc_update)
    _, _ -> pure unit

  where  
  -- This is a special version of ensureDesignDoc, created to add a validate_doc_update function.
  ensureDesignDoc :: forall f. DatabaseName -> DocumentName -> String -> MonadPouchdb f Revision_
  ensureDesignDoc dbName docname functionText = do
    (mddoc :: Maybe DesignDocument) <- tryGetDocument_ dbName ("_design/" <> docname)
    case mddoc of
      Nothing -> addDocument_ dbName 
        (DesignDocument
          { _id: "_design/" <> docname
          , _rev: Nothing
          , views: empty
          , validate_doc_update: Just functionText
          })
        ("_design/" <> docname)
      Just (DesignDocument ddoc) -> addDocument_ 
        dbName 
        (DesignDocument ddoc {validate_doc_update = Just functionText})
        ("_design/" <> docname)

type DocumentName = String

-- | Import the update function as a String.
-- | Customize for a given database by replacing the string $$DATABASENAME$$ with the actual database name.
foreign import validate_doc_update :: String

-- | The RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Accounts
resetPassword :: Array Url -> Array UserName -> Array Password -> RoleInstance -> MonadPerspectivesTransaction Unit
resetPassword databaseUrls userNames passwords _ = case head databaseUrls, head userNames, head passwords of
  Just databaseUrl, Just userName, Just password -> lift $ CDB.setPassword databaseUrl userName password
  _, _, _ -> pure unit

-- | Add credentials to the current session. Once persisted in the User's local storage, they will be retrieved on each session.
-- | Notice that this function causes a change in PerspectivesState but not in the Perspectives Universe.
addCredentials :: Array Url -> Array UserName -> Array Password -> RoleInstance -> MonadPerspectivesTransaction Unit
addCredentials urls usernames passwords _ = case head urls, head usernames, head passwords of 
  Just url, Just username, Just password -> lift $ Authentication.addCredentials url username password
  _, _, _ -> pure unit

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ 
  -- SERVERADMIN
    Tuple "model://perspectives.domains#Couchdb$CreateCouchdbDatabase" {func: unsafeCoerce createCouchdbDatabase, nArgs: 2, isFunctional: True}    
  , Tuple "model://perspectives.domains#Couchdb$createEntitiesDatabase" {func: unsafeCoerce createEntitiesDatabase, nArgs: 2, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$DeleteCouchdbDatabase" {func: unsafeCoerce deleteCouchdbDatabase, nArgs: 2, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$CreateUser" {func: unsafeCoerce createUser, nArgs: 3, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$DeleteUser" {func: unsafeCoerce deleteUser, nArgs: 2, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$ResetPassword" {func: unsafeCoerce resetPassword, nArgs: 3, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$MakeWritingMemberOf" {func: unsafeCoerce makeWritingMemberOf, nArgs: 3, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$RemoveAsWritingMemberOf" {func: unsafeCoerce removeAsWritingMemberOf, nArgs: 3, isFunctional: True}
  -- DATABASEADMIN
  , Tuple "model://perspectives.domains#Couchdb$MakeDatabasePublic" {func: unsafeCoerce makeDatabasePublic, nArgs: 2, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$MakeDatabaseWriteProtected" {func: unsafeCoerce makeDatabaseWriteProtected, nArgs: 2, isFunctional: True}  
  , Tuple "model://perspectives.domains#Couchdb$MakeAdminOfDb" {func: unsafeCoerce makeAdminOfDb, nArgs: 3, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$RemoveAsAdminFromDb" {func: unsafeCoerce removeAsAdminFromDb, nArgs: 3, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$MakeMemberOf" {func: unsafeCoerce makeMemberOf, nArgs: 3, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$RemoveAsMemberOf" {func: unsafeCoerce removeAsMemberOf, nArgs: 3, isFunctional: True}
  -- WRITINGMEMBER
  , Tuple "model://perspectives.domains#Couchdb$DeleteDocument" {func: unsafeCoerce deleteDocument, nArgs: 1, isFunctional: True}
  -- Requires writing to the _replicator database
  , Tuple "model://perspectives.domains#Couchdb$ReplicateContinuously" {func: unsafeCoerce replicateContinuously, nArgs: 4, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$EndReplication" {func: unsafeCoerce endReplication, nArgs: 3, isFunctional: True}
  -- Requires writing to the users model database.
  , Tuple "model://perspectives.domains#Couchdb$AddModelToLocalStore" {func: unsafeCoerce addModelToLocalStore_, nArgs: 1, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$RemoveModelFromLocalStore" {func: unsafeCoerce removeModelFromLocalStore, nArgs: 1, isFunctional: True}
  , Tuple "model://perspectives.domains#Couchdb$UpdateModel" {func: unsafeCoerce updateModel, nArgs: 2, isFunctional: True}

  -- These functions require read access to the users instances databases.
  , Tuple "model://perspectives.domains#Couchdb$RoleInstances" {func: unsafeCoerce roleInstancesFromCouchdb, nArgs: 1, isFunctional: Unknown}
  , Tuple "model://perspectives.domains#Couchdb$ContextInstances" {func: unsafeCoerce contextInstancesFromCouchdb, nArgs: 1, isFunctional: Unknown}
  , Tuple "model://perspectives.domains#Couchdb$PendingInvitations" {func: unsafeCoerce pendingInvitations, nArgs: 0, isFunctional: Unknown}
  -- This requires no access to Couchdb.
  , Tuple "model://perspectives.domains#Couchdb$AddCredentials" {func: unsafeCoerce addCredentials, nArgs: 3, isFunctional: True}

  ]
