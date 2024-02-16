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

import Control.Monad.AvarMonadAsk (modify, gets) as AMA
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (State, execState, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (cons, filter, foldl, head, union)
import Data.Array (union, delete) as ARR
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Iterable (toArray)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (over, unwrap)
import Data.Nullable (toMaybe)
import Data.String (Replacement(..), replace, Pattern(..))
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (read)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (empty, fromFoldable, insert, lookup, singleton)
import LRUCache (rvalues)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.StateCache (clearModelStates)
import Perspectives.Assignment.Update (addRoleInstanceToContext, cacheAndSave, getAuthor, getSubject)
import Perspectives.Authenticate (getMyPublicKey, signDelta)
import Perspectives.ContextAndRole (context_id, rol_context, rol_id)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MonadPerspectives, MonadPerspectivesTransaction, (##=))
import Perspectives.Couchdb (DatabaseName, SecurityDocument(..))
import Perspectives.Couchdb.Revision (Revision_, rev)
import Perspectives.Deltas (addCreatedContextToTransaction)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (getPatchAndBuild, getVersionToInstall, saveCachedDomeinFile, storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, SeparateInvertedQuery(..), addDownStreamAutomaticEffect, addDownStreamNotification, removeDownStreamAutomaticEffect, removeDownStreamNotification)
import Perspectives.Error.Boundaries (handleDomeinFileError, handleExternalFunctionError, handleExternalStatementError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (getFirstMatch, modelUri2ManifestUrl, modelUri2ModelUrl, modelUriVersion, newModelRegex, typeUri2LocalName_, unversionedModelUri)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance, createAndAddRoleInstance_)
import Perspectives.Instances.CreateContext (constructEmptyContext)
import Perspectives.Instances.CreateRole (constructEmptyRole)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.InvertedQuery (addInvertedQueryIndexedByContext, addInvertedQueryIndexedByRole, addInvertedQueryToPropertyIndexedByRole, deleteInvertedQueryFromPropertyTypeIndexedByRole, deleteInvertedQueryIndexedByContext, deleteInvertedQueryIndexedByRole)
import Perspectives.ModelDependencies (perspectivesUsersPublicKey)
import Perspectives.ModelDependencies as DEP
import Perspectives.Names (getMySystem, getPerspectivesUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (DesignDocument(..), MonadPouchdb, addDocument_, deleteDatabase, getAttachment, getDocument, getViewOnDatabase, splitRepositoryFileUrl, tryGetDocument_, withDatabase)
import Perspectives.Persistence.API (deleteDocument) as Persistence
import Perspectives.Persistence.Authentication (addCredentials) as Authentication
import Perspectives.Persistence.CouchdbFunctions (addRoleToUser, concatenatePathSegments, removeRoleFromUser)
import Perspectives.Persistence.CouchdbFunctions as CDB
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (UserName, Password)
import Perspectives.Persistent (addAttachment) as P
import Perspectives.Persistent (entitiesDatabaseName, getDomeinFile, getPerspectEntiteit, saveEntiteit_, saveMarkedResources, tryGetPerspectEntiteit)
import Perspectives.PerspectivesState (contextCache, roleCache)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Context (Context(..)) as CTXT
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), addInvertedQueryIndexedByTripleKeys, deleteInvertedQueryIndexedByTripleKeys)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), ResourceType(..))
import Perspectives.ResourceIdentifiers (createDefaultIdentifier, createResourceIdentifier', resourceIdentifier2WriteDocLocator)
import Perspectives.RoleAssignment (filledPointsTo, fillerPointsTo, roleIsMe)
import Perspectives.SaveUserData (scheduleContextRemoval)
import Perspectives.SetupCouchdb (contextViewFilter, roleViewFilter, setContextView, setCredentialsView, setFilledRolesView, setPendingInvitationView, setRoleFromContextView, setRoleView)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..), stripResourceSchemes)
import Prelude (Unit, bind, const, discard, eq, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=))
import Simple.JSON (writeJSON)
import Unsafe.Coerce (unsafeCoerce)

modelsDatabaseName :: MonadPerspectives String
modelsDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_models")

-- | Retrieves all instances of a particular role type from Couchdb. Instances that have the type as aspect are returned as well!
-- | For example: `user: Users = callExternal cdb:RoleInstances(sysUser) returns: model://perspectives.domains#System$PerspectivesSystem$User`
-- | Notice that only the first element of the array argument is actually used.
-- | Notice, too, that the second parameter is ignored. We must provide it, however, as the query compiler
-- | will give us an argument for it.
roleInstancesFromCouchdb :: Array String -> (ContextInstance ~~> RoleInstance)
roleInstancesFromCouchdb roleTypes _ = try 
  (ArrayT do
    case head roleTypes of
      Nothing -> pure []
      Just rt -> do
        (tell $ ArrayWithoutDoubles [RoleAssumption (ContextInstance "def:AnyContext") (EnumeratedRoleType rt)])
        instancesInCouchdb <- (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/roleView" (head roleTypes)
        instancesInCache <- lift (do
          cache <- roleCache
          cachedRoleAvars <- liftAff $ liftEffect (rvalues cache >>= toArray)
          cachedRoles <- lift $ traverse read cachedRoleAvars
          pure $ rol_id <$> filter (roleViewFilter $ EnumeratedRoleType rt) cachedRoles
          )
        pure $ instancesInCouchdb `union` instancesInCache)    
  >>= handleExternalFunctionError "model://perspectives.domains#Couchdb$RoleInstances"

contextInstancesFromCouchdb :: Array String -> (RoleInstance ~~> ContextInstance)
contextInstancesFromCouchdb contextTypeArr _ = try 
  (ArrayT do
    case head contextTypeArr of
      Nothing -> pure []
      Just ct -> do
        -- push assumption!
        instancesInCouchdb <- (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/contextView" (head contextTypeArr)
        instancesInCache <- lift (do
          cache <- contextCache
          cachedContextAvars <- liftAff $ liftEffect (rvalues cache >>= toArray)
          cachedContexts <- lift $ traverse read cachedContextAvars
          pure $ context_id <$> filter (contextViewFilter $ ContextType ct) cachedContexts
          )
        pure $ instancesInCouchdb `union` instancesInCache)
  >>= handleExternalFunctionError "model://perspectives.domains#Couchdb$ContextInstances"

pendingInvitations :: ContextInstance ~~> RoleInstance
pendingInvitations _ = try 
  (ArrayT $ lift do
    db <- entitiesDatabaseName
    filledRolesInDatabase :: Array RoleInstance <- getViewOnDatabase db "defaultViews/roleView" (Just DEP.invitation)
    filledRolesInCache :: Array RoleInstance <- (do 
      cache <- roleCache
      cachedRoleAvars <- liftAff $ liftEffect $ (rvalues cache >>= toArray)
      cachedRoles <- lift $ traverse read cachedRoleAvars
      pure $ rol_id <$> filter (roleViewFilter $ EnumeratedRoleType DEP.invitation) cachedRoles
      )
    pure $ filledRolesInDatabase `union` filledRolesInCache)
  >>= handleExternalFunctionError "model://perspectives.domains#Couchdb$PendingInvitations"
  
-- | Overwrites the model currently residing in the local models database.
-- | Takes care of inverted queries.
-- | Clears compiled states from cache.
-- | The first argument should contain the model name ("model://some.domain#Something@<SemVer>"), 
-- | The second argument should contain the string representation of a boolean value.
-- | The third argument is an array with an instance of the role ModelsInuse.
-- | If no SemVer is given, will try to load the unversioned model (if any).
updateModel :: Array String -> Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
updateModel arrWithModelName arrWithDependencies versions = try 
  (case head arrWithModelName of
    -- fail silently
    Nothing -> pure unit
    -- TODO: add a check on the form of the modelName.
    Just modelName -> updateModel' (maybe false (eq "true") (head arrWithDependencies)) (DomeinFileId modelName))
  >>= handleExternalStatementError "model://perspectives.domains#UpdateModel"

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
      models' <- lift (allModelsInUse >>= traverse getDomeinFile) -- MODELS IS LEEG
      -- For each model, look up in its invertedQueriesInOtherDomains those for this model (if any) and apply them.
      -- These are the 'incoming' InvertedQueries: those that are attached to types of this model.
      lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ models' \(DomeinFile{invertedQueriesInOtherDomains:invertedQueries}) ->
        forWithIndex_ invertedQueries
          \domainName queries -> if domainName == unversionedModelname
            then for_ queries addInvertedQuery
            else pure unit) dfr))
    
    allModelsInUse :: MonadPerspectives (Array DomeinFileId)
    allModelsInUse = do
      system <- getMySystem
      propGetter <- getDynamicPropertyGetter DEP.modelURI (ST (EnumeratedRoleType DEP.modelsInUse))
      values <- (ContextInstance system) ##= (getEnumeratedRoleInstances (EnumeratedRoleType DEP.modelsInUse) >=> propGetter)
      pure $ DomeinFileId <<< unwrap <$> values


-- | Retrieve the model(s) from the modelName(s) and add them to the local couchdb installation.
-- | Load the dependencies first.
-- | This function is applied with `callEffect`. Accordingly, it will get the ContextInstance of the Action as second parameter.
-- | Requires the right to write to the Repository database (SERVERADMIN, DATABASEADMIN, WRITINGMEMBER)
addModelToLocalStore_ :: Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
addModelToLocalStore_ modelNames _ = try (for_ modelNames (addModelToLocalStore' <<< DomeinFileId))
  >>= handleExternalStatementError "model://perspectives.domains#AddModelToLocalStore"

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
  x :: (Maybe {semver :: String, versionedModelManifest :: RoleInstance}) <- lift $ getVersionToInstall (DomeinFileId unversionedModelname)
  {patch, build} <- case x of 
    Nothing -> pure {patch: "0", build: "0"}
    Just {versionedModelManifest} -> lift $ getPatchAndBuild versionedModelManifest
  -- TODO. msversion is geen Maybe waarde!
  msversion <- lift (toMaybe <$> AMA.gets (_.useSystemVersion <<< _.runtimeOptions))
  version' <- case modelUriVersion modelname of
      Just v -> pure $ Just v
      Nothing -> pure $ _.semver <$> x
  version <- if unversionedModelname == DEP.systemModelName
    then case msversion of
      Nothing -> pure version'
      Just v -> pure $ Just v
    else pure version'
  -- If we can find a version at all, this is it.
  versionedModelName <- pure (unversionedModelname <> (maybe "" ((<>) "@") version))
  {repositoryUrl, documentName} <- pure $ unsafePartial modelUri2ModelUrl versionedModelName
  DomeinFile dfrecord@
    { id
    -- , indexedRoles
    -- , indexedContexts
    , referredModels
    , invertedQueriesInOtherDomains
    , upstreamStateNotifications
    , upstreamAutomaticEffects} <- lift $ getDocument repositoryUrl documentName

  -- Store the model in Couchdb, that is: in the local store of models.
  -- Save it with the revision of the local version that we have, if any (do not use the repository version).
  {documentName:unversionedDocumentName} <- lift $ resourceIdentifier2WriteDocLocator unversionedModelname
  lift $ void $ cacheEntity id (DomeinFile dfrecord { _rev = Nothing, _id = unversionedDocumentName})
  -- saveCachedDomeinFile takes care of revisions.
  revision <- lift $ saveCachedDomeinFile id >>= pure <<< rev

  -- Copy the attachment
  mAttachment <- lift $ getAttachment repositoryUrl documentName "screens.js"
  case mAttachment of
    Nothing -> pure unit
    Just attachment -> lift $ void $ P.addAttachment (DomeinFileId modelname) "screens.js" attachment (MediaType "text/ecmascript")


  if isInitialLoad'
    then do 
      createInitialInstances unversionedModelname versionedModelName patch build (_.versionedModelManifest <$> x)
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

createInitialInstances :: String -> String -> String -> String -> Maybe RoleInstance -> MonadPerspectivesTransaction Unit
createInitialInstances unversionedModelname versionedModelName patch build versionedModelManifest = do
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
  -- TODO: refactor. We can now actually use createAndAddRoleInstance!!
  (installerRole :: PerspectRol) <- constructEmptyRole
    (ContextInstance cid)
    (EnumeratedRoleType DEP.installer)
    0
    (RoleInstance (cid <> "$" <> (typeUri2LocalName_ DEP.installer) <> "_0000"))

  -- Fill the installerRole with the PerspectivesUser that represents the end user (all these operations cache the roles that are involved).
  me <- lift $ getPerspectivesUser
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
  signedDelta <- lift $ signDelta author (writeJSON $ stripResourceSchemes $ delta)

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
          , binding: unwrap <$> versionedModelManifest})

initSystem :: MonadPerspectivesTransaction Unit
initSystem = do
  lift $ saveMarkedResources
  -- Create the system instance (the instance of sys:PerspectivesSystem for this installation).
  -- This will also create an instance of IndexedContext in the new system instance, filled with itself.
  sysId <- lift getSystemIdentifier
  sysresult <- runExceptT $ constructContext Nothing 
    (ContextSerialization
      { id: Just sysId
      , prototype: Nothing
      , ctype: DEP.theSystem
      , rollen: empty
      , externeProperties: (PropertySerialization empty)
      } ) 
  case sysresult of
    Left se -> logPerspectivesError (Custom (show se))
    Right system@(ContextInstance systemId) -> do 
      -- Now create the user role (the instance of sys:PerspectivesSystem$User; it is cached automatically).
      -- This will also create the IndexedRole in the System instance, filled with the new User instance.
      userId <- createResourceIdentifier' (RType $ EnumeratedRoleType DEP.sysUser) (sysId <> "$" <> (typeUri2LocalName_ DEP.sysUser))
      mme <- createAndAddRoleInstance_ (EnumeratedRoleType DEP.sysUser) systemId
        (RolSerialization 
          { id: Just userId
          , properties: PropertySerialization empty
          , binding: Nothing
          })
        true
      case mme of 
        Nothing -> logPerspectivesError (Custom "Could not create User role of PerspectivesSystem.")
        Just me -> roleIsMe me system
      isFirstInstallation <- lift $ AMA.gets (_.isFirstInstallation <<< _.runtimeOptions)
      if isFirstInstallation
        then do
          mpublicKey <- lift getMyPublicKey
          case mpublicKey of 
            Just publicKey -> do
              -- Create TheWorld, complete with the PerspectivesUser role of TheWorld that represents the identity 
              -- of the natural person setting up this installation.
              worldresult <- runExceptT $ constructContext Nothing 
                (ContextSerialization
                  { id: Just "TheWorld"
                  , prototype: Nothing
                  , ctype: DEP.theWorld
                  , rollen: empty
                  , externeProperties: (PropertySerialization empty)
                  } )
              case worldresult of 
                Left e -> logPerspectivesError (Custom (show e))
                Right world@(ContextInstance worldId) -> do 
                  puserId <- createResourceIdentifier' (RType $ EnumeratedRoleType DEP.perspectivesUsers) (sysId <> "_KeyHolder")
                  muser <- createAndAddRoleInstance_ (EnumeratedRoleType DEP.perspectivesUsers) worldId
                    (RolSerialization 
                      { id: Just puserId
                      , properties: PropertySerialization (singleton perspectivesUsersPublicKey [publicKey])
                      , binding: Nothing
                      })
                      true
                  case muser of 
                    Nothing -> logPerspectivesError (Custom "Could not create the PerspectivesUsers instance for this installation.")
                    Just puser -> roleIsMe puser world
                  -- Add the new indexed resources to state.
                  void $ lift $ AMA.modify \ps -> ps 
                    { indexedContexts = fromFoldable [Tuple DEP.mySystem (ContextInstance systemId), Tuple DEP.theWorld world]
                    , indexedRoles = insert DEP.sysMe (unsafePartial fromJust mme) ps.indexedRoles
                    }
            Nothing -> logPerspectivesError (Custom "No public key found on setting up!")
        else pure unit


      -- Add the base repository to system:
      void $ createAndAddRoleInstance (EnumeratedRoleType DEP.baseRepository) systemId
        (RolSerialization 
          { id: Nothing 
          , properties: PropertySerialization empty
          , binding: Just "pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains$External"
          })

-- Returns string ending on forward slash (/).
repository :: String -> MonadPerspectivesTransaction String
repository url' = case getFirstMatch (unsafeRegex "^(.*/).+$" noFlags) url' of
  Nothing -> throwError (error ("Cannot get repository from " <> url'))
  Just s -> pure s

-- | Add an InvertedQuery stored in a DomeinFile under invertedQueriesInOtherDomains with the target DomeinFile.
addInvertedQuery :: SeparateInvertedQuery -> State DomeinFileRecord Unit
addInvertedQuery = modifyInvertedQuery true

-- | Remove an InvertedQuery stored in a DomeinFile under invertedQueriesInOtherDomains from the target DomeinFile.
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

-- | The argument may be a versioned or unversioned modelURI, e.g. model://perspectives.domains#System@1.1.0
removeModelFromLocalStore :: Array String ->  RoleInstance -> MonadPerspectivesTransaction Unit
removeModelFromLocalStore versionedModelURIA rid = try
  (case head versionedModelURIA of 
    Just versionedModelURI -> do 
      let unversionedURI = unversionedModelUri versionedModelURI
      let cid = createDefaultIdentifier ((unsafePartial modelUri2ManifestUrl unversionedURI).manifestName <> "_modelRootContext")
      scheduleContextRemoval Nothing (ContextInstance cid)
      scheduleDomeinFileRemoval (DomeinFileId unversionedURI)
    _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#RemoveModelFromLocalStore"

scheduleDomeinFileRemoval :: DomeinFileId -> MonadPerspectivesTransaction Unit
scheduleDomeinFileRemoval id = AMA.modify (over Transaction \t@{modelsToBeRemoved} -> t {modelsToBeRemoved = cons id modelsToBeRemoved})

type Url = String
-- | The RoleInstance is an instance of CouchdbServer$Repositories.
-- | Fails silently if either the url or the name is missing.
-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
-- | Execution of this function requires the user to have a SERVERADMIN account.
createCouchdbDatabase :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
createCouchdbDatabase databaseUrls databaseNames _ = try
  (case head databaseUrls, head databaseNames of
    -- NOTE: misschien moet er een slash tussen
    Just databaseUrl, Just databaseName -> lift $ withDatabase (databaseUrl <> databaseName) (pure <<< const unit)
    _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#CreateCouchdbDatabase"

-- | Create a database with all views that are useful for retrieving role- and context instances
createEntitiesDatabase  :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
createEntitiesDatabase databaseUrls databaseNames _ = try
  (case head databaseUrls, head databaseNames of
    -- NOTE: misschien moet er een slash tussen
    Just databaseUrl, Just databaseName -> lift $ withDatabase (databaseUrl <> databaseName) 
      (\_ -> do
        dbName <- pure (databaseUrl <> databaseName)
        setRoleView dbName
        setRoleFromContextView dbName
        -- OBSOLETE. Remove if testing shows the current definitioin of pendingInvitations works.
        setPendingInvitationView dbName
        setContextView dbName
        setCredentialsView dbName
        setFilledRolesView dbName
        )
    _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#CreateEntitiesDatabase"


-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
-- | Execution of this function requires the user to have a SERVERADMIN account.
deleteCouchdbDatabase :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
deleteCouchdbDatabase databaseUrls databaseNames _ = try
  (case head databaseUrls, head databaseNames of
    -- NOTE: misschien moet er een slash tussen
    Just databaseUrl, Just databaseName -> lift $ deleteDatabase (databaseUrl <> databaseName)
    _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#DeleteCouchdbDatabase"

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
replicateContinuously :: Array Url -> Array Url -> Array DatabaseName -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
replicateContinuously databaseUrls couchdbUrls sources targets _ = try
  (case head databaseUrls, head couchdbUrls, head sources, head targets of
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
    _, _, _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#ReplicateContinuously"

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Repositories.
endReplication :: Array Url -> Array DatabaseName -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
endReplication databaseUrls sources targets _ = try
  (case head databaseUrls, head sources, head targets of
    Just databaseUrl, Just source, Just target -> void $ lift $ CDB.endReplication databaseUrl source target
    _, _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#Couchdb$EndReplication"

deleteDocument :: Array Url -> RoleInstance -> MonadPerspectivesTransaction Unit
deleteDocument url_ _ = try
  (case head url_ of
    Just url -> case splitRepositoryFileUrl url of
      Nothing -> pure unit
      Just {database, document} -> lift $ void $ Persistence.deleteDocument database document Nothing
    _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#Couchdb$DeleteDocument"

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Accounts.
-- | Execution of this function requires the user to have a SERVERADMIN account.
createUser :: Array Url -> Array UserName -> Array Password -> RoleInstance -> MonadPerspectivesTransaction Unit
createUser databaseUrls userNames passwords _ =  try
  (case head databaseUrls, head userNames, head passwords of
    Just databaseurl, Just userName, Just password -> lift $ CDB.createUser databaseurl userName password []
    _, _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#Couchdb$CreateUser"

-- | RoleInstance is an instance of model:CouchdbManagement$CouchdbServer$Accounts.
-- | Execution of this function requires the user to have a SERVERADMIN account.
deleteUser :: Array Url -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
deleteUser databaseUrls userNames _ = try 
  (case head databaseUrls, head userNames of
    Just databaseurl, Just userName -> lift $ void $ CDB.deleteUser databaseurl userName
    _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#Couchdb$DeleteUser"

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
updateSecurityDocument :: String -> (String -> SecurityDocument -> SecurityDocument) -> Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
updateSecurityDocument fname updater databaseUrls databaseNames userNames _ = try 
  (case head databaseUrls, head databaseNames, head userNames of
    Just databaseUrl, Just databaseName, Just userName -> lift $ do
      sdoc <- CDB.ensureSecurityDocument databaseUrl databaseName
      CDB.setSecurityDocument databaseUrl databaseName (updater userName sdoc)
    _, _, _ -> pure unit)
  >>= handleExternalStatementError ("model://perspectives.domains#Couchdb$" <> fname)

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
makeAdminOfDb :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeAdminOfDb = updateSecurityDocument "MakeAdminOfDb" \userName (SecurityDocument r) -> SecurityDocument r {admins = {names: Just $ maybe [userName] (ARR.union [userName]) r.admins.names, roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
removeAsAdminFromDb :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
removeAsAdminFromDb = updateSecurityDocument "RemoveAsAdminFromDb" \userName (SecurityDocument r) -> SecurityDocument r {admins = {names: ARR.delete userName <$> r.admins.names, roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
makeMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeMemberOf = updateSecurityDocument "MakeMemberOf" \userName (SecurityDocument r) -> SecurityDocument r {members = {names: Just (maybe [userName] (ARR.union [userName]) r.members.names), roles: r.members.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
removeAsMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
removeAsMemberOf = updateSecurityDocument "RemoveAsMemberOf"
  \userName (SecurityDocument r) -> 
    SecurityDocument r 
      { members = 
        { names: ARR.delete userName <$> r.members.names
        , roles: r.admins.roles}}

-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a SERVERADMIN account.
-- | Adds the database name as role name to the user document.
makeWritingMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeWritingMemberOf databaseUrls databaseNames userNames _ = try
  (case head databaseUrls, head databaseNames, head userNames of
    Just databaseUrl, Just databaseName, Just userName -> lift $ addRoleToUser databaseUrl userName databaseName
    _, _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#Couchdb$MakeWritingMemberOf"
 
-- | The RoleInstance is an instance of model:CouchdbManagement$Repository$Admin
-- | Execution of this function requires the user to have a SERVERADMIN account.
-- | Removes the database name as role name from the user document.
removeAsWritingMemberOf :: Array Url -> Array DatabaseName -> Array UserName -> RoleInstance -> MonadPerspectivesTransaction Unit
removeAsWritingMemberOf databaseUrls databaseNames userNames _ = try
  (case head databaseUrls, head databaseNames, head userNames of
    Just databaseUrl, Just databaseName, Just userName -> lift $ removeRoleFromUser databaseUrl userName databaseName
    _, _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#Couchdb$RemoveAsWritingMemberOf"

-- | Any user can read the documents (and write them too, though we can restrict this by applying makeDatabaseWriteProtected).
-- | This involves removing both the `names` and the `roles` field from the members section (the admin section need not be changed).
-- | First execution of this function requires the user to have SERVERADMIN account.
-- | After that, a DATABASEADMIN may change it.
makeDatabasePublic :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeDatabasePublic databaseUrls databaseNames roleId = try 
  (lift $
    case head databaseUrls, head databaseNames of
      Just databaseUrl, Just databaseName -> do 
        SecurityDocument sdoc <- CDB.ensureSecurityDocument databaseUrl databaseName
        CDB.setSecurityDocument 
          databaseUrl 
          databaseName 
          (SecurityDocument $ sdoc { members = 
            { names: Just []
            , roles: []}})
      _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#MakeDatabasePublic"

-- | Only users who have a role that equals the name of the database can create, update or delete documents in it.
-- | This involves adding a validate_doc_update function.
-- | Execution of this function requires the user to have a DATABASEADMIN or SERVERADMIN account.
makeDatabaseWriteProtected :: Array Url -> Array DatabaseName -> RoleInstance -> MonadPerspectivesTransaction Unit
makeDatabaseWriteProtected databaseUrls databaseNames roleId = try 
  (lift $
    case head databaseUrls, head databaseNames of
      Just databaseUrl, Just databaseName -> void $ ensureDesignDoc (databaseUrl `concatenatePathSegments` databaseName) "writeprotection" (replace (Pattern "$$DATABASENAME$$") (Replacement databaseName) validate_doc_update)
      _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#MakeDatabaseWriteProtected"

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
resetPassword databaseUrls userNames passwords _ = try
  (case head databaseUrls, head userNames, head passwords of
    Just databaseUrl, Just userName, Just password -> lift $ CDB.setPassword databaseUrl userName password
    _, _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#ResetPassword"
  
-- | Add credentials to the current session. Once persisted in the User's local storage, they will be retrieved on each session.
-- | Notice that this function causes a change in PerspectivesState but not in the Perspectives Universe.
addCredentials :: Array Url -> Array UserName -> Array Password -> RoleInstance -> MonadPerspectivesTransaction Unit
addCredentials urls usernames passwords _ = try 
  (case head urls, head usernames, head passwords of 
    Just url, Just username, Just password -> lift $ Authentication.addCredentials url username password
    _, _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#AddCredentials"

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ 
  -- SERVERADMIN
    Tuple "model://perspectives.domains#Couchdb$CreateCouchdbDatabase" {func: unsafeCoerce createCouchdbDatabase, nArgs: 2, isFunctional: True}    
  , Tuple "model://perspectives.domains#Couchdb$CreateEntitiesDatabase" {func: unsafeCoerce createEntitiesDatabase, nArgs: 2, isFunctional: True}
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
