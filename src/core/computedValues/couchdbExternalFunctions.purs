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

module Perspectives.Extern.Couchdb
  where

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.AvarMonadAsk (modify, gets) as AMA
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (execState, execStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Array (catMaybes, cons, filter, head, union)
import Data.Array (union, delete) as ARR
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (over, unwrap)
import Data.Nullable (toMaybe)
import Data.String (Replacement(..), replace, Pattern(..))
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Decacheable (decache)
import Effect.Aff.AVar (tryRead)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (empty, fromFoldable, singleton)
import JS.Iterable (toArray)
import LRUCache (rvalues)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.StateCache (clearModelStates)
import Perspectives.Assignment.Update (withAuthoringRole)
import Perspectives.Authenticate (getMyPublicKey)
import Perspectives.ContextAndRole (changeRol_isMe, context_id, rol_id)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), InformedAssumption(..), MonadPerspectives, MonadPerspectivesTransaction, (##=))
import Perspectives.Couchdb (DatabaseName, SecurityDocument(..))
import Perspectives.Couchdb.Revision (Revision_)
import Perspectives.Deltas (addCreatedContextToTransaction)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (addAttachments, fetchTranslations, getPatchAndBuild, getVersionToInstall, saveCachedDomeinFile, storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), addDownStreamAutomaticEffect, addDownStreamNotification, removeDownStreamAutomaticEffect, removeDownStreamNotification)
import Perspectives.Error.Boundaries (handleDomeinFileError, handleExternalFunctionError, handleExternalStatementError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (Namespace, getFirstMatch, isModelUri, modelUri2ManifestUrl, modelUri2ModelUrl, modelUriVersion, newModelRegex, typeUri2LocalName_, unversionedModelUri)
import Perspectives.InstanceRepresentation (PerspectContext)
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance, createAndAddRoleInstance_)
import Perspectives.Instances.CreateContext (constructEmptyContext)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.InvertedQuery.Storable (getInvertedQueriesOfModel, removeInvertedQueriesContributedByModel, saveInvertedQueries, clearInvertedQueriesDatabase)
import Perspectives.ModelDependencies (identifiableLastName, perspectivesUsersPublicKey, theWorldInitializer)
import Perspectives.ModelDependencies as DEP
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (DesignDocument(..), MonadPouchdb, addDocument_, deleteDatabase, getAttachment, getDocument, getViewOnDatabase, splitRepositoryFileUrl, tryGetDocument_, withDatabase, Keys(..))
import Perspectives.Persistence.API (deleteDocument, documentsInDatabase, excludeDocs) as Persistence
import Perspectives.Persistence.Authentication (addCredentials) as Authentication
import Perspectives.Persistence.CouchdbFunctions (addRoleToUser, concatenatePathSegments, removeRoleFromUser)
import Perspectives.Persistence.CouchdbFunctions as CDB
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (UserName, Password)
import Perspectives.Persistent (entitiesDatabaseName, forceSaveDomeinFile, getDomeinFile, getPerspectRol, saveEntiteit, saveEntiteit_, saveMarkedResources, tryGetPerspectContext, tryGetPerspectEntiteit)
import Perspectives.PerspectivesState (clearQueryCache, contextCache, getPerspectivesUser, modelsDatabaseName, removeTranslationTable, roleCache)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance, perspectivesUser2RoleInstance)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), RoleType(..))
import Perspectives.ResourceIdentifiers (createDefaultIdentifier, resourceIdentifier2WriteDocLocator, takeGuid)
import Perspectives.RoleAssignment (roleIsMe)
import Perspectives.SaveUserData (scheduleContextRemoval)
import Perspectives.SetupCouchdb (contextViewFilter, roleViewFilter, setContext2RoleView, setContextView, setCredentialsView, setFiller2FilledView, setFilled2FillerView, setPendingInvitationView, setRoleFromContextView, setRoleView, setRole2ContextView)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.Transaction (Transaction(..), UninterpretedTransactionForPeer(..))
import Prelude (Unit, bind, const, discard, eq, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=))
import Simple.JSON (read_)
import Unsafe.Coerce (unsafeCoerce)

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
        instancesInCouchdb <- (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/roleView" (maybe NoKey Key (head roleTypes))
        instancesInCache <- lift (do
          cache <- roleCache
          cachedRoleAvars <- liftAff $ liftEffect (rvalues cache >>= pure <<< toArray)
          cachedRoles <- catMaybes <$> (lift $ traverse tryRead cachedRoleAvars)
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
        instancesInCouchdb <- (lift entitiesDatabaseName) >>= \db -> lift $ getViewOnDatabase db "defaultViews/contextView" (maybe NoKey Key (head contextTypeArr))
        instancesInCache <- lift (do
          cache <- contextCache
          cachedContextAvars <- liftAff $ liftEffect (rvalues cache >>= pure <<< toArray)
          cachedContexts <- catMaybes <$> (lift $ traverse tryRead cachedContextAvars)
          pure $ context_id <$> filter (contextViewFilter $ ContextType ct) cachedContexts
          )
        pure $ instancesInCouchdb `union` instancesInCache)
  >>= handleExternalFunctionError "model://perspectives.domains#Couchdb$ContextInstances"

pendingInvitations :: ContextInstance ~~> RoleInstance
pendingInvitations _ = try 
  (ArrayT $ lift do
    db <- entitiesDatabaseName
    filledRolesInDatabase :: Array RoleInstance <- getViewOnDatabase db "defaultViews/roleView" (Key DEP.invitation)
    filledRolesInCache :: Array RoleInstance <- (do 
      cache <- roleCache
      cachedRoleAvars <- liftAff $ liftEffect $ (rvalues cache >>= pure <<< toArray)
      cachedRoles <- catMaybes <$> (lift $ traverse tryRead cachedRoleAvars)
      pure $ rol_id <$> filter (roleViewFilter $ EnumeratedRoleType DEP.invitation) cachedRoles
      )
    pure $ filledRolesInDatabase `union` filledRolesInCache)
  >>= handleExternalFunctionError "model://perspectives.domains#Couchdb$PendingInvitations"
  
-- | Overwrites the model currently residing in the local models database.
-- | Takes care of inverted queries in Couchdb.
-- | Clears the query cache in PerspectivesState.
-- | Clears compiled states from cache.
-- | The first argument should contain the model name ("model://some.domain#Something@<SemVer>"), 
-- | The second argument should contain the string representation of a boolean value.
-- | The third argument is an array with an instance of the role ModelsInuse.
-- | If no SemVer is given, will try to load the unversioned model (if any).
updateModel :: Array String -> Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
updateModel arrWithModelName arrWithDependencies _ = try 
  (case head arrWithModelName of
    -- fail silently
    Nothing -> pure unit
    -- TODO: add a check on the form of the modelName.
    Just modelName -> if isModelUri modelName 
      then updateModel' (maybe false (eq "true") (head arrWithDependencies)) (DomeinFileId modelName)
      else throwError (error $ "This is not a well-formed domain name: " <> modelName))
  >>= handleExternalStatementError "model://perspectives.domains#UpdateModel"

  where
    updateModel' :: Boolean -> DomeinFileId -> MonadPerspectivesTransaction Unit
    updateModel' withDependencies dfid@(DomeinFileId modelName) = do
      {repositoryUrl, documentName} <- pure $ unsafePartial modelUri2ModelUrl modelName
      unversionedModelname <- pure $ unversionedModelUri modelName
      (lift $ tryGetPerspectEntiteit (DomeinFileId unversionedModelname)) >>= case _ of 
        -- If we have not installed this model, do nothing.
        Nothing -> pure unit
        Just (DomeinFile{upstreamStateNotifications, upstreamAutomaticEffects, referredModels}) -> do
          -- Remove the inverted queries, upstream notifications and automatic effects of the OLD version of the model!
          if withDependencies
            then for_ referredModels (updateModel' withDependencies)
            else pure unit
          -- Remove the inverted queries contributed by this model.
          lift $ removeInvertedQueriesContributedByModel dfid
          -- Get the inverted queries from the repository,
          -- and add the inverted queries to the local database.
          lift (getInvertedQueriesOfModel repositoryUrl documentName >>= saveInvertedQueries)

          forWithIndex_ upstreamStateNotifications
            \domainName notifications -> do
              (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
                handleDomeinFileError "updateModel'"
                \(DomeinFile dfr) -> do
                  lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ notifications removeDownStreamNotification) dfr))
          forWithIndex_ upstreamAutomaticEffects
            \domainName automaticEffects -> do
              (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
                handleDomeinFileError "updateModel'"
                \(DomeinFile dfr) -> do
                  lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ automaticEffects removeDownStreamAutomaticEffect) dfr))
          -- Clear the caches of compiled states.
          void $ pure $ clearModelStates (DomeinFileId unversionedModelname)
          -- Install the new model, taking care of outgoing InvertedQueries.
          addModelToLocalStore (DomeinFileId modelName) isUpdate
          -- The model is now decached, but the translations table is still in cache.
          -- It will be loaded when a new type lookup is performed.
          lift $ removeTranslationTable modelName
          lift $ fetchTranslations dfid
    
    allModelsInUse :: MonadPerspectives (Array DomeinFileId)
    allModelsInUse = do
      system <- getMySystem
      propGetter <- getDynamicPropertyGetter DEP.modelURI (UET (EnumeratedRoleType DEP.modelsInUse))
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
-- | Attachments are fetched from the repository and stored locally.
-- | Invariant: the model is not in cache when this function returns.
addModelToLocalStore :: DomeinFileId -> Boolean -> MonadPerspectivesTransaction Unit
addModelToLocalStore dfid@(DomeinFileId modelname) isInitialLoad' = do
  unversionedModelname <- pure $ unversionedModelUri modelname
  x :: (Maybe {semver :: String, versionedModelManifest :: RoleInstance}) <- lift $ getVersionToInstall (DomeinFileId unversionedModelname)
  {patch, build} <- case x of 
    Nothing -> pure {patch: "0", build: "0"}
    Just {versionedModelManifest} -> lift $ getPatchAndBuild versionedModelManifest
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
    , upstreamAutomaticEffects
    , _attachments} <- lift $ getDocument repositoryUrl documentName

  attachments <- case _attachments of
    Nothing -> pure empty
    Just atts ->  lift $ traverseWithIndex
      (\attName {content_type} -> Tuple (MediaType content_type) <$> getAttachment repositoryUrl documentName attName)
      atts

  -- Store the model in Couchdb, that is: in the local store of models.
  -- Save it with the revision of the local version that we have, if any (do not use the repository version).
  {documentName:unversionedDocumentName} <- lift $ resourceIdentifier2WriteDocLocator unversionedModelname
  lift $ void $ cacheEntity id (DomeinFile dfrecord { _rev = Nothing, _id = unversionedDocumentName, _attachments = Nothing})
  -- saveCachedDomeinFile takes care of revisions.
  void $ lift $ saveCachedDomeinFile id

  if isInitialLoad'
    then do 
      createInitialInstances unversionedModelname versionedModelName patch build (_.versionedModelManifest <$> x)
            -- Add new dependencies.
      for_ referredModels \dfid' -> do
        mmodel <- lift $ tryGetPerspectEntiteit dfid'
        case mmodel of
          Nothing -> addModelToLocalStore' dfid'
          Just _ -> pure unit
    else pure unit

  lift (getInvertedQueriesOfModel repositoryUrl documentName >>= saveInvertedQueries)
  -- As we have the new definitions of invertedQueries in place in the database, we should now clear the cache in PerspectivesState
  -- to prevent old versions of being used.
  lift clearQueryCache

  -- Distribute upstream state notifications over the other domains.
  forWithIndex_ upstreamStateNotifications
    \domainName notifications -> do
      (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
        handleDomeinFileError "addModelToLocalStore'"
        \(DomeinFile dfr) -> do
          lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ notifications addDownStreamNotification) dfr))

  -- Distribute upstream automatic effects over the other domains.
  forWithIndex_ upstreamAutomaticEffects
    \domainName automaticEffects -> do
      (lift $ try $ getDomeinFile (DomeinFileId domainName)) >>=
        handleDomeinFileError "addModelToLocalStore'"
        \(DomeinFile dfr) -> do
          lift (storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ automaticEffects addDownStreamAutomaticEffect) dfr))

  -- Add all attachments.
  dbName <- lift modelsDatabaseName
  -- First make sure the DomeinFile has been saved:
  lift $ forceSaveDomeinFile id
  (DomeinFile {_rev}) <- lift $ getDomeinFile id
  void $ lift $ execStateT (addAttachments dbName unversionedDocumentName attachments) _rev
  -- Now uncache the DomeinFile, as it no longer holds the right revision, neither has the attachments.
  lift $ decache id

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

  -- Now create the Installer user role IN THE MODEL INSTANCE.
  me <- lift $ getPerspectivesUser
  minstallerId <- createAndAddRoleInstance (EnumeratedRoleType DEP.installer) cid
    (RolSerialization 
      { id: Nothing
      , properties: PropertySerialization empty
      , binding: Just $ unwrap me})
  -- Make the PDR recognize this role as one played by the user.
  case minstallerId  of 
    Just installerId ->  do 
      installer <- lift $ getPerspectRol installerId
      void $ lift $ cacheEntity installerId (changeRol_isMe installer true)
    _ -> pure unit


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

-- | Creates instances in a transaction where the authoring role is PerspectivesSystem$User (the 'subject' of the delta: the role that must have the right perspective), of:
-- |    * PerspectivesSystem
-- |    * PerspectivesSystem$User
-- |    * TheWorld
-- |    * PerspectivesUsers (with author TheWorld$Initializer) for the new user and for authoring role "serializationuser", where
-- |      - the new user gets a PublicKey
-- |      - the serializationuser gets a LastName ("serializationuser").
-- |    * BaseRepository, filled with the Perspectives Repository.
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
      userId <- pure (systemId <> "$" <> (typeUri2LocalName_ DEP.sysUser))
      me <- createAndAddRoleInstance_ (EnumeratedRoleType DEP.sysUser) systemId
        (RolSerialization 
          { id: Just userId
          , properties: PropertySerialization empty
          , binding: Nothing
          })
        true
      roleIsMe me system
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
                  -- Is with a storage scheme right from the start.
                  PerspectivesUser perspectivesUser <- lift getPerspectivesUser
                  withAuthoringRole (CR $ CalculatedRoleType theWorldInitializer) do
                    puser <- createAndAddRoleInstance_ (EnumeratedRoleType DEP.perspectivesUsers) worldId
                      (RolSerialization 
                        { id: Just perspectivesUser
                        , properties: PropertySerialization (singleton perspectivesUsersPublicKey [publicKey])
                        , binding: Nothing
                        })
                        true
                    roleIsMe puser world
                    void $ createAndAddRoleInstance_ (EnumeratedRoleType DEP.perspectivesUsers) worldId
                      (RolSerialization 
                        { id: Just "def:#serializationuser"
                        , properties: PropertySerialization (singleton identifiableLastName ["Serialisation persona"])
                        , binding: Nothing
                        })
                        false
            Nothing -> logPerspectivesError (Custom "No public key found on setting up!")
          -- Instead, read the Identity document.
          else do 
            mIdoc <- gets \(Transaction{identityDocument}) -> identityDocument
            case mIdoc of 
              Just (UninterpretedTransactionForPeer i) -> 
                case read_ i of 
                  Just identityDocument -> do 
                    executeTransaction identityDocument
                    -- now set isMe of the PerspectivesUser that fills sys:SocialMe.
                    pUser <- perspectivesUser2RoleInstance <$> lift getPerspectivesUser
                    pUserRole <- lift (getPerspectRol pUser)
                    lift $ void $ cacheEntity pUser $ changeRol_isMe pUserRole true
                    lift $ void $ saveEntiteit pUser 
                  Nothing -> pure unit
              Nothing -> pure unit

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

-- | The argument may be a versioned or unversioned modelURI, e.g. model://perspectives.domains#System@1.1.0
removeModelFromLocalStore :: Array String ->  RoleInstance -> MonadPerspectivesTransaction Unit
removeModelFromLocalStore versionedModelURIA rid = try
  (case head versionedModelURIA of 
    Just versionedModelURI -> do 
      let unversionedURI = unversionedModelUri versionedModelURI
      let cid = createDefaultIdentifier ((unsafePartial modelUri2ManifestUrl unversionedURI).manifestName <> "_modelRootContext")
      scheduleContextRemoval Nothing [] (ContextInstance cid)
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
createEntitiesDatabase  :: Array Url -> Array DatabaseName -> Array Namespace -> RoleInstance -> MonadPerspectivesTransaction Unit
createEntitiesDatabase databaseUrls databaseNames namespaces _ = try
  (case head databaseUrls, head databaseNames, head namespaces of
    Just databaseUrl, Just databaseName, Just namespace -> do 
      dbName <- pure (databaseUrl <> databaseName)
      lift $ withDatabase (databaseUrl <> databaseName) 
        (\_ -> do
          setRoleView dbName
          setRoleFromContextView dbName
          -- OBSOLETE. Remove if testing shows the current definitioin of pendingInvitations works.
          setPendingInvitationView dbName
          setContextView dbName
          setCredentialsView dbName
          setFiller2FilledView dbName
          setFilled2FillerView dbName
          setContext2RoleView dbName
          setRole2ContextView dbName
          )
      -- dbName is also the url that is provided in a "public Visitor at <location>" declaration.
      -- Hence we can use it to construct an instance of TheWorld in that database.
      theworldid <- pure (ContextInstance $ "pub:https://" <> namespace <> "/#TheWorld")
      mtheWorld <- lift $ tryGetPerspectContext theworldid
      case mtheWorld of 
        Nothing -> do 
          void $ runExceptT $ constructEmptyContext theworldid DEP.theWorld "TheWorld" (PropertySerialization empty) Nothing
          lift $ void $ saveEntiteit theworldid
        _ -> pure unit
    _, _, _ -> pure unit)
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
      usr <- lift $ getPerspectivesUser
      lift $ CDB.replicateContinuously
        usr
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
-- | The username MAY be with a storage scheme; we discard it.
addCredentials :: Array Url -> Array UserName -> Array Password -> RoleInstance -> MonadPerspectivesTransaction Unit
addCredentials urls usernames passwords _ = try 
  (case head urls, head usernames, head passwords of 
    Just url, Just username, Just password -> lift $ Authentication.addCredentials url (takeGuid username) password
    _, _, _ -> pure unit)
  >>= handleExternalStatementError "model://perspectives.domains#AddCredentials"

clearAndFillInvertedQueriesDatabase :: RoleInstance -> MonadPerspectivesTransaction Unit
clearAndFillInvertedQueriesDatabase _ = do
  modelsDb <- lift $ modelsDatabaseName
  lift $ clearInvertedQueriesDatabase
  {rows:allModels} <- lift $ Persistence.documentsInDatabase modelsDb Persistence.excludeDocs
  for_ allModels \{id} -> reloadQueries modelsDb id
  where
  reloadQueries :: String ->  String -> MonadPerspectivesTransaction Unit
  reloadQueries db modelFileName = do
    lift (getInvertedQueriesOfModel db modelFileName >>= saveInvertedQueries)

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ 
  -- SERVERADMIN
    Tuple "model://perspectives.domains#Couchdb$CreateCouchdbDatabase" {func: unsafeCoerce createCouchdbDatabase, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$CreateEntitiesDatabase" {func: unsafeCoerce createEntitiesDatabase, nArgs: 3, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$DeleteCouchdbDatabase" {func: unsafeCoerce deleteCouchdbDatabase, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$CreateUser" {func: unsafeCoerce createUser, nArgs: 3, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$DeleteUser" {func: unsafeCoerce deleteUser, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$ResetPassword" {func: unsafeCoerce resetPassword, nArgs: 3, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$MakeWritingMemberOf" {func: unsafeCoerce makeWritingMemberOf, nArgs: 3, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$RemoveAsWritingMemberOf" {func: unsafeCoerce removeAsWritingMemberOf, nArgs: 3, isFunctional: True, isEffect: true}
  -- DATABASEADMIN
  , Tuple "model://perspectives.domains#Couchdb$MakeDatabasePublic" {func: unsafeCoerce makeDatabasePublic, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$MakeDatabaseWriteProtected" {func: unsafeCoerce makeDatabaseWriteProtected, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$MakeAdminOfDb" {func: unsafeCoerce makeAdminOfDb, nArgs: 3, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$RemoveAsAdminFromDb" {func: unsafeCoerce removeAsAdminFromDb, nArgs: 3, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$MakeMemberOf" {func: unsafeCoerce makeMemberOf, nArgs: 3, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$RemoveAsMemberOf" {func: unsafeCoerce removeAsMemberOf, nArgs: 3, isFunctional: True, isEffect: true}
  -- WRITINGMEMBER
  , Tuple "model://perspectives.domains#Couchdb$DeleteDocument" {func: unsafeCoerce deleteDocument, nArgs: 1, isFunctional: True, isEffect: true}
  -- Requires writing to the _replicator database
  , Tuple "model://perspectives.domains#Couchdb$ReplicateContinuously" {func: unsafeCoerce replicateContinuously, nArgs: 4, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$EndReplication" {func: unsafeCoerce endReplication, nArgs: 3, isFunctional: True, isEffect: true}
  -- Requires writing to the users model database.
  , Tuple "model://perspectives.domains#Couchdb$AddModelToLocalStore" {func: unsafeCoerce addModelToLocalStore_, nArgs: 1, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$RemoveModelFromLocalStore" {func: unsafeCoerce removeModelFromLocalStore, nArgs: 1, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$UpdateModel" {func: unsafeCoerce updateModel, nArgs: 2, isFunctional: True, isEffect: true}

  -- These functions require read access to the users instances databases.
  , Tuple "model://perspectives.domains#Couchdb$RoleInstances" {func: unsafeCoerce roleInstancesFromCouchdb, nArgs: 1, isFunctional: Unknown, isEffect: false}
  , Tuple "model://perspectives.domains#Couchdb$ContextInstances" {func: unsafeCoerce contextInstancesFromCouchdb, nArgs: 1, isFunctional: Unknown, isEffect: false}
  , Tuple "model://perspectives.domains#Couchdb$PendingInvitations" {func: unsafeCoerce pendingInvitations, nArgs: 0, isFunctional: Unknown, isEffect: false}
  -- This requires no access to Couchdb.
  , Tuple "model://perspectives.domains#Couchdb$AddCredentials" {func: unsafeCoerce addCredentials, nArgs: 3, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Couchdb$ClearAndFillInvertedQueriesDatabase" {func: unsafeCoerce clearAndFillInvertedQueriesDatabase, nArgs: 0, isFunctional: True, isEffect: true}

  ]
