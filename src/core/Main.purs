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

module Main

where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Writer (runWriterT)
import Data.Array (cons, foldM)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (insert)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), catchError, delay, error, forkAff, joinFiber, runAff, throwError, try)
import Effect.Aff.AVar (AVar, empty, new, put, take, read)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Foreign (Foreign)
import Foreign.Object (fromFoldable, singleton)
import IDBKeyVal (clear, idbSet)
import Perspectives.AMQP.IncomingPost (retrieveBrokerService, incomingPost)
import Perspectives.Api (resumeApi, setupApi) as API
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Authenticate (getPrivateKey)
import Perspectives.CoreTypes (IndexedResource(..), IntegrityFix(..), JustInTimeModelLoad(..), MonadPerspectives, MonadPerspectivesTransaction, PerspectivesState, RepeatingTransaction(..), RuntimeOptions, (##=), (##>>))
import Perspectives.Couchdb (SecurityDocument(..))
import Perspectives.DataUpgrade (runDataUpgrades)
import Perspectives.DataUpgrade.RecompileLocalModels as RECOMPILE
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Error.Boundaries (handlePerspectRolError')
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (addModelToLocalStore, isInitialLoad, roleInstancesFromCouchdb)
import Perspectives.Extern.Utilities (pdrVersion)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.Indexed (indexedContexts_, indexedRoles_)
import Perspectives.Instances.ObjectGetters (context, externalRole)
import Perspectives.ModelDependencies (indexedContext, indexedContextName, indexedRole, indexedRoleName, sysUser, userWithCredentialsAuthorizedDomain, userWithCredentialsPassword, userWithCredentialsUsername)
import Perspectives.ModelTranslation (getCurrentLanguageFromIDB)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (DatabaseName, Keys(..), PouchdbUser, UserName, createDatabase, databaseInfo, decodePouchdbUser', deleteDatabase, getViewOnDatabase)
import Perspectives.Persistence.CouchdbFunctions (setSecurityDocument)
import Perspectives.Persistence.State (getSystemIdentifier, withCouchdbUrl)
import Perspectives.Persistence.Types (Credential(..))
import Perspectives.Persistent (entitiesDatabaseName, invertedQueryDatabaseName, postDatabaseName, saveMarkedResources)
import Perspectives.PerspectivesState (defaultRuntimeOptions, modelsDatabaseName, newPerspectivesState, resetCaches)
import Perspectives.Proxy (handleClientRequest) as Proxy
import Perspectives.Query.UnsafeCompiler (getPropertyFromTelescope, getPropertyFunction, getRoleFunction, getterFromPropertyType)
import Perspectives.ReferentialIntegrity (fixReferences)
import Perspectives.Repetition (Duration, fromDuration)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier)
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runEmbeddedIfNecessary, runEmbeddedTransaction, runMonadPerspectivesTransaction)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupCouchdb (createUserDatabases)
import Perspectives.SetupUser (reSetupUser, setupUser)
import Perspectives.Sync.Channel (endChannelReplication)
import Perspectives.Sync.Transaction (UninterpretedTransactionForPeer(..))
import Perspectives.SystemClocks (forkedSystemClocks)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (+), (-), (<), (<$>), (<<<), (<>), (>), (>=>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-- | Don't do anything. runPDR will actually start the core.
main :: Effect Unit
main = pure unit

-- | For testing, uncomment this version of main.
-- | Runs the PDR with default credentials.
-- main :: Effect Unit
-- main = runPDR
--   "cor"
--   "geheim"
--   (encodePouchdbUser'
--     { _rev: Nothing
--       , systemIdentifier: "cor"
--       , password: "geheim"
--       , couchdbUrl: Just "http://127.0.0.1:6984/"
--       -- ,userName: CDBstate.UserName "cor"
--       })
  -- "http://joopringelberg.nl/cbd/repository"
-- main = parseFromCommandLine

-- NOTE: For release v0.1.0, I've commented out the original main function because it requires the perspectivesproxy
-- While this is available, it is webpacked for usage in the browser and will not run from the command line
-- (The PDR is not meant for command line usage).

-- | To be called from the client.
-- | Execute the Perspectives Distributed Runtime by creating a listener to the internal channel.
-- | Implementation note: the PouchdbUser should have a couchdbUrl that terminates on a forward slash.
runPDR :: UserName -> Foreign -> RuntimeOptions -> (Boolean -> Effect Unit) -> Effect Unit
runPDR usr rawPouchdbUser options callback = void $ runAff handler do
  case decodePouchdbUser' rawPouchdbUser of
    Left _ -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in runPDR")
    Right (pouchdbUser :: PouchdbUser) -> do
      transactionFlag <- new true
      brokerService <- empty
      transactionWithTiming <- empty
      modelToLoad <- empty
      indexedResourceToCreate <- empty
      missingResource <- empty
      state <- getCurrentLanguageFromIDB >>= new <<< newPerspectivesState
        pouchdbUser 
        transactionFlag 
        transactionWithTiming 
        modelToLoad 
        options 
        brokerService 
        indexedResourceToCreate
        missingResource

      -- Start by running data upgrades.
      runPerspectivesWithState runDataUpgrades state
      
      -- Fork aff to capture transactions to run.
      void $ forkAff $ forkTimedTransactions transactionWithTiming state

      -- Fork aff to load models just in time.
      void $ forkAff $ forkJustInTimeModelLoader modelToLoad state

      -- Fork aff to restore referential integrity
      void $ forkAff $ forkReferentialIntegrityFixer missingResource state

      -- Fork aff to save to the database
      void $ forkAff $ forkDatabasePersistence state

      void $ forkAff $ forkCreateIndexedResources indexedResourceToCreate state

      runPerspectivesWithState (do
        addAllExternalFunctions
        addIndexedNames
        retrieveAllCredentials
        retrieveBrokerService
        key <- getPrivateKey
        modify \(s@{runtimeOptions}) -> s {runtimeOptions = runtimeOptions {privateKey = unsafeCoerce key}}
        )
        state
      void $ forkAff $ run state
      -- void $ forkAff $ forever do
      --   apiFiber <- forkAff $ do
      --     log "(Re)starting the API listener."
      --     runPerspectivesWithState setupApi state
      --   catchError (joinFiber apiFiber)
      --     \e -> do
      --       logPerspectivesError $ Custom $ "API stopped and restarted because of: " <> show e
      -- Trial and error shows us that if we apply the pattern used for setupApi to incomingPost,
      -- The application will not run well.
      void $ forkAff $ do
        postFiber <- forkAff $ runPerspectivesWithState incomingPost state
        catchError (joinFiber postFiber)
          \e -> do
            logPerspectivesError $ Custom $ "Stopped handling incoming post because of: " <> show e

      -- Fork aff to run the system clocks. Should not be started before we have added the private key to state!
      void $ forkAff $ forkedSystemClocks state

  where
    run :: AVar PerspectivesState -> Aff Unit
    run state = catchError 
      (do 
        log "Starting the Perspectives API."
        -- The very first request will invoke detectPublicStateChanges.
        runPerspectivesWithState API.setupApi state)
      \e -> do
        logPerspectivesError $ Custom $ "API stopped because: " <> show e
        resumeRun state

    resumeRun :: AVar PerspectivesState -> Aff Unit
    resumeRun state = catchError 
      (do 
        log "Resuming the Perspectives API."
        runPerspectivesWithState API.resumeApi state)
      \e -> do
        logPerspectivesError $ Custom $ "API stopped because: " <> show e
        resumeRun state

    forkTimedTransactions :: AVar RepeatingTransaction -> AVar PerspectivesState -> Aff Unit
    forkTimedTransactions repeatingTransactionAVar state = do
      repeatingTransaction <- take repeatingTransactionAVar
      case repeatingTransaction of
        (TransactionWithTiming t@{instanceId, stateId, startMoment, endMoment}) -> do
          f <- forkAff (do 
            case startMoment of
              Nothing -> pure unit
              Just d -> delay (fromDuration d)
            -- Calculate the end moment on the clock and pass on to repeatUnlimited
            mendMoment <- computeEndMoment endMoment
            repeatUnlimited t mendMoment)
          registerTransactionFiber f instanceId stateId state
          forkTimedTransactions repeatingTransactionAVar state
        RepeatNtimes t@{instanceId, stateId, nrOfTimes, startMoment, endMoment} -> do
          f <- forkAff (do 
            case startMoment of
              Nothing -> pure unit
              Just d -> delay (fromDuration d)
            -- Calculate the end moment on the clock and pass on to repeatN
            mendMoment <- computeEndMoment endMoment
            repeatN t nrOfTimes mendMoment)
          registerTransactionFiber f instanceId stateId state
          forkTimedTransactions repeatingTransactionAVar state
      where
        repeatUnlimited :: forall f. 
          {transaction :: MonadPerspectivesTransaction Unit, authoringRole :: RoleType, interval :: Duration | f} 
          -> Maybe Instant
          -> Aff Unit
        repeatUnlimited t@{transaction, authoringRole, interval} mendMoment = do
          delay (fromDuration interval)
          case mendMoment of 
            Nothing -> do 
              _ <- runPerspectivesWithState (runMonadPerspectivesTransaction authoringRole transaction) state
              repeatUnlimited t mendMoment
            Just endMoment -> do 
              n <- liftEffect $ now
              -- If not past the endMoment:
              if n < endMoment
                then do
                  _ <- runPerspectivesWithState (runMonadPerspectivesTransaction authoringRole transaction) state
                  repeatUnlimited t mendMoment
                else pure unit

        repeatN :: forall f. 
          {transaction :: MonadPerspectivesTransaction Unit, authoringRole :: RoleType, interval :: Duration | f} 
          -> Int 
          -> Maybe Instant
          -> Aff Unit
        repeatN t@{transaction, authoringRole, interval} counter mendMoment = do
          delay (fromDuration interval)
          case mendMoment of 
            Nothing -> do 
              if counter > 0
                then do 
                  -- If not past the endMoment:
                  _ <- runPerspectivesWithState (runMonadPerspectivesTransaction authoringRole transaction) state
                  repeatN t (counter - 1) mendMoment
                else pure unit
            Just endMoment -> do
              n <- liftEffect $ now
              -- If not past the endMoment:
              if n < endMoment
                then do
                  if counter > 0
                    then do 
                      -- If not past the endMoment:
                      _ <- runPerspectivesWithState (runMonadPerspectivesTransaction authoringRole transaction) state
                      repeatN t (counter - 1) mendMoment
                    else pure unit
                else pure unit

        computeEndMoment :: Maybe Duration -> Aff (Maybe Instant)
        computeEndMoment Nothing = pure Nothing
        computeEndMoment (Just d) = do 
          (Milliseconds start) <- liftEffect $ unInstant <$> now
          (Milliseconds interval) <- pure $ fromDuration d
          pure $ instant $ Milliseconds (start + interval)

    -- Register the fiber so it can be stopped if the instance exits the state.
    registerTransactionFiber :: Fiber Unit -> String -> StateIdentifier -> AVar PerspectivesState -> Aff Unit
    registerTransactionFiber f instanceId stateId stateAVar = do
      s@{transactionFibers} <- take stateAVar
      put s {transactionFibers = insert (Tuple instanceId stateId) f transactionFibers} stateAVar

    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in runPDR: " <> (show e)
      callback false
    handler (Right _) = do
      logPerspectivesError $ Custom $ "Started the PDR for: " <> usr
      callback true

forkDatabasePersistence :: AVar PerspectivesState -> Aff Unit 
forkDatabasePersistence state = do
  -- TODO. Read this from state.
  delay (Milliseconds 10000.0)
  try (runPerspectivesWithState saveMarkedResources state) >>= case _ of 
    Left e -> logPerspectivesError (Custom $ "Could not complete saving resources without an error: " <> show e)
    Right _ -> pure unit
  forkDatabasePersistence state

forkCreateIndexedResources :: AVar IndexedResource -> AVar PerspectivesState -> Aff Unit
forkCreateIndexedResources av state = do
  -- take the contents of the AVar that holds an IndexedResource
  ir <- read av
  (try $ runPerspectivesWithState (create ir) state) >>= 
    case _ of 
      Left e -> do 
        logPerspectivesError (Custom $ "Error on creating indexedResource : " <> show e)
        void $ take av
      Right _ -> void $ take av
  forkCreateIndexedResources av state
  where 
    create :: IndexedResource -> MonadPerspectives Unit
    create ir = case ir of 
      IndexedContext (ContextInstance cid) iName -> do
        mysystem <- getMySystem
        void $ runEmbeddedTransaction
          false -- We do not share IndexedRole or IndexedContext.
          (ENR $ EnumeratedRoleType sysUser)
          (createAndAddRoleInstance (EnumeratedRoleType indexedContext) mysystem
            (RolSerialization
              { id: Nothing
              , binding: Just $ buitenRol cid
              , properties: PropertySerialization (singleton indexedContextName [iName])
              }))
      IndexedRole (RoleInstance rid) iName -> do
        mysystem <- getMySystem
        void $ runEmbeddedTransaction 
          false
          (ENR $ EnumeratedRoleType sysUser)
          (createAndAddRoleInstance (EnumeratedRoleType indexedRole) mysystem
            (RolSerialization 
              { id: Nothing
              , binding: Just rid
              , properties: PropertySerialization (singleton indexedRoleName [iName])
              }))

forkJustInTimeModelLoader :: AVar JustInTimeModelLoad -> AVar PerspectivesState -> Aff Unit
forkJustInTimeModelLoader modelToLoadAVar state = run
  where
    run :: Aff Unit
    run = do
      modelLoad <- take modelToLoadAVar
      case modelLoad of
        LoadModel dfId -> do
          -- Create an AVar for communication between the requester and the fiber we'll fork shortly:
          hotline <- empty
          -- Return the hotline to the requester:
          put (HotLine hotline) modelToLoadAVar
          -- now fork a process that will actually load the model (and it will use the HotLine to 
          -- return the results to the requester):
          loadingProcess <- forkAff $ do
            runPerspectivesWithState 
              (runEmbeddedIfNecessary
                doNotShareWithPeers 
                (ENR $ EnumeratedRoleType sysUser) 
                (do 
                  log ("Will just-in-time load " <> show dfId)
                  void $ addModelToLocalStore dfId isInitialLoad))
              state
            -- Report back to retrieveDomeinFile on the Aff level, i.e. when the embedded loading on the level of
            -- MonadPerspectives has been run.
            liftAff $ put ModelLoaded hotline
          catchError (joinFiber loadingProcess)
            \e -> do
              logPerspectivesError $ Custom $ "Embedded (just in time) model load failed, because: " <> show e
              liftAff $ put (LoadingFailed $ show e) hotline
          -- and repeat
          run
        Stop -> pure unit
        -- Other cases should not happen; we just ignore them here.
        _ -> run

forkReferentialIntegrityFixer :: AVar IntegrityFix -> AVar PerspectivesState -> Aff Unit
forkReferentialIntegrityFixer missingResourceAVar state = run
  where
    run :: Aff Unit
    run = do
      -- Create an AVar for communication between the requester and the fiber we'll fork shortly:
      hotline <- empty
      integrityFix <- take missingResourceAVar
      case integrityFix of 
        Missing missingResource -> do 
          -- Return the hotline to the requester. It will take it from the missingResourceAVar and stop execution until we put something in the HotLine.
          put (FixingHotLine hotline) missingResourceAVar
          -- now fork a process that will actually load the model (and it will use the HotLine to 
          -- return the results to the requester):
          fixingProcess <- forkAff $ do
            runPerspectivesWithState 
              (fixReferences missingResource)
              state
          -- This blocks until the fiber completes.
          result <- try (joinFiber fixingProcess)
          case result of 
            Left e -> do
              logPerspectivesError $ Custom $ "Fixing referential integrity failed, because: " <> show e
              liftAff $ put (FixFailed $ show e) hotline
            -- Report back to the caller on the Aff level, i.e. when the fixing on the level of
            -- MonadPerspectives has been executed.
            Right _ -> liftAff $ put FixSucceeded hotline
          -- and repeat
          run
        -- This we never send, currently.
        StopFixing -> pure unit
        _ -> run

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = logPerspectivesError $Custom $ "An error condition: " <> (show e)
handleError (Right _) = pure unit

createAccount :: UserName -> Foreign -> RuntimeOptions -> Nullable Foreign -> ({success :: Boolean, reason :: Nullable String} -> Effect Unit) -> Effect Unit
createAccount perspectivesUser rawPouchdbUser runtimeOptions nullableIdentityDocument callback = void $ runAff handler
  case decodePouchdbUser' rawPouchdbUser of
    Left e -> throwError (error $ "Wrong format for parameter 'rawPouchdbUser' in createAccount: " <> (show e))
    Right (pouchdbUser :: PouchdbUser) -> do 
      -- Set the current PDR version.
      idbSet "CurrentPDRVersion" (unsafeCoerce pdrVersion)

      transactionFlag <- new true
      brokerService <- empty
      transactionWithTiming <- empty
      modelToLoad <- empty
      indexedResourceToCreate <- empty
      missingResource <- empty
      state <- getCurrentLanguageFromIDB >>= new <<< newPerspectivesState 
        pouchdbUser 
        transactionFlag 
        transactionWithTiming 
        modelToLoad 
        runtimeOptions 
        brokerService 
        indexedResourceToCreate 
        missingResource
      
      -- Fork aff to load models just in time.
      void $ forkAff $ forkJustInTimeModelLoader modelToLoad state
      -- Fork aff to save to the database
      -- TODO: beeindig database persistence.
      void $ forkAff $ forkDatabasePersistence state
      -- Fork aff to create indexed roles and contexts.
      void $ forkAff $ forkCreateIndexedResources indexedResourceToCreate state
      -- Fork aff to restore referential integrity
      void $ forkAff $ forkReferentialIntegrityFixer missingResource state

      -- Set up.
      runPerspectivesWithState
        (do
          addAllExternalFunctions
          key <- getPrivateKey
          modify \(s@{runtimeOptions:ro}) -> s {runtimeOptions = ro {privateKey = unsafeCoerce key}}
          getSystemIdentifier >>= createUserDatabases
          setupUser (UninterpretedTransactionForPeer <$> (toMaybe nullableIdentityDocument))
          saveMarkedResources
          )
        state
      -- and stop
      put Stop modelToLoad
  where
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in createAccount: " <> (show e)
      callback {success: false, reason: toNullable $ Just $ show e}
    handler (Right _) = do
      logPerspectivesError $ Custom $ "Created an account " <> perspectivesUser
      callback {success: true, reason: toNullable Nothing}

reCreateInstances :: Foreign -> RuntimeOptions -> (Boolean -> Effect Unit) -> Effect Unit
reCreateInstances rawPouchdbUser options callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left _ -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in reCreateInstances")
      Right (pouchdbUser :: PouchdbUser) -> do
        transactionFlag <- new true
        brokerService <- empty
        transactionWithTiming <- empty
        modelToLoad <- empty
        indexedResourceToCreate <- empty
        missingResource <- empty
        state <- getCurrentLanguageFromIDB >>= new <<< newPerspectivesState 
          pouchdbUser 
          transactionFlag 
          transactionWithTiming 
          modelToLoad 
          options 
          brokerService 
          indexedResourceToCreate 
          missingResource
        -- Fork aff to create indexed roles and contexts.
        void $ forkAff $ forkCreateIndexedResources indexedResourceToCreate state
        -- Fork aff to restore referential integrity
        void $ forkAff $ forkReferentialIntegrityFixer missingResource state
        runPerspectivesWithState
          (do
            -- Clear the databases.
            clearUserDatabase
            clearPostDatabase
            -- clear the caches, otherwise nothing happens.
            resetCaches
            addAllExternalFunctions
            key <- getPrivateKey
            modify \(s@{runtimeOptions}) -> s {runtimeOptions = runtimeOptions {privateKey = unsafeCoerce key}}
            getSystemIdentifier >>= createUserDatabases
            reSetupUser
            saveMarkedResources)
          state
  where
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in reCreateInstances: " <> (show e)
      callback false
    handler (Right _) = do
      logPerspectivesError $ Custom $ "Succesfully re-created the instances of this installation."
      callback true


-- | This is for development only! Assumes the user identifier equals the user name.
resetAccount :: UserName -> Foreign -> RuntimeOptions -> (Boolean -> Effect Unit) -> Effect Unit
resetAccount usr rawPouchdbUser options callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left _ -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in resetAccount")
      Right (pouchdbUser :: PouchdbUser) -> do
        transactionFlag <- new true
        brokerService <- empty
        transactionWithTiming <- empty
        modelToLoad <- empty
        indexedResourceToCreate <- empty
        missingResource <- empty
        state <- getCurrentLanguageFromIDB >>= new <<< newPerspectivesState 
          pouchdbUser 
          transactionFlag 
          transactionWithTiming 
          modelToLoad 
          options 
          brokerService 
          indexedResourceToCreate
          missingResource
        runPerspectivesWithState
          (do
            (catchError do
                -- Get all Channels
                getChannels <- getRoleFunction "sys:PerspectivesSystem$Channels"
                system <- getMySystem
                (channels :: Array ContextInstance) <- (ContextInstance system) ##= getChannels >=> context
                -- End their replication
                void $ withCouchdbUrl \url -> for_ channels (endChannelReplication url)
                -- remove the databases
                getChannelDbId <- getPropertyFunction "sys:Channel$External$ChannelDatabaseName"
                for_ channels \c -> (c ##>> externalRole >=> getChannelDbId) >>= deleteDb <<< unwrap
              \e -> logPerspectivesError $ Custom $ "Could not delete channel databases because: " <> show e)
            clearUserDatabase
            clearModelDatabase
            clearPostDatabase
            -- clear the caches, otherwise nothing happens.
            resetCaches
            addAllExternalFunctions
            key <- getPrivateKey
            modify \(s@{runtimeOptions}) -> s {runtimeOptions = runtimeOptions {privateKey = unsafeCoerce key}}
            getSystemIdentifier >>= createUserDatabases
            setupUser Nothing
            saveMarkedResources)
          state
    where

      clearModelDatabase :: MonadPerspectives Unit
      clearModelDatabase = do
        dbname <- modelsDatabaseName
        catchError (deleteDatabase dbname)
          \_ -> createDatabase dbname
        createDatabase dbname
        -- If this is a remote (Couchdb) database, set a security policy:
        mcouchdbUrl <- gets _.couchdbUrl
        case mcouchdbUrl of
          Nothing -> pure unit
          _ -> do
            -- We need to do this because we use the Pouchdb adapter.
            -- Pouchdb actually creates the database only with the first action on it.
            -- As setSecurityDocument is implemented using Affjax, it bypasses Pouchdb
            -- and tries to set a security policy on a database that does not yet exist.
            void $ databaseInfo dbname
            -- Now set the security document such that there is no role restriction for members.
            -- (only applies to Couchdb backends).
            void $ withCouchdbUrl \url -> setSecurityDocument url dbname
                (SecurityDocument {admins: {names: Just [], roles: ["_admin"]}, members: {names: Just [], roles: []}})

      handler :: Either Error Unit -> Effect Unit
      handler (Left e) = do
        logPerspectivesError $ Custom $ "An error condition in resetAccount: " <> (show e)
        callback false
      handler (Right _) = do
        logPerspectivesError $ Custom $ "Reset the account " <> usr
        callback true

clearPostDatabase :: MonadPerspectives Unit
clearPostDatabase = do
  dbname <- postDatabaseName
  catchError (deleteDatabase dbname)
    \_ -> createDatabase dbname
  createDatabase dbname

clearUserDatabase :: MonadPerspectives Unit
clearUserDatabase = do
  userDatabaseName <- entitiesDatabaseName
  catchError (deleteDatabase userDatabaseName)
    \_ -> createDatabase userDatabaseName
  createDatabase userDatabaseName

deleteDb :: DatabaseName -> MonadPerspectives Unit
deleteDb dbname = do
  r <- try (deleteDatabase dbname)
  case r of
    Left e -> logPerspectivesError $ Custom (show e)
    Right _ -> pure unit

-- NOTE: Only local databases are removed.
removeAccount :: UserName -> Foreign -> (Boolean -> Effect Unit) -> Effect Unit
removeAccount usr rawPouchdbUser callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left _ -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in removeAccount")
      Right (pdbu :: PouchdbUser) -> do
        (pouchdbUser :: PouchdbUser) <- pure 
          { systemIdentifier: pdbu.systemIdentifier 
          , perspectivesUser: pdbu.perspectivesUser
          , userName: pdbu.userName
          , password: pdbu.password
          , couchdbUrl: pdbu.couchdbUrl
          }
        transactionFlag <- new true
        brokerService <- empty
        transactionWithTiming <- empty
        modelToLoad <- empty
        indexedResourceToCreate <- empty
        missingResource <- empty
        state <- getCurrentLanguageFromIDB >>= new <<< newPerspectivesState 
          pouchdbUser 
          transactionFlag 
          transactionWithTiming 
          modelToLoad 
          defaultRuntimeOptions 
          brokerService 
          indexedResourceToCreate
          missingResource
        runPerspectivesWithState
          do
            -- Get all Channels
            getChannels <- getRoleFunction "sys:PerspectivesSystem$Channels"
            -- End their replication
            system <- getMySystem
            (channels :: Array ContextInstance) <- (ContextInstance system) ##= getChannels >=> context
            void $ withCouchdbUrl \url -> for_ channels (endChannelReplication url)
            -- remove the channels
            getChannelDbId <- getPropertyFunction "sys:Channel$External$ChannelDatabaseName"
            for_ channels \c -> (c ##>> externalRole >=> getChannelDbId) >>= deleteDb <<< unwrap
            -- remove the databases
            entitiesDatabaseName >>= deleteDb
            modelsDatabaseName >>= deleteDb
            postDatabaseName >>= deleteDb
            invertedQueryDatabaseName >>= deleteDb
            liftAff $ liftEffect clear
          state
  where
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in removeAccount: " <> (show e)
      callback false
    handler (Right _) = do
      logPerspectivesError $ Custom $ "removeAccount an account " <> usr
      callback true

-- | Retrieve all instances of sys:Model$IndexedRole and sys:Model$IndexedContext and create a table of
-- | all known indexed names and their private replacements in PerspectivesState.
-- | By retrieving the role and context instances directly from Couchdb using a view, we don't have to 
-- | rely on model://perspectives.domains$System in this early stage of starting up.
addIndexedNames :: MonadPerspectives Unit
addIndexedNames = do
  (roleInstances :: Array RoleInstance) <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb [indexedRole] (ContextInstance "")))
  iRoles <- indexedRoles_ roleInstances
  contextInstances <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb [indexedContext] (ContextInstance "")))
  iContexts <- indexedContexts_ contextInstances
  modify \ps -> ps {indexedRoles = iRoles, indexedContexts = iContexts}

-- | Recompiles all models in an installation. Use this function when the definition of the DomeinFile
-- | has changed. The local models directory of the user that is provided, will have the freshly compiled
-- | DomeinFiles, so this user can be booted. 
recompileLocalModels :: Foreign -> (Boolean -> Effect Unit) -> Effect Unit
recompileLocalModels rawPouchdbUser callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left e -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in resetAccount")
      Right (pouchdbUser :: PouchdbUser) -> do
        transactionFlag <- new true
        brokerService <- empty
        transactionWithTiming <- empty
        modelToLoad <- empty
        indexedResourceToCreate <- empty 
        missingResource <- empty
        state <- getCurrentLanguageFromIDB >>= new <<< newPerspectivesState
          pouchdbUser 
          transactionFlag 
          transactionWithTiming 
          modelToLoad 
          defaultRuntimeOptions 
          brokerService 
          indexedResourceToCreate
          missingResource
        runPerspectivesWithState RECOMPILE.recompileLocalModels state
  where
    handler :: Either Error Boolean -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in recompileLocalModels: " <> (show e)
      callback false
    handler (Right e) = do
      logPerspectivesError $ Custom $ "Basic models recompiled!"
      callback e


retrieveAllCredentials :: MonadPerspectives Unit 
retrieveAllCredentials = do
  -- All instances with type model://perspectives.domains#System$WithCredentials.
  (roleInstances :: Array RoleInstance) <- entitiesDatabaseName >>= \db -> getViewOnDatabase db "defaultViews/credentialsView" (NoKey :: Keys String)
  userNameGetter <- getterFromPropertyType (CP $ CalculatedPropertyType userWithCredentialsUsername) 
  rows :: Array (Tuple String Credential) <- foldM
    (\rows' roleId -> (try (roleId ##>> getPropertyFromTelescope (EnumeratedPropertyType userWithCredentialsPassword))) >>= 
      handlePerspectRolError' "retrieveAllCredentials_Password" rows' 
        \pw -> (try (roleId ##>> getPropertyFromTelescope (EnumeratedPropertyType userWithCredentialsAuthorizedDomain))) >>=
          handlePerspectRolError' "retrieveAllCredentials_AuthorizedDomain" rows' 
            (\authorizedDomain ->  (try (roleId ##>> userNameGetter)) >>=
              handlePerspectRolError' "retrieveAllCredentials_Username" rows' 
                (\username -> pure $ cons (Tuple (unwrap authorizedDomain) (Credential (takeGuid (unwrap username)) (unwrap pw))) rows')))
    []
    roleInstances
  modify \s@{couchdbCredentials} -> s {couchdbCredentials = couchdbCredentials <> fromFoldable rows}

handleClientRequest :: Foreign
handleClientRequest = Proxy.handleClientRequest