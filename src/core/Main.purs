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
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Writer (runWriterT)
import Data.Array (catMaybes, cons, filter, foldM)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (insert)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), catchError, delay, error, forkAff, joinFiber, runAff, throwError, try)
import Effect.Aff.AVar (AVar, empty, new, put, take)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Foreign (Foreign)
import Foreign.Object (fromFoldable)
import Main.RecompileBasicModels (UninterpretedDomeinFile, executeInTopologicalOrder, recompileModel)
import Perspectives.AMQP.IncomingPost (retrieveBrokerService, incomingPost)
import Perspectives.Api (setupApi)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, PerspectivesState, RepeatingTransaction(..), (##=), (##>>))
import Perspectives.Couchdb (SecurityDocument(..))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Error.Boundaries (handlePerspectRolError')
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (modelsDatabaseName, roleInstancesFromCouchdb)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (isModelName)
import Perspectives.Instances.Indexed (indexedContexts_, indexedRoles_)
import Perspectives.Instances.ObjectGetters (context, externalRole, getProperty)
import Perspectives.ModelDependencies (indexedContext, indexedRole, sysUser, userWithCredentials, userWithCredentialsAuthorizedDomain, userWithCredentialsPassword)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (DatabaseName, Password, PouchdbUser, Url, UserName, createDatabase, databaseInfo, decodePouchdbUser', deleteDatabase, documentsInDatabase, includeDocs)
import Perspectives.Persistence.CouchdbFunctions (setSecurityDocument)
import Perspectives.Persistence.State (getSystemIdentifier, withCouchdbUrl)
import Perspectives.Persistent (entitiesDatabaseName, postDatabaseName)
import Perspectives.PerspectivesState (newPerspectivesState, resetCaches)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction, getRoleFunction)
import Perspectives.Repetition (Duration, fromDuration)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..), StateIdentifier)
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction, runMonadPerspectivesTransaction')
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupCouchdb (createPerspectivesUser, createUserDatabases, setupPerspectivesInCouchdb)
import Perspectives.SetupUser (setupUser)
import Perspectives.Sync.Channel (endChannelReplication)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (*>), (+), (-), (<), (<$>), (<<<), (<>), (>), (>=>), (>>=))
import Simple.JSON (read)

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
runPDR :: UserName -> Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
runPDR usr rawPouchdbUser publicRepo callback = void $ runAff handler do
  case decodePouchdbUser' rawPouchdbUser of
    Left _ -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in runPDR")
    Right (pouchdbUser :: PouchdbUser) -> do
      transactionFlag <- new 0
      transactionWithTiming <- empty
      state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag transactionWithTiming
      
      -- Fork aff to capture transactions to run.
      void $ forkAff $ forkTimedTransactions transactionWithTiming state

      runPerspectivesWithState (do
        addAllExternalFunctions
        addIndexedNames
        retrieveAllCredentials
        retrieveBrokerService)
        state
      void $ forkAff $ forever do
        apiFiber <- forkAff $ runPerspectivesWithState setupApi state
        catchError (joinFiber apiFiber)
          \e -> do
            logPerspectivesError $ Custom $ "API stopped and restarted because of: " <> show e
      -- Trial and error shows us that if we apply the pattern used for setupApi to incomingPost,
      -- The application will not run well.
      void $ forkAff $ do
        postFiber <- forkAff $ runPerspectivesWithState incomingPost state
        catchError (joinFiber postFiber)
          \e -> do
            logPerspectivesError $ Custom $ "Stopped handling incoming post because of: " <> show e
  where
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

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = logPerspectivesError $Custom $ "An error condition: " <> (show e)
handleError (Right _) = pure unit

-- | Call this function from the client to initialise the Persistence of Perspectives.
-- | Implementation notes:
-- |  1. we need credentials if Couchdb is the database backend.
-- |  2. couchdbUrl should terminate on a forward slash.
initialisePersistence :: UserName -> Password -> Maybe Url -> Effect Unit
initialisePersistence userName password couchdbUrl = void $ runAff
  handleError
  (setupPerspectivesInCouchdb userName password couchdbUrl)

-- | Implementation notes:
-- |  1. couchdbUrl should terminate on a forward slash.
createUser :: UserName -> Password -> Maybe Url -> Effect Unit
createUser userName password couchdbUrl = void $ runAff
  handleError
  (createPerspectivesUser userName password couchdbUrl)

createAccount :: UserName -> Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
createAccount usr rawPouchdbUser publicRepo callback = void $ runAff handler
  case decodePouchdbUser' rawPouchdbUser of
    Left _ -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in createAccount")
    Right (pouchdbUser :: PouchdbUser) -> do
      transactionFlag <- new 0
      transactionWithTiming <- empty
      state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag transactionWithTiming
      runPerspectivesWithState
        (do
          addAllExternalFunctions
          -- TODO. Vermoedelijk is dit overbodig voor Pouchdb.
          getSystemIdentifier >>= createUserDatabases
          setupUser
          )
        state
  where
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in createAccount: " <> (show e)
      callback false
    handler (Right _) = do
      logPerspectivesError $ Custom $ "Created an account " <> usr
      callback true

-- | This is for development only! Assumes the user identifier equals the user name.
resetAccount :: UserName -> Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
resetAccount usr rawPouchdbUser publicRepo callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left _ -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in resetAccount")
      Right (pouchdbUser :: PouchdbUser) -> do
        transactionFlag <- new 0
        transactionWithTiming <- empty
        state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag transactionWithTiming
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
            setupUser)
          state
    where

      clearUserDatabase :: MonadPerspectives Unit
      clearUserDatabase = do
        userDatabaseName <- entitiesDatabaseName
        catchError (deleteDatabase userDatabaseName)
          \_ -> createDatabase userDatabaseName
        createDatabase userDatabaseName
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

      clearPostDatabase :: MonadPerspectives Unit
      clearPostDatabase = do
        dbname <- postDatabaseName
        catchError (deleteDatabase dbname)
          \_ -> createDatabase dbname
        createDatabase dbname
      handler :: Either Error Unit -> Effect Unit
      handler (Left e) = do
        logPerspectivesError $ Custom $ "An error condition in resetAccount: " <> (show e)
        callback false
      handler (Right _) = do
        logPerspectivesError $ Custom $ "Reset the account " <> usr
        callback true

deleteDb :: DatabaseName -> MonadPerspectives Unit
deleteDb dbname = do
  r <- try (deleteDatabase dbname)
  case r of
    Left e -> logPerspectivesError $ Custom (show e)
    Right _ -> pure unit

-- NOTE: Only local databases are removed.
removeAccount :: UserName -> Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
removeAccount usr rawPouchdbUser publicRepo callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left _ -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in removeAccount")
      Right (pdbu :: PouchdbUser) -> do
        (pouchdbUser :: PouchdbUser) <- pure
          { systemIdentifier: pdbu.systemIdentifier
          , password: pdbu.password
          , couchdbUrl: pdbu.couchdbUrl
          }
        transactionFlag <- new 0
        transactionWithTiming <- empty
        state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag transactionWithTiming
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
addIndexedNames :: MonadPerspectives Unit
addIndexedNames = do
  (roleInstances :: Array RoleInstance) <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb [indexedRole] (ContextInstance "")))
  iRoles <- indexedRoles_ roleInstances
  contextInstances <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb [indexedContext] (ContextInstance "")))
  iContexts <- indexedContexts_ contextInstances
  modify \ps -> ps {indexedRoles = iRoles, indexedContexts = iContexts}

-- | Recompiles a number of essential models. Use this function when the definition of the DomeinFile
-- | has changed. The local models directory of the user that is provided, will have the freshly compiled
-- | DomeinFiles, so this user can be booted. The essential models include model:ModelManagement, so that
-- | using this account, all models can be recompiled from the client.
-- | These functions depend heavily on the Perspectives types in model:System and
-- | model:ModelManagement.
recompileBasicModels :: Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
recompileBasicModels rawPouchdbUser publicRepo callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left e -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in resetAccount")
      Right (pouchdbUser :: PouchdbUser) -> do
        transactionFlag <- new 0
        transactionWithTiming <- empty
        state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag transactionWithTiming
        runPerspectivesWithState
          (do
            addAllExternalFunctions
            modelsDb <- modelsDatabaseName
            {rows:allModels} <- documentsInDatabase modelsDb includeDocs
            uninterpretedDomeinFiles <- for (filter (isModelName <<< _.id) allModels) \({id, doc}) -> case read <$> doc of
              Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
              Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
              Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
            r <- runMonadPerspectivesTransaction'
              false
              (ENR $ EnumeratedRoleType sysUser)
              (runExceptT (executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModel))
            case r of 
              Left errors -> logPerspectivesError (Custom ("recompileModelsAtUrl: " <> show errors)) 
              _ -> pure unit
          )
          state
  where
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in recompileBasicModels: " <> (show e)
      callback false
    handler (Right e) = do
      logPerspectivesError $ Custom $ "Basic models recompiled!"
      callback true

retrieveAllCredentials :: MonadPerspectives Unit
retrieveAllCredentials = do
  (roleInstances :: Array RoleInstance) <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb [userWithCredentials] (ContextInstance "")))
  rows <- foldM
    (\rows' roleId -> (try (roleId ##>> getProperty (EnumeratedPropertyType userWithCredentialsPassword))) >>=
      handlePerspectRolError' "retrieveAllCredentials_Password" rows'
        \pw -> (try (roleId ##>> getProperty (EnumeratedPropertyType userWithCredentialsAuthorizedDomain))) >>=
          handlePerspectRolError' "retrieveAllCredentials_AuthorizedDomain" rows' 
            (\authorizedDomain -> pure $ cons (Tuple (unwrap authorizedDomain) (unwrap pw)) rows')
        )
    []
    roleInstances
  modify \s@{couchdbCredentials} -> s {couchdbCredentials = couchdbCredentials <> fromFoldable rows}
