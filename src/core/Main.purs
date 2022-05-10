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
import Main.RecompileBasicModels as RBM
import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Writer (runWriterT)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Error, catchError, error, forkAff, joinFiber, runAff, throwError, try)
import Effect.Aff.AVar (new)
import Foreign (Foreign)
import Perspectives.AMQP.IncomingPost (retrieveBrokerService, incomingPost)
import Perspectives.Api (setupApi)
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>>))
import Perspectives.Couchdb (SecurityDocument(..))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (modelsDatabaseName, roleInstancesFromCouchdb)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Instances.Indexed (indexedContexts_, indexedRoles_)
import Perspectives.Instances.ObjectGetters (context, externalRole)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (DatabaseName, Password, PouchdbUser, Url, UserName, createDatabase, databaseInfo, decodePouchdbUser', deleteDatabase)
import Perspectives.Persistence.CouchdbFunctions (setSecurityDocument)
import Perspectives.Persistence.State (getSystemIdentifier, withCouchdbUrl)
import Perspectives.Persistent (entitiesDatabaseName, postDatabaseName)
import Perspectives.PerspectivesState (newPerspectivesState, resetCaches)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupCouchdb (createPerspectivesUser, createUserDatabases, setupPerspectivesInCouchdb)
import Perspectives.SetupUser (setupUser)
import Perspectives.Sync.Channel (endChannelReplication) 
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (<$>), (<<<), (<>), (>=>), (>>=), (>>>))

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
    Right (pdbu :: PouchdbUser) -> do
      (pouchdbUser :: PouchdbUser) <- pure
        { systemIdentifier: pdbu.systemIdentifier
        , password: pdbu.password
        , couchdbUrl: pdbu.couchdbUrl
      }
      transactionFlag <- new true
      state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag

      runPerspectivesWithState (do
        addAllExternalFunctions
        addIndexedNames
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
    Right (pdbu :: PouchdbUser) -> do
      (pouchdbUser :: PouchdbUser) <- pure
        { systemIdentifier: pdbu.systemIdentifier
        , password: pdbu.password
        , couchdbUrl: pdbu.couchdbUrl
        }
      transactionFlag <- new true
      state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag
      runPerspectivesWithState
        (do
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
        transactionFlag <- new true
        state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag
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
        mcouchdbUrl <- gets (_.userInfo >>> _.couchdbUrl)
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
        transactionFlag <- new true
        state <- new $ newPerspectivesState pouchdbUser publicRepo transactionFlag
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
  (roleInstances :: Array RoleInstance) <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb ["model:System$Model$IndexedRole"] (ContextInstance "")))
  iRoles <- indexedRoles_ roleInstances
  contextInstances <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb ["model:System$Model$IndexedContext"] (ContextInstance "")))
  iContexts <- indexedContexts_ contextInstances
  modify \ps -> ps {indexedRoles = iRoles, indexedContexts = iContexts}

recompileBasicModels :: Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
recompileBasicModels = RBM.recompileBasicModels
