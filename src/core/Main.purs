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

module Main where
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
import Perspectives.Couchdb (DatabaseName, SecurityDocument(..))
import Perspectives.Couchdb.Databases (setSecurityDocument)
import Perspectives.CouchdbState (UserName(..)) as CDBstate
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (modelsDatabaseName, roleInstancesFromCouchdb)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Instances.Indexed (indexedContexts_, indexedRoles_)
import Perspectives.Instances.ObjectGetters (context, externalRole)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (Password, PouchdbUser, PouchdbUser', Url, UserName, createDatabase, databaseInfo, decodePouchdbUser', deleteDatabase)
import Perspectives.Persistent (entitiesDatabaseName, postDatabaseName)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.RunPerspectives (runPerspectives, runPerspectivesWithState)
import Perspectives.SetupCouchdb (createPerspectivesUser, setupPerspectivesInCouchdb)
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
runPDR :: UserName -> Password -> Foreign -> Url -> Effect Unit
runPDR usr pwd rawPouchdbUser publicRepo = void $ runAff handleError do
  case decodePouchdbUser' rawPouchdbUser of
    Left e -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in runPDR")
    Right (pdbu :: PouchdbUser'()) -> do
      (pouchdbUser :: PouchdbUser) <- pure
        { _rev: pdbu._rev
        , systemIdentifier: pdbu.systemIdentifier
        , password: pdbu.password
        , couchdbUrl: pdbu.couchdbUrl
        , userName: CDBstate.UserName usr}
      state <- new $ newPerspectivesState pouchdbUser
        -- TODO. Deze twee waarden kunnen weg zodra Perspectives.Persistence.API alles heeft overgenomen.
        "https://localhost"
        6984
        pwd
        publicRepo
      runPerspectivesWithState (do
        void $ setupUser
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
      postFiber <- forkAff $ runPerspectivesWithState incomingPost state
      catchError (joinFiber postFiber)
        \e -> do
          logPerspectivesError $ Custom $ "Stopped handling incoming post because of: " <> show e

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = logPerspectivesError $Custom $ "An error condition: " <> (show e)
handleError (Right a) = pure unit

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

-- | This is for development only! Assumes the user identifier equals the user name.
-- TODO. Als Perspectives.Persistence.API alles heeft overgenomen, kunnen we hier een PouchdbUser meegeven.
resetAccount :: String -> String -> String -> Int -> String -> (Boolean -> Effect Unit) -> Effect Unit
resetAccount usr pwd host port publicRepo callback = void $ runAff handler (runPerspectives usr pwd usr host port publicRepo do
  -- Get all Channels
  getChannels <- getRoleFunction "sys:PerspectivesSystem$Channels"
  system <- getMySystem
  (channels :: Array ContextInstance) <- (ContextInstance system) ##= getChannels >=> context
  -- End their replication
  for_ channels endChannelReplication
  -- remove the databases
  getChannelDbId <- getPropertyFunction "sys:Channel$External$ChannelDatabaseName"
  for_ channels \c -> (c ##>> externalRole >=> getChannelDbId) >>= deleteDb <<< unwrap
  clearUserDatabase
  clearModelDatabase
  clearPostDatabase)
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
        otherwise -> do
          -- We need to do this because we use the Pouchdb adapter.
          -- Pouchdb actually creates the database only with the first action on it.
          -- As setSecurityDocument is implemented using Affjax, it bypasses Pouchdb
          -- and tries to set a security policy on a database that does not yet exist.
          void $ databaseInfo dbname
          -- Now set the security document such that there is no role restriction for members.
          void $ setSecurityDocument dbname
            (SecurityDocument {_id: "_security", admins: {names: [], roles: ["_admin"]}, members: {names: [], roles: []}})

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
    handler (Right e) = do
      logPerspectivesError $ Custom $ "Cleared the account " <> usr
      callback true

    deleteDb :: DatabaseName -> MonadPerspectives Unit
    deleteDb dbname = do
      r <- try (deleteDatabase dbname)
      case r of
        Left e -> logPerspectivesError $ Custom (show e)
        Right _ -> pure unit


-- | Retrieve all instances of sys:Model$IndexedRole and sys:Model$IndexedContext and create a table of
-- | all known indexed names and their private replacements in PerspectivesState.
addIndexedNames :: MonadPerspectives Unit
addIndexedNames = do
  (roleInstances :: Array RoleInstance) <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb ["model:System$Model$IndexedRole"] (ContextInstance "")))
  iRoles <- indexedRoles_ roleInstances
  contextInstances <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb ["model:System$Model$IndexedContext"] (ContextInstance "")))
  iContexts <- indexedContexts_ contextInstances
  modify \ps -> ps {indexedRoles = iRoles, indexedContexts = iContexts}
