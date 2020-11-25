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

module Main where
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Writer (runWriterT)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Error, catchError, forkAff, joinFiber, runAff, try)
import Effect.Aff.AVar (AVar, new)
import Effect.Class.Console (log)
import Perspectives.AMQP.IncomingPost (retrieveBrokerService, incomingPost)
import Perspectives.Api (setupApi)
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>>))
import Perspectives.Couchdb (DatabaseName, SecurityDocument(..))
import Perspectives.Couchdb.Databases (createDatabase, deleteDatabase, setSecurityDocument)
import Perspectives.CouchdbState (CouchdbUser(..), UserName(..))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Extern.Couchdb (modelsDatabaseName, roleInstancesFromCouchdb)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Instances.Indexed (indexedContexts_, indexedRoles_)
import Perspectives.Instances.ObjectGetters (context, externalRole)
import Perspectives.LocalAuthentication (AuthenticationResult(..))
import Perspectives.LocalAuthentication (authenticate) as LA
import Perspectives.Names (getMySystem)
import Perspectives.Persistent (entitiesDatabaseName, postDatabaseName)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.RunPerspectives (runPerspectives, runPerspectivesWithState)
import Perspectives.SetupCouchdb (createAnotherPerspectivesUser, setupPerspectivesInCouchdb)
import Perspectives.SetupUser (setupUser)
import Perspectives.Sync.Channel (endChannelReplication)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (<$>), (<<<), (<>), (>=>), (>>=))

-- | Runs the PDR with default credentials. Used for testing clients without authentication.
main :: Effect Unit
main = runPDR
  "cor"
  "geheim"
  (CouchdbUser
    { userName: UserName "cor"
    , systemIdentifier: "cor"
    , _rev: Nothing})
  "http://127.0.0.1"
  5984
-- main = parseFromCommandLine

-- NOTE: For release v0.1.0, I've commented out the original main function because it requires the perspectivesproxy
-- While this is available, it is webpacked for usage in the browser and will not run from the command line
-- (The PDR is not meant for command line usage).

-- | Execute the Perspectives Distributed Runtime by creating a listener to the internal channel.
  -- TODO. Bewaar de fiber in state en kill om uit te loggen?
runPDR :: String -> String -> CouchdbUser -> String -> Int -> Effect Unit
runPDR usr pwd couchdbUser host port = void $ runAff handleError do
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  state <- new $ newPerspectivesState couchdbUser
    host
    port
    pwd
    av
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
        log $ "API stopped because of: " <> show e
        log "Will start the API again."
  -- connected <- runPerspectivesWithState (connectedToAMQPBroker >>= \c -> log ("Connected to RabbitMQ? = " <> show c)) state
  postFiber <- forkAff $ runPerspectivesWithState incomingPost state
  catchError (joinFiber postFiber)
    \e -> do
      log $ "Stopped handling incoming post because of: " <> show e
  -- void $ forkAff $ forever do
  --   postFiber <- forkAff $ runPerspectivesWithState incomingPost state
  --   catchError (joinFiber postFiber)
  --     \e -> do
  --       log $ "Stopped handling incoming post because of: " <> show e
  --       log "Will start the handler again."
  -- void $ forkAff $ runPerspectivesWithState setupTcpApi state

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Perspectives-core has started!"

-- | The main entrance to the PDR for client programs. Runs the PDR on succesful login.
-- | When Couchdb is in Party Mode, initialises the first admin.
authenticate :: String -> String -> String -> Int -> (Int -> Effect Unit) -> Effect Unit
authenticate usr pwd host port callback = void $ runAff handler (LA.authenticate usr pwd host port)
  where
    handler :: Either Error AuthenticationResult -> Effect Unit
    handler (Left e) = do
      log $ "An error condition: " <> (show e)
      callback 1
    handler (Right r) = case r of
      -- Not a valid system administrator for Couchdb.
      WrongCredentials -> callback 0
      -- System administrator, but not a Perspectives User. We'll make him one.
      UnknownUser -> void $ runAff runWithUser do
        -- There may not yet be another Perspectives user. If so, we need to set up
        -- system databases etc.
        setupPerspectivesInCouchdb usr pwd host port
        createAnotherPerspectivesUser usr pwd host port
      -- System administrator and a Perspectives User.
      OK couchdbUser -> runWithUser $ Right couchdbUser
    runWithUser :: Either Error CouchdbUser -> Effect Unit
    runWithUser (Left e) = do
      log $ "Could not create another Perspectives user, because: " <> (show e)
      callback 1
    runWithUser (Right couchdbUser) = do
      runPDR usr pwd couchdbUser host port
      callback 2

-- | This is for development only! Assumes the user identifier equals the user name.
resetAccount :: String -> String -> String -> Int -> (Boolean -> Effect Unit) -> Effect Unit
resetAccount usr pwd host port callback = void $ runAff handler (runPerspectives usr pwd usr host port do
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
      -- Now set the security document such that there is no role restriction for members.
      setSecurityDocument dbname
        (SecurityDocument {admins: {names: [], roles: ["_admin"]}, members: {names: [], roles: []}})

    clearPostDatabase :: MonadPerspectives Unit
    clearPostDatabase = do
      dbname <- postDatabaseName
      catchError (deleteDatabase dbname)
        \_ -> createDatabase dbname
      createDatabase dbname
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      log $ "An error condition in resetAccount: " <> (show e)
      callback false
    handler (Right e) = do
      log $ "Cleared the account " <> usr
      callback true

    deleteDb :: DatabaseName -> MonadPerspectives Unit
    deleteDb dbname = do
      r <- try (deleteDatabase dbname)
      case r of
        Left e -> log (show e)
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
