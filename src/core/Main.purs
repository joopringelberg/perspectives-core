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
import Control.Monad.Writer (runWriterT)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Error, forkAff, runAff)
import Effect.Aff.AVar (AVar, new)
import Effect.Console (log)
import Perspectives.Api (setupApi, setupTcpApi)
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>>))
import Perspectives.Couchdb.Databases (createDatabase, deleteDatabase)
import Perspectives.CouchdbState (CouchdbUser(..), UserName(..))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Extern.Couchdb (modelsDatabaseName, roleInstancesFromCouchdb)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Instances.Indexed (indexedContexts_, indexedRoles_)
import Perspectives.Instances.ObjectGetters (context, externalRole, getProperty)
import Perspectives.LocalAuthentication (AuthenticationResult(..))
import Perspectives.LocalAuthentication (authenticate) as LA
import Perspectives.Names (getMySystem)
import Perspectives.Persistent (entitiesDatabaseName, postDatabaseName)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.RunPerspectives (runPerspectives, runPerspectivesWithState)
import Perspectives.SetupCouchdb (partyMode, setupCouchdbForFirstUser)
import Perspectives.SetupUser (setupUser)
import Perspectives.Sync.Channel (endChannelReplication)
import Perspectives.Sync.IncomingPost (incomingPost)
import Perspectives.User (getSystemIdentifier)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (<>), (<$>), (>=>), (>>=), (<<<))

-- | Runs the PDR with default credentials. Used for testing clients without authentication.
main :: Effect Unit
main = runPDR "cor" "geheim" "cor"
-- main = parseFromCommandLine

-- NOTE: For release v0.1.0, I've commented out the original main function because it requires the perspectivesproxy
-- While this is available, it is webpacked for usage in the browser and will not run from the command line
-- (The PDR is not meant for command line usage).

-- | Execute the Perspectives Distributed Runtime by creating a listener to the internal channel.
  -- TODO. Bewaar de fiber in state en kill om uit te loggen?
runPDR :: String -> String -> String -> Effect Unit
runPDR usr pwd ident = void $ runAff handleError do
  url <- pure "http://127.0.0.1:5984/"
  (av :: AVar String) <- new "This value will be removed on first authentication!"
  state <- new $ newPerspectivesState (CouchdbUser
    { userName: UserName usr
    , couchdbPassword: pwd
    , couchdbHost: "http://127.0.0.1"
    , couchdbPort: 5984
    , systemIdentifier: ident
    , _rev: Nothing}) av
  runPerspectivesWithState (do
    void $ setupUser
    addAllExternalFunctions
    addIndexedNames)
    state
  void $ forkAff $ runPerspectivesWithState setupApi state
  void $ forkAff $ runPerspectivesWithState incomingPost state
  -- void $ forkAff $ runPerspectivesWithState setupTcpApi state

handleError :: forall a. (Either Error a -> Effect Unit)
handleError (Left e) = log $ "An error condition: " <> (show e)
handleError (Right a) = log $ "Perspectives-core has started!"

-- | The main entrance to the PDR for client programs. Runs the PDR on succesful login.
-- | When Couchdb is in Party Mode, initialises the first admin.
authenticate :: String -> String -> (Int -> Effect Unit) -> Effect Unit
authenticate usr pwd callback = void $ runAff handler do
  pm <- partyMode
  if pm
    then setupCouchdbForFirstUser usr pwd
    else pure unit
  (LA.authenticate usr pwd)
  where
    handler :: Either Error AuthenticationResult -> Effect Unit
    handler (Left e) = log $ "An error condition: " <> (show e)
    handler (Right r) = case r of
      UnknownUser -> callback 0
      WrongPassword -> callback 1
      OK (CouchdbUser{systemIdentifier})-> do
        runPDR usr pwd systemIdentifier
        callback 2

-- | This is for development only! Assumes the user identifier equals the user name.
resetAccount :: String -> String -> Effect Unit
resetAccount usr pwd = void $ runAff handler (runPerspectives usr pwd usr do
  -- Get all Channels
  getChannels <- getRoleFunction "sys:PerspectivesSystem$Channels"
  system <- getMySystem
  (channels :: Array ContextInstance) <- (ContextInstance system) ##= getChannels >=> context
  -- End their replication
  for_ channels endChannelReplication
  -- remove the databases
  getChannelDbId <- getPropertyFunction "sys:Channel$External$ChannelDatabaseName"
  for_ channels \c -> (c ##>> externalRole >=> getChannelDbId) >>= deleteDatabase <<< unwrap
  clearUserDatabase
  clearModelDatabase
  clearPostDatabase)
  where

    clearUserDatabase :: MonadPerspectives Unit
    clearUserDatabase = do
      userDatabaseName <- entitiesDatabaseName
      deleteDatabase userDatabaseName
      createDatabase userDatabaseName
    clearModelDatabase :: MonadPerspectives Unit
    clearModelDatabase = do
      dbname <- modelsDatabaseName
      deleteDatabase dbname
      createDatabase dbname
    clearPostDatabase :: MonadPerspectives Unit
    clearPostDatabase = do
      dbname <- postDatabaseName
      deleteDatabase dbname
      createDatabase dbname
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = log $ "An error condition in resetAccount: " <> (show e)
    handler (Right e) = log $ "Cleared the account " <> usr


-- | Retrieve all instances of sys:Model$IndexedRole and sys:Model$IndexedContext and create a table of
-- | all known indexed names and their private replacements in PerspectivesState.
addIndexedNames :: MonadPerspectives Unit
addIndexedNames = do
  (roleInstances :: Array RoleInstance) <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb ["model:System$Model$IndexedRole"] (ContextInstance "")))
  iRoles <- indexedRoles_ roleInstances
  contextInstances <- fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb ["model:System$Model$IndexedContext"] (ContextInstance "")))
  iContexts <- indexedContexts_ contextInstances
  modify \ps -> ps {indexedRoles = iRoles, indexedContexts = iContexts}
