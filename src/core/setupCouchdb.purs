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

module Perspectives.SetupCouchdb where

import Affjax (request, put, Request)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (fromString)
import Data.Array (null)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, try)
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (Password, SecurityDocument(..), User, View(..), onAccepted)
import Perspectives.Couchdb.Databases (addViewToDatabase, allDbs, createDatabase, databaseExists, defaultPerspectRequest, ensureAuthentication, setSecurityDocument)
import Perspectives.CouchdbState (MonadCouchdb, CouchdbUser(..), runMonadCouchdb)
import Perspectives.Persistent (entitiesDatabaseName, saveEntiteit_)
import Perspectives.RunPerspectives (runPerspectives)
import Perspectives.User (getCouchdbBaseURL, getHost, getPort, getSystemIdentifier)
import Prelude (Unit, bind, discard, not, pure, unit, void, when, ($), (<>), (>>=))

-----------------------------------------------------------
-- SETUPCOUCHDBFORFIRSTUSER
-- Notice: Requires Couchdb to be in partymode.
-----------------------------------------------------------
setupCouchdbForFirstUser :: String -> String -> String -> Int -> Aff Unit
setupCouchdbForFirstUser usr pwd host port = do
  -- TODO: genereer hier de systeemIdentifier als een guid.
  runMonadCouchdb usr pwd usr host port (createFirstAdmin usr pwd)
  setupPerspectivesInCouchdb usr pwd host port
  runPerspectives usr pwd usr host port do
    getSystemIdentifier >>= createUserDatabases
    void addUserToLocalUsers
    entitiesDatabaseName >>= setRoleView

-----------------------------------------------------------
-- SETUPPERSPECTIVESINCOUCHDB
-- Notice: Requires authentication.
-----------------------------------------------------------
-- | If it has not been done before,
-- |  * create system databases
-- |  * initialize the local repository
-- |  * create the database `localusers` and set its security document.
setupPerspectivesInCouchdb :: String -> String -> String -> Int -> Aff Unit
setupPerspectivesInCouchdb usr pwd host port = runMonadCouchdb usr pwd usr host port
  do
    isFirstUser <- databaseExists "localusers"
    when (not isFirstUser)
      (ensureAuthentication do
        createSystemDatabases
        -- For now, we initialise the repository, too.
        initRepository
        createDatabase "localusers"
        setSecurityDocument "localusers" (SecurityDocument {admins: {names: [], roles: []}, members: {names: [], roles: ["NotExistingRole"]}}))

-----------------------------------------------------------
-- SETUPCOUCHDBFORANOTHERUSER
-- Notice: Requires authentication.
-----------------------------------------------------------
setupCouchdbForAnotherUser :: String -> String -> MonadPerspectives Unit
setupCouchdbForAnotherUser usr pwd = do
  createAnotherAdmin usr pwd
  host <- getHost
  port <- getPort
  void $ lift $ createAnotherPerspectivesUser usr pwd host port

createAnotherPerspectivesUser :: String -> String -> String -> Int -> Aff CouchdbUser
createAnotherPerspectivesUser usr pwd host port =
  -- TODO: genereer hier de systeemIdentifier als een guid.
  runPerspectives usr pwd usr host port do
    getSystemIdentifier >>= createUserDatabases
    u <- addUserToLocalUsers
    entitiesDatabaseName >>= setRoleView
    pure u

-----------------------------------------------------------
-- INITREPOSITORY
-----------------------------------------------------------
-- | Create a database "repository" and add a view to it to retrive the external roles of the ModelDescription instances.
initRepository :: forall f. MonadCouchdb f Unit
initRepository = do
  createDatabase "repository"
  setModelDescriptionsView

-----------------------------------------------------------
-- ADDUSERTOLOCALUSERS
-----------------------------------------------------------
addUserToLocalUsers :: MonadPerspectives CouchdbUser
addUserToLocalUsers = (gets _.userInfo) >>= \u@(CouchdbUser{userName}) -> saveEntiteit_ userName u

-----------------------------------------------------------
-- CREATEFIRSTADMIN
-- Notice: no authentication. Requires Couchdb to be in party mode.
-- Assumes Couchdb to run on http://127.0.0.1:5984.
-----------------------------------------------------------
createFirstAdmin :: forall f. User -> Password -> MonadCouchdb f Unit
createFirstAdmin user password = do
  base <- getCouchdbBaseURL
  -- See http://docs.couchdb.org/en/2.1.1/api/server/configuration.html#api-config
  res <- liftAff $ put ResponseFormat.string (base <> "_node/couchdb@localhost/_config/admins/" <> user) (RequestBody.json (fromString password))
  onAccepted res.status [200] "createFirstAdmin"
    $ pure unit

-----------------------------------------------------------
-- CREATEANOTHERADMIN
-- Notice: requires authentication!
-----------------------------------------------------------
createAnotherAdmin :: User -> Password -> MonadPerspectives Unit
createAnotherAdmin user password = ensureAuthentication do
  base <- getCouchdbBaseURL
  (rq :: (Request String)) <- defaultPerspectRequest
  res <- liftAff $ request $ rq {method = Left PUT, url = (base <> "_node/couchdb@localhost/_config/admins/" <> user), content = Just $ RequestBody.json (fromString password)}
  liftAff $ onAccepted res.status [200] "createAnotherAdmin" $ pure unit

-----------------------------------------------------------
-- CREATESYSTEMDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createSystemDatabases :: forall f. MonadCouchdb f Unit
createSystemDatabases = do
  createDatabase "_users"
  createDatabase "_replicator"
  createDatabase "_global_changes"

-----------------------------------------------------------
-- CREATEUSERDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createUserDatabases :: forall f. User -> MonadCouchdb f Unit
createUserDatabases user = do
  createDatabase $ user <> "_entities"
  createDatabase $ user <> "_post"
  createDatabase $ user <> "_models/"

-----------------------------------------------------------
-- PARTYMODE
-----------------------------------------------------------
-- | PartyMode operationalized as Couchdb having no databases, or failing.
partyMode :: String -> Int -> Aff Boolean
partyMode host port = runMonadCouchdb "authenticator" "secret" "authenticator" host port
  do
    r <- try $ allDbs
    case r of
      Left _ -> pure false
      Right dbs -> pure $ null dbs

-----------------------------------------------------------
-- THE VIEW 'MODELDESCRIPTIONS'
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setModelDescriptionsView :: forall f. MonadCouchdb f Unit
setModelDescriptionsView = do
  addViewToDatabase "repository" "defaultViews" "modeldescriptions" (View {map: modelDescriptions, reduce: Nothing})

-- | Import the view definition as a String.
foreign import modelDescriptions :: String

-----------------------------------------------------------
-- THE VIEW 'ROLE'
-- This view collects instances of a particular role type.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setRoleView :: forall f. String -> MonadCouchdb f Unit
setRoleView dbname = do
  addViewToDatabase dbname "defaultViews" "roleView" (View {map: roleView, reduce: Nothing})

-- | Import the view definition as a String.
foreign import roleView :: String
