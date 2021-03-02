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

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Perspectives.Couchdb (SecurityDocument(..), User)
import Perspectives.Couchdb.Databases (databaseExists, ensureAuthentication, setSecurityDocument)
import Perspectives.Persistence.API (MonadPouchdb, Password, Url, UserName, addViewToDatabase, createDatabase, getSystemIdentifier, runMonadPouchdb)
import Perspectives.Persistent (entitiesDatabaseName)
import Perspectives.User (getCouchdbBaseURL)
import Prelude (Unit, bind, discard, not, pure, unit, void, ($), (<>), (>>=))

-----------------------------------------------------------
-- SETUPPERSPECTIVESINCOUCHDB
-- Notice: Requires authentication.
-----------------------------------------------------------
-- | If it has not been done before,
-- |  * create system databases
-- |  * initialize the local repository
-- |  * create the database `localusers` and set its security document.
setupPerspectivesInCouchdb :: UserName -> Password -> Maybe Url -> Aff Unit
setupPerspectivesInCouchdb usr pwd couchdbUrl = runMonadPouchdb usr pwd usr couchdbUrl
  do
    isFirstUser <- databaseExists_ "localusers"
    if not isFirstUser
      then (ensureAuthentication do
        createSystemDatabases
        -- For now, we initialise the repository, too.
        initRepository
        createDatabase "localusers"
        setSecurityDocument "localusers" (SecurityDocument {admins: {names: [], roles: []}, members: {names: [], roles: ["NotExistingRole"]}}))
      else pure unit

databaseExists_ :: forall f. String -> MonadPouchdb f Boolean
databaseExists_ dbname = do
  base <- getCouchdbBaseURL
  databaseExists (base <> dbname)

-----------------------------------------------------------
-- CREATEPERSPECTIVESUSER
-- Notice: Requires authentication.
-----------------------------------------------------------
createPerspectivesUser :: UserName -> Password -> Maybe Url -> Aff Unit
createPerspectivesUser usr pwd couchdbUrl =
  -- TODO: genereer hier de systeemIdentifier als een guid.
  runMonadPouchdb usr pwd usr couchdbUrl do
    getSystemIdentifier >>= createUserDatabases
    entitiesDatabaseName >>= setRoleView

-----------------------------------------------------------
-- INITREPOSITORY
-----------------------------------------------------------
-- | Create a database "repository" and add a view to it to retrive the external roles of the ModelDescription instances.
initRepository :: forall f. MonadPouchdb f Unit
initRepository = do
  createDatabase "repository"
  setModelDescriptionsView
  setSecurityDocument "repository"
    (SecurityDocument {admins: {names: [], roles: ["_admin"]}, members: {names: [], roles: []}})

-----------------------------------------------------------
-- CREATESYSTEMDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createSystemDatabases :: forall f. MonadPouchdb f Unit
createSystemDatabases = do
  databaseExists_ "_users" >>= \exists -> if not exists then createDatabase "_users" else pure unit
  databaseExists_ "_replicator" >>= \exists -> if not exists then createDatabase "_replicator" else pure unit
  databaseExists_ "_global_changes" >>= \exists -> if not exists then createDatabase "_global_changes" else pure unit

-----------------------------------------------------------
-- CREATEUSERDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createUserDatabases :: forall f. User -> MonadPouchdb f Unit
createUserDatabases user = do
  createDatabase $ user <> "_entities"
  createDatabase $ user <> "_post"
  createDatabase $ user <> "_models/"
  -- Now set the security document such that there is no role restriction for members.
  setSecurityDocument (user <> "_models/")
    (SecurityDocument {admins: {names: [], roles: ["_admin"]}, members: {names: [], roles: []}})

-----------------------------------------------------------
-- THE VIEW 'MODELDESCRIPTIONS'
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setModelDescriptionsView :: forall f. MonadPouchdb f Unit
setModelDescriptionsView = do
  void $ addViewToDatabase "repository" "defaultViews" "modeldescriptions" ({map: modelDescriptions, reduce: Nothing})

-- | Import the view definition as a String.
foreign import modelDescriptions :: String

-----------------------------------------------------------
-- THE VIEW 'ROLE'
-- This view collects instances of a particular role type.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setRoleView :: forall f. String -> MonadPouchdb f Unit
setRoleView dbname = do
  void $ addViewToDatabase dbname "defaultViews" "roleView" ({map: roleView, reduce: Nothing})

-- | Import the view definition as a String.
foreign import roleView :: String

-- THE VIEW 'ROLEFROMCONTEXT'
-- This view collects instances of a particular role type, **from a particular context instance**.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setRoleFromContextView :: forall f. String -> MonadPouchdb f Unit
setRoleFromContextView dbname = do
  void $ addViewToDatabase dbname "defaultViews" "roleFromContext" ({map: roleFromContextView, reduce: Nothing})

-- | Import the view definition as a String.
foreign import roleFromContextView :: String

-----------------------------------------------------------
-- THE VIEW 'PENDINGINVITATIONS'
-- This view collects instances model:System$Invitation$External.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setPendingInvitationView :: forall f. String -> MonadPouchdb f Unit
setPendingInvitationView dbname = do
  void $ addViewToDatabase dbname "defaultViews" "pendingInvitations" ({map: pendingInvitations, reduce: Nothing})

-- | Import the view definition as a String.
foreign import pendingInvitations :: String

-----------------------------------------------------------
-- THE VIEW 'CONTEXTVIEW'
-- This view collects instances of a particular context type.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setContextView :: forall f. String -> MonadPouchdb f Unit
setContextView dbname = do
  void $ addViewToDatabase dbname "defaultViews" "contextView" ({map: contextView, reduce: Nothing})

-- | Import the view definition as a String.
foreign import contextView :: String
