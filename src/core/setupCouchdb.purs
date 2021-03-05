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

module Perspectives.SetupCouchdb where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Perspectives.Couchdb (SecurityDocument(..), User)
import Perspectives.Couchdb.Databases (ensureAuthentication, setSecurityDocument)
import Perspectives.Persistence.API (MonadPouchdb, Password, Url, UserName, addViewToDatabase, createDatabase, databaseInfo, getSystemIdentifier, runMonadPouchdb)
import Perspectives.Persistent (entitiesDatabaseName)
import Perspectives.PerspectivesState (backendIsCouchdb)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<>), (>>=), (==))

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
    {doc_count} <- databaseInfo "localusers"
    -- isFirstUser <- databaseExists_ "localusers"
    if doc_count == 0
      then (ensureAuthentication do
        createSystemDatabases
        -- For now, we initialise the repository, too.
        initRepository
        createDatabase "localusers"
        backendIsCouchdb >>= if _
          then void $ setSecurityDocument "localusers" (SecurityDocument {_id: "_security", admins: {names: [], roles: []}, members: {names: [], roles: ["NotExistingRole"]}})
          else pure unit)
      else pure unit

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
  backendIsCouchdb >>= if _
    then void $ setSecurityDocument "repository"
      (SecurityDocument {_id: "_security", admins: {names: [], roles: ["_admin"]}, members: {names: [], roles: []}})
    else pure unit

-----------------------------------------------------------
-- CREATESYSTEMDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createSystemDatabases :: forall f. MonadPouchdb f Unit
createSystemDatabases = do
  -- If these databases exist, nothing new is created.
  createDatabase "_users"
  createDatabase "_replicator"
  createDatabase "_global_changes"

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
  backendIsCouchdb >>= if _
    then void $ setSecurityDocument (user <> "_models/")
      (SecurityDocument {_id: "_security", admins: {names: [], roles: ["_admin"]}, members: {names: [], roles: []}})
    else pure unit

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
