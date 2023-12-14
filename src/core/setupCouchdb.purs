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

import Data.Array (elemIndex)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff)
import Perspectives.ContextAndRole (context_allTypes, context_id, context_pspType, rol_allTypes, rol_binding, rol_context)
import Perspectives.Couchdb (SecurityDocument(..), User)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Persistence.API (MonadPouchdb, Password, Url, UserName, addViewToDatabase, createDatabase, databaseInfo, runMonadPouchdb)
import Perspectives.Persistence.CouchdbFunctions (setSecurityDocument)
import Perspectives.Persistence.State (getSystemIdentifier, withCouchdbUrl)
import Perspectives.Persistent (entitiesDatabaseName)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<>), (>>=), (==), (&&))

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
      then do
        createSystemDatabases
        -- For now, we initialise the repository, too.
        initRepository
        createDatabase "localusers"
        void $ withCouchdbUrl
          \url -> setSecurityDocument url "localusers"
            (SecurityDocument {admins: {names: Just [], roles: []}, members: {names: Just [], roles: ["NotExistingRole"]}})
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
  void $ withCouchdbUrl \url -> setSecurityDocument url "repository"
      (SecurityDocument {admins: {names: Just [], roles: ["_admin"]}, members: {names: Just [], roles: []}})

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
  -- Force Pouchdb to create the database: we need to do this in case the backend is Couchdb.
  void $ databaseInfo $ user <> "_entities"
  createDatabase $ user <> "_post"
  void $ databaseInfo $ user <> "_post"
  createDatabase $ user <> "_models"
  void $ databaseInfo $ user <> "_models"
  -- Now set the security document such that there is no role restriction for members.
  void $ withCouchdbUrl \url -> setSecurityDocument url (user <> "_models")
      (SecurityDocument {admins: {names: Just [], roles: ["_admin"]}, members: {names: Just [], roles: []}})

-----------------------------------------------------------
-- VIEWS AND FILTER FUNCTIONS
-- As we do not immediately store new instances, but only at certain intervals, it may happen an instance has been created
-- and is in cache, but not yet in the database. In such a situation, a view that might fetch the instance from the database
-- will not return it.
-- To compensate, we provide filter functions that can be applied to the caches, giving the same results as the views - but obviously
-- only on cached resources.
-- We provide filter functions for all views, excecpt for:
--    * credentialsView
--    * roleSpecialisationsView (a view on types rather than instances)
--    * contextSpecialisationsView (idem)
-- 
-- The function names equal the view names, with "Filter" postfixed.
-----------------------------------------------------------

-----------------------------------------------------------
-- THE VIEW 'ROLE'
-- This view collects instances of a particular role type.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setRoleView :: forall f. String -> MonadPouchdb f Unit
setRoleView dbname = void $ addViewToDatabase dbname "defaultViews" "roleView" ({map: roleView, reduce: Nothing})

-- | Import the view definition as a String.
foreign import roleView :: String

roleViewFilter :: EnumeratedRoleType -> PerspectRol -> Boolean
roleViewFilter rtype rinstance = isJust $ elemIndex rtype (rol_allTypes rinstance)

-- THE VIEW 'ROLEFROMCONTEXT'
-- This view collects instances of a particular role type, **from a particular context instance**.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setRoleFromContextView :: forall f. String -> MonadPouchdb f Unit
setRoleFromContextView dbname = void $ addViewToDatabase dbname "defaultViews" "roleFromContext" ({map: roleFromContextView, reduce: Nothing})

-- | Import the view definition as a String.
foreign import roleFromContextView :: String

roleFromContextFilter :: EnumeratedRoleType -> ContextInstance -> PerspectRol -> Boolean
roleFromContextFilter eRoleType cinstance role = rol_context role == cinstance && isJust (elemIndex eRoleType (rol_allTypes role))

-- OBSOLETE. Remove if testing shows the current definitioin of pendingInvitations works.
-----------------------------------------------------------
-- THE VIEW 'PENDINGINVITATIONS'
-- This view collects instances model:System$Invitation$External.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setPendingInvitationView :: forall f. String -> MonadPouchdb f Unit
setPendingInvitationView dbname = void $ addViewToDatabase dbname "defaultViews" "pendingInvitations" ({map: pendingInvitations, reduce: Nothing})

-- | Import the view definition as a String.
foreign import pendingInvitations :: String

-----------------------------------------------------------
-- THE VIEW 'CONTEXTVIEW'
-- This view collects instances of a particular context type.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setContextView :: forall f. String -> MonadPouchdb f Unit
setContextView dbname = void $ addViewToDatabase dbname "defaultViews" "contextView" ({map: contextView, reduce: Nothing})

-- | Import the view definition as a String.
foreign import contextView :: String

contextViewFilter :: ContextType -> PerspectContext -> Boolean
contextViewFilter ctype cinstance = isJust $ elemIndex ctype (context_allTypes cinstance)

-----------------------------------------------------------
-- THE VIEW 'ROLESPECIALISATIONVIEW'
-- This view collects instances of a particular role type.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setRoleSpecialisationsView :: forall f. String -> MonadPouchdb f Unit
setRoleSpecialisationsView dbname = void $ addViewToDatabase dbname "defaultViews" "roleSpecialisationsView" ({map: roleSpecialisations, reduce: Nothing})

-- | Import the view definition as a String.
foreign import roleSpecialisations :: String

-----------------------------------------------------------
-- THE VIEW 'CONTEXTSPECIALISATIONVIEW'
-- This view collects instances of a particular context type.
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
setContextSpecialisationsView :: forall f. String -> MonadPouchdb f Unit
setContextSpecialisationsView dbname = void $ addViewToDatabase dbname "defaultViews" "contextSpecialisationsView" ({map: contextSpecialisations, reduce: Nothing})

-- | Import the view definition as a String.
foreign import contextSpecialisations :: String

-----------------------------------------------------------
-- THE VIEW 'CREDENTIALSVIEW'
-- This view collects instances of roles of type "model://perspectives.domains#System$WithCredentials".
-- However, only instances whose `isMe` value is true, are returned.
-----------------------------------------------------------
setCredentialsView :: forall f. String -> MonadPouchdb f Unit
setCredentialsView dbname = void $ addViewToDatabase dbname "defaultViews" "credentialsView" ({map: credentials, reduce: Nothing})

-- | Import the view definition as a String.
foreign import credentials :: String

-----------------------------------------------------------
-- THE VIEW 'FILLEDROLESVIEW'
-- This view collects instances of roles that are filled by the given role.
-----------------------------------------------------------
setFilledRolesView :: forall f. String -> MonadPouchdb f Unit
setFilledRolesView dbname = void $ addViewToDatabase dbname "defaultViews" "filledRolesView" ({map: filledRoles, reduce: Nothing})

-- | Import the view definition as a String.
foreign import filledRoles :: String

filledRolesFilter :: RoleInstance -> PerspectRol -> Boolean
filledRolesFilter rid role = Just rid == rol_binding role 