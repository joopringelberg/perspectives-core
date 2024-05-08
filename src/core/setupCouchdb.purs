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
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Foreign.Object (foldMap)
import Perspectives.ContextAndRole (context_allTypes, context_iedereRolInContext, rol_allTypes, rol_binding, rol_context, rol_gevuldeRollen)
import Perspectives.Couchdb (SecurityDocument(..), User)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Persistence.API (MonadPouchdb, addViewToDatabase, createDatabase, databaseInfo)
import Perspectives.Persistence.CouchdbFunctions (setSecurityDocument)
import Perspectives.Persistence.State (withCouchdbUrl)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType)
import Prelude (Unit, discard, void, ($), (&&), (<>), (==))

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
  createDatabase $ user <> "_invertedqueries"
  void $ databaseInfo $ user <> "_invertedqueries"
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
-- THE VIEW 'FILLER2FILLEDVIEW'
-- This view is a table [filler;filled]
-- Use it by selecting on filler to obtain those roles that are filled by it.
-----------------------------------------------------------
setFiller2FilledView :: forall f. String -> MonadPouchdb f Unit
setFiller2FilledView dbname = void $ addViewToDatabase dbname "defaultViews" "filler2filledView" ({map: filler2filledView, reduce: Nothing})

-- | Import the view definition as a String.
foreign import filler2filledView :: String

-- | Does the RoleInstance instance fill the PerspectRol?
filler2filledFilter :: RoleInstance -> PerspectRol -> Boolean
filler2filledFilter filler role = Just filler == rol_binding role 

-----------------------------------------------------------
-- THE VIEW 'FILLERROLEVIEW'
-- This view is a table [filled; {filler, filledContextType, filledRoleType}].
-- Use it by selecting on filled to obtain the role that fills it.
-----------------------------------------------------------
setFilled2FillerView :: forall f. String -> MonadPouchdb f Unit
setFilled2FillerView dbname = void $ addViewToDatabase dbname "defaultViews" "filled2fillerView"
  ({map: filled2fillerView, reduce: Nothing})

foreign import filled2fillerView :: String

-- | Is the RoleInstance filled by the PerspectRol?
filled2fillerFilter :: RoleInstance -> PerspectRol -> Boolean
filled2fillerFilter rid role = unwrap (foldMap
    (\ctype filleds -> foldMap 
      (\rtype filleds' -> Disj $ isJust $ elemIndex rid filleds')
      filleds)
    (rol_gevuldeRollen role))

-----------------------------------------------------------
-- THE VIEW 'CONTEXT2ROLEVIEW'
-- This view is a table [context;role]
-- Use it by selecting on context to obtain its roles.
-----------------------------------------------------------
setContext2RoleView :: forall f. String -> MonadPouchdb f Unit
setContext2RoleView dbname = void $ addViewToDatabase dbname "defaultViews" "context2RoleView"
  ({map: context2roleView, reduce: Nothing})

foreign import context2roleView :: String

context2RoleFilter :: RoleInstance -> PerspectContext -> Boolean
context2RoleFilter rid context = unwrap (foldMap
  (\rtype roles -> Disj $ isJust $ elemIndex rid roles)
  (context_iedereRolInContext context))

-----------------------------------------------------------
-- THE VIEW 'ROLE2CONTEXTVIEW'
-- This view is a table [role; context]
-- Use it by selecting on role to obtain its context.
-----------------------------------------------------------
setRole2ContextView :: forall f. String -> MonadPouchdb f Unit
setRole2ContextView dbname = void $ addViewToDatabase dbname "defaultViews" "role2ContextView"
  ({map: role2contextView, reduce: Nothing})

foreign import role2contextView :: String

role2ContextFilter :: ContextInstance -> PerspectRol -> Boolean
role2ContextFilter cid prol = cid == rol_context prol

-----------------------------------------------------------
-- THE VIEW 'modelView'
-----------------------------------------------------------
setModelView :: forall f. String -> MonadPouchdb f Unit
setModelView dbname = void $ addViewToDatabase dbname "defaultViews" "modelView"
  ({map: modelView, reduce: Nothing})

foreign import modelView :: String
-----------------------------------------------------------
-- THE VIEW 'RTPropertyKeyView'
-----------------------------------------------------------
setRTPropertyKeyView :: forall f. String -> MonadPouchdb f Unit
setRTPropertyKeyView dbname = void $ addViewToDatabase dbname "defaultViews" "RTPropertyKeyView"
  ({map: rTPropertyKeyView, reduce: Nothing})

foreign import rTPropertyKeyView :: String

-----------------------------------------------------------
-- THE VIEW 'RTRoleKeyView'
-----------------------------------------------------------
setRTRoleKeyView :: forall f. String -> MonadPouchdb f Unit
setRTRoleKeyView dbname = void $ addViewToDatabase dbname "defaultViews" "RTRoleKeyView"
  ({map: rTRoleKeyView, reduce: Nothing})

foreign import rTRoleKeyView :: String

-----------------------------------------------------------
-- THE VIEW 'RTContextKey'
-----------------------------------------------------------
setRTContextKeyView :: forall f. String -> MonadPouchdb f Unit
setRTContextKeyView dbname = void $ addViewToDatabase dbname "defaultViews" "RTContextKeyView"
  ({map: rTContextKeyView, reduce: Nothing})

foreign import rTContextKeyView :: String

-----------------------------------------------------------
-- THE VIEW 'RTFillerKey'
-----------------------------------------------------------
setRTFillerKeyView :: forall f. String -> MonadPouchdb f Unit
setRTFillerKeyView dbname = void $ addViewToDatabase dbname "defaultViews" "RTFillerKeyView"
  ({map: rTFillerKeyView, reduce: Nothing})

foreign import rTFillerKeyView :: String

-----------------------------------------------------------
-- THE VIEW 'RTFilledKey'
-----------------------------------------------------------
setRTFilledKeyView :: forall f. String -> MonadPouchdb f Unit
setRTFilledKeyView dbname = void $ addViewToDatabase dbname "defaultViews" "RTFilledKeyView"
  ({map: rTFilledKeyView, reduce: Nothing})

foreign import rTFilledKeyView :: String