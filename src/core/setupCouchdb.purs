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
import Data.Argonaut (fromString)
import Data.Array (null)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (Password, User, View(..), onAccepted)
import Perspectives.Couchdb.Databases (addViewToDatabase, allDbs, createDatabase, defaultPerspectRequest, ensureAuthentication)
import Perspectives.User (getCouchdbBaseURL, getCouchdbPassword, getUser)
import Prelude (Unit, bind, discard, pure, unit, ($), (<<<), (<>), (>>=))

-----------------------------------------------------------
-- SETUPCOUCHDB
-- Notice: Requires Couchdb to have a server admin user "admin" whose password is "admin"
-- and the "_users" database available.
-----------------------------------------------------------
setupCouchdb :: MonadPerspectives Unit
setupCouchdb = do
  usr <- getUser
  pwd <- getCouchdbPassword
  createFirstAdmin usr pwd
  createSystemDatabases
  createUserDatabases usr
  createDatabase "perspect_models"
  -- For now, we initialise the database with a repository, too.
  createDatabase "repository"

-----------------------------------------------------------
-- CREATEFIRSTADMIN
-- Notice: no authentication. Requires Couchdb to be in party mode.
-----------------------------------------------------------
createFirstAdmin :: User -> Password -> MonadPerspectives Unit
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
createSystemDatabases :: MonadPerspectives Unit
createSystemDatabases = do
  createDatabase "_users"
  createDatabase "_replicator"
  createDatabase "_global_changes"

-----------------------------------------------------------
-- CREATEUSERDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createUserDatabases :: User -> MonadPerspectives Unit
createUserDatabases user = do
  createDatabase $ "user_" <> user <> "_entities"
  createDatabase $ "user_" <> user <> "_post"

-----------------------------------------------------------
-- PARTYMODE
-----------------------------------------------------------
-- | PartyMode operationalized as Couchdb having no databases.
partyMode :: MonadPerspectives Boolean
partyMode = allDbs >>= pure <<< null

-----------------------------------------------------------
-- THE VIEW 'MODELDESCRIPTIONS'
-----------------------------------------------------------
-- | Add a view to the couchdb installation in the 'repository' db.
-- | Notice that the .json file must be available runtime, in the "views" directory of cwd.
setModelDescriptionsView :: MonadPerspectives Unit
setModelDescriptionsView = do
  addViewToDatabase "repository" "defaultViews" "modeldescriptions" (View {map: modelDescriptions, reduce: Nothing})

foreign import modelDescriptions :: String
