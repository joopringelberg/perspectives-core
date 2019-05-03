module Perspectives.SetupCouchdb where

import Effect.Aff.Class (liftAff)
import Data.Argonaut (fromString)
import Data.Array (null)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Affjax (request, put, Request)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (Password, User, onAccepted)
import Perspectives.Couchdb.Databases (createDatabase, defaultPerspectRequest, ensureAuthentication, allDbs)

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
