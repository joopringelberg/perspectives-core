module Perspectives.SetupCouchdb where

import Control.Monad.Aff.Class (liftAff)
import Data.Argonaut (fromString)
import Data.Array (null)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AffjaxRequest, AffjaxResponse, affjax, put)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (Password, User, onAccepted)
import Perspectives.Couchdb.Databases (createDatabase, defaultPerspectRequest, ensureAuthentication, allDbs)
import Perspectives.Effects (AjaxAvarCache, AjaxAvar)
import Perspectives.User (getCouchdbBaseURL, getCouchdbPassword, getUser)
import Prelude (Unit, bind, discard, pure, unit, ($), (<<<), (<>), (>>=))

-----------------------------------------------------------
-- SETUPCOUCHDB
-- Notice: Requires Couchdb to have a server admin user "admin" whose password is "admin"
-- and the "_users" database available.
-----------------------------------------------------------
setupCouchdb :: forall e. MonadPerspectives (AjaxAvarCache e) Unit
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
createFirstAdmin :: forall e. User -> Password -> MonadPerspectives (AjaxAvarCache e) Unit
createFirstAdmin user password = do
  base <- getCouchdbBaseURL
  -- See http://docs.couchdb.org/en/2.1.1/api/server/configuration.html#api-config
  (res :: AffjaxResponse String) <- liftAff $ put (base <> "_node/couchdb@localhost/_config/admins/" <> user) (fromString password)
  onAccepted res.status [200] "createFirstAdmin"
    $ pure unit

-----------------------------------------------------------
-- CREATEANOTHERADMIN
-- Notice: requires authentication!
-----------------------------------------------------------
createAnotherAdmin :: forall e. User -> Password -> MonadPerspectives (AjaxAvarCache e) Unit
createAnotherAdmin user password = ensureAuthentication do
  base <- getCouchdbBaseURL
  (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
  (res :: AffjaxResponse String) <- liftAff $ affjax $ rq {method = Left PUT, url = (base <> "_node/couchdb@localhost/_config/admins/" <> user), content = Just (fromString password)}
  liftAff $ onAccepted res.status [200] "createAnotherAdmin" $ pure unit

-----------------------------------------------------------
-- CREATESYSTEMDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createSystemDatabases :: forall e. MonadPerspectives (AjaxAvarCache e) Unit
createSystemDatabases = do
  createDatabase "_users"
  createDatabase "_replicator"
  createDatabase "_global_changes"

-----------------------------------------------------------
-- CREATEUSERDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createUserDatabases :: forall e. User -> MonadPerspectives (AjaxAvarCache e) Unit
createUserDatabases user = do
  createDatabase $ "user_" <> user <> "_entities"
  createDatabase $ "user_" <> user <> "_post"

-----------------------------------------------------------
-- PARTYMODE
-----------------------------------------------------------
-- | PartyMode operationalized as Couchdb having no databases.
partyMode :: forall e. MonadPerspectives (AjaxAvar e) Boolean
partyMode = allDbs >>= pure <<< null
