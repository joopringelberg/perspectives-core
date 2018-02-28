module Perspectives.SetupCouchdb where

import Control.Monad.Aff.Class (liftAff)
import Data.Argonaut (fromString)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AffjaxRequest, AffjaxResponse, put, affjax)
import Perspectives.Couchdb (Password, User, onAccepted)
import Perspectives.Couchdb.Databases (createDatabase, defaultPerspectRequest, ensureAuthentication, logout, requestAuthentication')
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PerspectivesState (MonadPerspectives, takeSessionCookieValue)
import Perspectives.User (getCouchdbBaseURL, getCouchdbPassword, getUser)
import Prelude (Unit, bind, discard, pure, unit, ($), (<>))

-----------------------------------------------------------
-- SETUPCOUCHDB
-- Notice: Requires Couchdb to have a server admin user "admin" whose password is "admin"
-- and the "_users" database available.
-----------------------------------------------------------
setupCouchdb :: forall e. MonadPerspectives (AjaxAvarCache e) Unit
setupCouchdb = do
  _ <- takeSessionCookieValue
  requestAuthentication' "admin" "admin"
  usr <- getUser
  pwd <- getCouchdbPassword
  createAnotherAdmin usr pwd
  createSystemDatabases
  createUserDatabases usr
  createDatabase "perspectives_models"
  logout
  -- configureCORS

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
-- CONFIGURECORS
-- Notice: no authentication. Requires Couchdb to be in party mode.
-----------------------------------------------------------
{-
This should be the cors configuration:
[httpd]
enable_cors = true

[cors]
origins = http://www.pureperspectives.nl
credentials = true
headers = accept, authorization, content-type, origin, referer, Server, X-Couch-Id, X-Couch-Update-Newrev
methods = GET, PUT, POST, HEAD, DELETE, OPTIONS

See http://docs.couchdb.org/en/2.1.1/api/server/configuration.html#api-config
PUT /_node/{node-name}/_config/{section}/{key}
-}
configureCORS :: forall e. MonadPerspectives (AjaxAvarCache e) Unit
configureCORS =
  do
    setKey "httpd/enable_cors" "true"
    setKey "cors/headers" "accept, authorization, content-type, origin, referer, Server, X-Couch-Id, X-Couch-Update-Newrev"
    setKey "cors/origins" "http://www.pureperspectives.nl"
    setKey "cors/credentials" "true"
    setKey "cors/methods" "GET, PUT, POST, HEAD, DELETE, OPTIONS"
  where
    setKey key value = do
      base <- getCouchdbBaseURL
      (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
      (res :: AffjaxResponse String) <- liftAff $ affjax $ rq {method = Left PUT, url = (base <> "_node/couchdb@localhost/_config/" <> key), content = Just $ fromString value}
      -- (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put (base <> "_node/couchdb@localhost/_config/cors/" <> key) value
      onAccepted res.status [200] "configureCORS" $ pure unit


-----------------------------------------------------------
-- CREATESYSTEMDATABASES
-- Notice: authentication required!
-----------------------------------------------------------
createSystemDatabases :: forall e. MonadPerspectives (AjaxAvarCache e) Unit
createSystemDatabases = do
  -- createDatabase "_users"
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
