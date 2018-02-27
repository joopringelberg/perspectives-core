module Perspectives.SetupCouchdb where

-----------------------------------------------------------
-- CREATEFIRSTADMIN
-- Notice: no authentication. Requires Couchdb to be in party mode.
-----------------------------------------------------------
import Control.Monad.Aff.Class (liftAff)
import Data.Argonaut (fromString)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AffjaxRequest, AffjaxResponse, put, affjax)
import Perspectives.Couchdb (Password, User, onAccepted)
import Perspectives.Couchdb.Databases (createDatabase, defaultRequest)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.User (getCouchdbBaseURL, getCouchdbPassword, getUser)
import Prelude (Unit, bind, pure, unit, ($), (<>), discard)

-----------------------------------------------------------
-- SETUPCOUCHDB
-- Notice: no authentication. Requires Couchdb to be in party mode.
-----------------------------------------------------------
setupCouchdb :: forall e. MonadPerspectives (AjaxAvarCache e) Unit
setupCouchdb = do
  usr <- getUser
  pwd <- getCouchdbPassword
  -- createFirstAdmin usr pwd
  createSystemDatabases
  createUserDatabases usr
  createDatabase "perspectives_models"
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
      (rq :: (AffjaxRequest Unit)) <- defaultRequest
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
