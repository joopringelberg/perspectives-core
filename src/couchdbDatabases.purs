module Perspectives.Couchdb.Databases where

import Control.Monad.Aff (message)
import Control.Monad.Aff.AVar (isEmptyVar, readVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError, catchJust)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (fromObject, fromString)
import Data.Array (find)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AffjaxResponse, put, post) as AJ
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeaderName, responseHeaderValue)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.Couchdb (CouchdbStatusCodes, DatabaseName, Password, PostCouchdb_session, User, onAccepted, onAccepted')
import Perspectives.Effects (AjaxAvarCache, AjaxAvar)
import Perspectives.PerspectivesState (MonadPerspectives, couchdbSessionStarted, sessionCookie, sessionCookieValue, setSessionCookie)
import Perspectives.User (getCouchdbBaseURL, getUser, getCouchdbPassword)
import Prelude (Unit, bind, const, ifM, pure, unit, void, ($), (*>), (<<<), (<>), (==), (>>=))

-- TODO: als we een cookie hebben, stuur het mee! En bovendien, vang het bij elke call weer op en sla het op.
ensureAuthentication :: forall e a. MonadPerspectives (AjaxAvarCache e) a -> MonadPerspectives (AjaxAvarCache e) a
ensureAuthentication a =
  ifM couchdbSessionStarted
    (catchJust predicate a (const (authenticate *> a)))
    (authenticate *> a)
  where
    predicate :: Error -> Maybe Unit
    predicate e = if message e == "UNAUTHORIZED" then Just unit else Nothing

-----------------------------------------------------------
-- CREATEFIRSTADMIN
-- Notice: no authentication. Requires Couchdb to be in party mode.
-----------------------------------------------------------
createFirstAdmin :: forall e. User -> Password -> MonadPerspectives (AjaxAvarCache e) Unit
createFirstAdmin user password = do
  base <- getCouchdbBaseURL
  -- TODO. Het _config endpoint bestaat niet meer in Couchdb ^2.0. In plaats daarvan: /_node//_config/.
  -- Zie ook: http://docs.couchdb.org/en/2.1.1/config/auth.html
  (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put (base <> "_config/admins/" <> user) password
  onAccepted res.status [200] "createFirstAdmin"
    $ pure unit
-----------------------------------------------------------
-- CREATE, DELETE, DATABASE
-----------------------------------------------------------
createStatusCodes :: CouchdbStatusCodes
createStatusCodes = fromFoldable
  [ Tuple 400 "Bad Request. Invalid database name."
  , Tuple 401 "Unauthorized. CouchDB Server Administrator privileges required."
  , Tuple 412 "Precondition failed. Database already exists."]

createDatabase :: forall e. DatabaseName -> MonadPerspectives (AjaxAvarCache e) Unit
createDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put (base <> dbname) unit
  liftAff $ onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit

deleteDatabase :: forall e. DatabaseName -> MonadPerspectives (AjaxAvarCache e) Unit
deleteDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put (base <> dbname) unit
  liftAff $ onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit

-----------------------------------------------------------
-- AUTHENTICATION
-- See: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie
-----------------------------------------------------------
-- TODO: Catch and save the cookie for the server version.
authenticate :: forall e. MonadPerspectives (AjaxAvar e) Unit
authenticate =
  ifM (sessionCookie >>= lift <<< isEmptyVar)
    -- An authentication request is under way. Just wait till the AVar contains a value.
    (sessionCookie >>= (void <<< lift <<< readVar))
    -- New authentication is necessary.
    requestAuthentication

requestAuthentication :: forall e. MonadPerspectives (AjaxAvar e) Unit
requestAuthentication = do
  _ <- sessionCookieValue
  base <- getCouchdbBaseURL
  usr <- getUser
  pwd <- getCouchdbPassword
  (res :: AJ.AffjaxResponse PostCouchdb_session) <- lift $ AJ.post
    (base <> "_session")
    (fromObject (StrMap.fromFoldable [Tuple "name" (fromString usr), Tuple "password" (fromString pwd)]))
  case res.status of
    (StatusCode 200) -> saveCookie res.headers
    (StatusCode 203) -> saveCookie res.headers
    otherwise -> throwError $ error "Failure in authenticate. Unauthorized. Username or password wasn’t recognized"
  where
  -- In the browser, the cookie header is hidden from our code: the browser handles it by itself.
  saveCookie :: Array ResponseHeader -> MonadPerspectives (AjaxAvar e) Unit
  saveCookie headers = case find (\rh -> responseHeaderName rh == "set-cookie") headers of
    Nothing -> setSessionCookie "Browser"
    (Just h) -> setSessionCookie $ responseHeaderValue h
