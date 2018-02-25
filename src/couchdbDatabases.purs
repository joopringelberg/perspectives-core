module Perspectives.Couchdb.Databases where

import Control.Monad.Aff (message)
import Control.Monad.Aff.AVar (AVAR, isEmptyVar, readVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError, catchJust)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (fromObject, fromString)
import Data.Array (cons, find)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AffjaxRequest)
import Network.HTTP.Affjax (AffjaxResponse, put, post, affjax) as AJ
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeaderName, responseHeaderValue)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.Couchdb (CouchdbStatusCodes, DatabaseName, Password, PostCouchdb_session, User, onAccepted, onAccepted')
import Perspectives.Effects (AjaxAvarCache, AjaxAvar)
import Perspectives.PerspectivesState (MonadPerspectives, sessionCookie, takeSessionCookieValue, readSessionCookieValue, setSessionCookie)
import Perspectives.User (getCouchdbBaseURL, getUser, getCouchdbPassword)
import Prelude (Unit, bind, const, ifM, pure, unit, void, ($), (*>), (<<<), (<>), (==), (>>=))

ensureAuthentication :: forall e a. MonadPerspectives (AjaxAvarCache e) a -> MonadPerspectives (AjaxAvarCache e) a
ensureAuthentication a =
  ifM (sessionCookie >>= lift <<< isEmptyVar)
    (authenticate *> a)
    (catchJust predicate a (const (authenticate *> a)))
  where
    predicate :: Error -> Maybe Unit
    predicate e = if message e == "UNAUTHORIZED" then Just unit else Nothing

-----------------------------------------------------------
-- QUALIFIEDREQUEST
-----------------------------------------------------------
-- | On the browser, do nothing; otherwise add a Cookie header containing the cached cookie.
qualifyRequest :: forall e a. AffjaxRequest a -> MonadPerspectives (avar :: AVAR | e) (AffjaxRequest a)
qualifyRequest req@{headers} = do
  cookie <- readSessionCookieValue
  case cookie of
    "Browser" -> pure req
    otherwise -> pure $ req {headers = cons (RequestHeader "Cookie" cookie) headers}

defaultRequest :: forall e. MonadPerspectives (avar :: AVAR | e) (AffjaxRequest Unit)
defaultRequest = qualifyRequest
  { method: Left GET
  , url: "http://localhost:5984/"
  , headers: []
  , content: Nothing
  -- TODO. Zonder de credentials weer mee te sturen, ben je niet geauthenticeerd.
  , username: Nothing
  , password: Nothing
  , withCredentials: true
}

-----------------------------------------------------------
-- CREATEFIRSTADMIN
-- Notice: no authentication. Requires Couchdb to be in party mode.
-----------------------------------------------------------
createFirstAdmin :: forall e. User -> Password -> MonadPerspectives (AjaxAvarCache e) Unit
createFirstAdmin user password = do
  base <- getCouchdbBaseURL
  -- TODO. Het _config endpoint bestaat niet meer in Couchdb ^2.0.
  -- In plaats van:
  --    _config/
  --    _node/couchdb@localhost/_config/
  -- Zie ook: http://docs.couchdb.org/en/2.1.1/config/auth.html
  (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put (base <> "_node/couchdb@localhost/_config/admins/" <> user) password
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
  (rq :: (AffjaxRequest Unit)) <- defaultRequest
  (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.affjax $ rq {method = Left PUT, url = (base <> dbname)}
  -- (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put' (base <> dbname) (Nothing :: Maybe String)
  liftAff $ onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit

deleteDatabase :: forall e. DatabaseName -> MonadPerspectives (AjaxAvarCache e) Unit
deleteDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  (rq :: (AffjaxRequest Unit)) <- defaultRequest
  (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.affjax $ rq {method = Left DELETE, url = (base <> dbname)}
  -- liftAff $ AJ.put' (base <> dbname) (Nothing :: Maybe String)
  liftAff $ onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit

-----------------------------------------------------------
-- AUTHENTICATION
-- See: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie
-----------------------------------------------------------
authenticate :: forall e. MonadPerspectives (AjaxAvar e) Unit
authenticate =
  ifM (sessionCookie >>= lift <<< isEmptyVar)
    -- An authentication request is under way. Just wait till the AVar contains a value.
    (sessionCookie >>= (void <<< lift <<< readVar))
    -- New authentication is necessary.
    requestAuthentication

requestAuthentication :: forall e. MonadPerspectives (AjaxAvar e) Unit
requestAuthentication = do
  _ <- takeSessionCookieValue
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
  saveCookie headers = case find (\rh -> (responseHeaderName rh) == "Set-Cookie") headers of
    Nothing -> do
      setSessionCookie "Browser"
    (Just h) -> do
      -- NOTE. The Node implementation of Affjax depends on https://www.npmjs.com/package/xhr2. However, this emulation does not implement cookie authentication. Hence, we cannot use Perspectives from the command line.
      setSessionCookie $ responseHeaderValue h
