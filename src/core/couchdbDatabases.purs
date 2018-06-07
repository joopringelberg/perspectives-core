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
import Data.Map (fromFoldable, insert)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.StrMap (fromFoldable) as StrMap
import Data.String (Pattern(..), stripPrefix, stripSuffix)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AffjaxRequest)
import Network.HTTP.Affjax (AffjaxResponse, affjax, get) as AJ
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeaderName, responseHeaderValue)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (CouchdbStatusCodes, DatabaseName, PostCouchdb_session, User, Password, onAccepted', onAccepted, DBS)
import Perspectives.Effects (AjaxAvar, AjaxAvarCache, AvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.PerspectivesState (sessionCookie, setSessionCookie, takeSessionCookieValue, tryReadSessionCookieValue)
import Perspectives.User (getCouchdbBaseURL, getUser, getCouchdbPassword)
import Prelude (Unit, bind, const, ifM, pure, unit, void, ($), (*>), (/=), (<<<), (<>), (==), (>>=))

-----------------------------------------------------------
-- QUALIFYREQUEST
-----------------------------------------------------------
-- | On the browser, do nothing; otherwise add a Cookie header containing the cached cookie. This is a synchronous function.
qualifyRequest :: forall e a. AffjaxRequest a -> MonadPerspectives (avar :: AVAR | e) (AffjaxRequest a)
qualifyRequest req@{headers} = do
  cookie <- tryReadSessionCookieValue
  case cookie of
    (Just x) | x /= "Browser" -> pure req
    (Just ck) -> pure $ req {headers = cons (RequestHeader "Cookie" ck) headers}
    otherwise -> pure req

defaultPerspectRequest :: forall e. MonadPerspectives (avar :: AVAR | e) (AffjaxRequest Unit)
defaultPerspectRequest = qualifyRequest
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

-- | To be called when the cookie is no longer valid.
requestAuthentication :: forall e. MonadPerspectives (AjaxAvar e) Unit
requestAuthentication = do
  _ <- takeSessionCookieValue
  usr <- getUser
  pwd <- getCouchdbPassword
  requestAuthentication' usr pwd

-- | To be called if there is no cookie at all.
requestAuthentication' :: forall e. User -> Password -> MonadPerspectives (AjaxAvar e) Unit
requestAuthentication' usr pwd = do
  base <- getCouchdbBaseURL
  (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
  (res :: AJ.AffjaxResponse PostCouchdb_session) <- liftAff $ AJ.affjax $ rq {method = Left POST, url = (base <> "_session"), content = Just (fromObject (StrMap.fromFoldable [Tuple "name" (fromString usr), Tuple "password" (fromString pwd)]))}
  -- (res :: AJ.AffjaxResponse PostCouchdb_session) <- lift $ AJ.post
    -- (base <> "_session")
    -- (fromObject (StrMap.fromFoldable [Tuple "name" (fromString usr), Tuple "password" (fromString pwd)]))
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

ensureAuthentication :: forall e a. MonadPerspectives (AjaxAvarCache e) a -> MonadPerspectives (AjaxAvarCache e) a
ensureAuthentication a =
  ifM (sessionCookie >>= lift <<< isEmptyVar)
    (authenticate *> a)
    (catchJust predicate a (const (authenticate *> a)))
  where
    predicate :: Error -> Maybe Unit
    predicate e = if message e == "UNAUTHORIZED" then Just unit else Nothing

-- | A logout is purely client side, as Couchdb keeps no session state.
-- | (see: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie)
logout :: forall e. MonadPerspectives (AvarCache e) Unit
logout = void takeSessionCookieValue

-----------------------------------------------------------
-- CREATE, DELETE, DATABASE
-----------------------------------------------------------
databaseStatusCodes :: CouchdbStatusCodes
databaseStatusCodes = fromFoldable
  [ Tuple 400 "Bad Request. Invalid database name."
  , Tuple 401 "Unauthorized. CouchDB Server Administrator privileges required."]

createDatabase :: forall e. DatabaseName -> MonadPerspectives (AjaxAvarCache e) Unit
createDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
  (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.affjax $ rq {method = Left PUT, url = (base <> dbname)}
  -- (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put' (base <> dbname) (Nothing :: Maybe String)
  liftAff $ onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit
  where
    createStatusCodes = insert 412 "Precondition failed. Database already exists."
      databaseStatusCodes

deleteDatabase :: forall e. DatabaseName -> MonadPerspectives (AjaxAvarCache e) Unit
deleteDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
  (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.affjax $ rq {method = Left DELETE, url = (base <> dbname)}
  -- liftAff $ AJ.put' (base <> dbname) (Nothing :: Maybe String)
  liftAff $ onAccepted' deleteStatusCodes res.status [200] "deleteDatabase" $ pure unit
  where
    deleteStatusCodes = insert 412 "Precondition failed. Database does not exist."
      databaseStatusCodes

-----------------------------------------------------------
-- ALLDBS
-----------------------------------------------------------
allDbs :: forall e. MonadPerspectives (AjaxAvar e) (Array String)
allDbs = do
  base <- getCouchdbBaseURL
  (res :: AJ.AffjaxResponse DBS) <- lift $ AJ.get (base <> "_all_dbs")
  pure $ unwrap res.response

-----------------------------------------------------------
-- DOCUMENT VERSION
-----------------------------------------------------------
retrieveDocumentVersion :: forall e. ID -> MonadPerspectives (AjaxAvarCache e) String
retrieveDocumentVersion url = do
  (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
  (res :: AJ.AffjaxResponse Unit) <- liftAff $ AJ.affjax $ rq {method = Left HEAD, url = url}
  vs <- version res.headers
  liftAff $ onAccepted res.status [200, 304] "retrieveDocumentVersion" (pure vs)

  where
    version :: Array ResponseHeader -> MonadPerspectives (AjaxAvarCache e) String
    version headers =  case find (\rh -> (responseHeaderName rh) == "ETag") headers of
      Nothing -> throwError $ error ("retrieveDocumentVersion: couchdb returns no ETag header holding a document version number for " <> url)
      (Just h) -> (pure $ responseHeaderValue h) >>= removeDoubleQuotes

    removeDoubleQuotes :: String -> MonadPerspectives (AjaxAvarCache e) String
    removeDoubleQuotes s = do
      ms1 <- pure $ stripPrefix (Pattern "\"") s
      case ms1 of
        Nothing -> throwError $ error ("retrieveDocumentVersion: couchdb returns ETag value that is not a valid JSON string for " <> url)
        (Just s1) -> do
          ms2 <- pure $ stripSuffix (Pattern "\"") s1
          case ms2 of
            Nothing -> throwError $ error ("retrieveDocumentVersion: couchdb returns ETag value that is not a double quoted string for " <> url)
            (Just s2) -> pure s2
