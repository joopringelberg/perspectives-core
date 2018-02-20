module Perspectives.Couchdb.Databases where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (fromObject, fromString)
import Data.Map (fromFoldable)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AffjaxResponse, put) as AJ
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.Couchdb (CouchdbStatusCodes, DatabaseName, Password, PostCouchdb_session, User, onAccepted, onAccepted')
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PerspectivesState (MonadPerspectives, couchdbSessionStarted, setCouchdbSessionStarted)
import Perspectives.User (getCouchdbBaseURL, getUser, getCouchdbPassword)
import Prelude (Unit, bind, (<>), ($), pure, unit, discard)


-- TODO: relogin if expired. Catch and save the cookie for the server version.
ensureAuthentication :: forall e a. Aff (AjaxAvarCache e) a -> MonadPerspectives (AjaxAvarCache e) a
ensureAuthentication a = do
  authenticated <- couchdbSessionStarted
  if authenticated
    then liftAff a
    else do
      authenticate
      liftAff a

-----------------------------------------------------------
-- CREATEFIRSTADMIN
-- Notice: no authentication. Requires Couchdb to be in party mode.
-----------------------------------------------------------
createFirstAdmin :: forall e. User -> Password -> Aff (AjaxAvarCache e) Unit
createFirstAdmin user password = do
  base <- getCouchdbBaseURL
  (res :: AJ.AffjaxResponse String) <- AJ.put (base <> "_config/admins/" <> user) password
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
  (res :: AJ.AffjaxResponse String) <- AJ.put (base <> dbname) unit
  onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit

deleteDatabase :: forall e. DatabaseName -> MonadPerspectives (AjaxAvarCache e) Unit
deleteDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  (res :: AJ.AffjaxResponse String) <- AJ.put (base <> dbname) unit
  onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit

-----------------------------------------------------------
-- AUTHENTICATION
-- See: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie
-----------------------------------------------------------
-- TODO: Catch and save the cookie for the server version.
authenticate :: forall e. MonadPerspectives (AjaxAvarCache e) Unit
authenticate = do
  base <- liftAff getCouchdbBaseURL
  usr <- liftAff getUser
  pwd <- liftAff getCouchdbPassword
  (res :: AJ.AffjaxResponse PostCouchdb_session) <- liftAff $ AJ.put
    (base <> "_session")
    (fromObject (StrMap.fromFoldable [Tuple "name" (fromString usr), Tuple "password" (fromString pwd)]))
  case res.status of
    (StatusCode 200) -> setCouchdbSessionStarted true
    (StatusCode 203) -> setCouchdbSessionStarted true
    otherwise -> do
      setCouchdbSessionStarted false
      throwError $ error "Failure in authenticate. Unauthorized. Username or password wasn’t recognized"
