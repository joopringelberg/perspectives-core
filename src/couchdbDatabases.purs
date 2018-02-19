module Perspectives.Couchdb.Databases where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (StateT, get, lift, put)
import Data.Argonaut (fromObject, fromString)
import Data.Map (fromFoldable)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AffjaxResponse, put) as AJ
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.Couchdb (CouchdbStatusCodes, DatabaseName, Password, PostCouchdb_session, User, onAccepted, onAccepted')
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.User (getCouchdbBaseURL, getUser, getCouchdbPassword)
import Prelude (Unit, bind, (<>), ($), pure, unit, discard)

type Authenticated = Boolean
type CouchdbSession e a = StateT Authenticated (Aff e) a

-- TODO: relogin if expired. Catch and save the cookie for the server version.
ensureAuthentication :: forall e a. Aff (AjaxAvarCache e) a -> CouchdbSession (AjaxAvarCache e) a
ensureAuthentication a = do
  authenticated <- get
  if authenticated
    then lift a
    else do
      authenticate
      lift a

-----------------------------------------------------------
-- CREATEFIRSTADMIN
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

createDatabase :: forall e. DatabaseName -> CouchdbSession (AjaxAvarCache e) Unit
createDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  (res :: AJ.AffjaxResponse String) <- AJ.put (base <> dbname) unit
  onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit

deleteDatabase :: forall e. DatabaseName -> CouchdbSession (AjaxAvarCache e) Unit
deleteDatabase dbname = ensureAuthentication do
  base <- getCouchdbBaseURL
  (res :: AJ.AffjaxResponse String) <- AJ.put (base <> dbname) unit
  onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit

-----------------------------------------------------------
-- AUTHENTICATION
-- See: http://127.0.0.1:5984/_utils/docs/api/server/authn.html#api-auth-cookie
-----------------------------------------------------------
-- TODO: Catch and save the cookie for the server version.
authenticate :: forall e. CouchdbSession (AjaxAvarCache e) Unit
authenticate = do
  base <- lift getCouchdbBaseURL
  usr <- lift getUser
  pwd <- lift getCouchdbPassword
  (res :: AJ.AffjaxResponse PostCouchdb_session) <- lift $ AJ.put
    (base <> "_session")
    (fromObject (StrMap.fromFoldable [Tuple "name" (fromString usr), Tuple "password" (fromString pwd)]))
  case res.status of
    (StatusCode 200) -> put true
    (StatusCode 203) -> put true
    otherwise -> do
      put false
      throwError $ error "Failure in authenticate. Unauthorized. Username or password wasn’t recognized"
