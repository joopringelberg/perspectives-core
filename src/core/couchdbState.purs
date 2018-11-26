module Perspectives.CouchdbState where

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (putVar, readVar, takeVar, tryReadVar)
import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Eff.AVar (AVAR, AVar)
import Control.Monad.Reader (ReaderT, lift)
import Data.Maybe (Maybe)

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
type UserInfo =
  { userName :: String
  , couchdbPassword :: String
  , couchdbBaseURL :: String
  }

-----------------------------------------------------------
-- COUCHDBSTATE
-----------------------------------------------------------
type CouchdbState f =
  { userInfo :: UserInfo
  , couchdbSessionStarted :: Boolean
  , sessionCookie :: AVar String
  | f
  }

type MonadCouchdb e f = ReaderT (AVar (CouchdbState f)) (Aff e)

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: forall e f. MonadCouchdb (avar :: AVAR | e) f Boolean
couchdbSessionStarted = gets _.couchdbSessionStarted

setCouchdbSessionStarted :: forall e f. Boolean -> MonadCouchdb (avar :: AVAR | e) f Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}

sessionCookie :: forall e f. MonadCouchdb (avar :: AVAR | e) f (AVar String)
sessionCookie = gets _.sessionCookie

takeSessionCookieValue :: forall e f. MonadCouchdb (avar :: AVAR | e) f String
takeSessionCookieValue = gets _.sessionCookie >>= lift <<< takeVar

readSessionCookieValue :: forall e f. MonadCouchdb (avar :: AVAR | e) f String
readSessionCookieValue = gets _.sessionCookie >>= lift <<< readVar

tryReadSessionCookieValue :: forall e f. MonadCouchdb (avar :: AVAR | e) f (Maybe String)
tryReadSessionCookieValue = gets _.sessionCookie >>= lift <<< tryReadVar

setSessionCookie :: forall e f. String -> MonadCouchdb (avar :: AVAR | e) f Unit
setSessionCookie c = sessionCookie >>= (lift <<< putVar c)
