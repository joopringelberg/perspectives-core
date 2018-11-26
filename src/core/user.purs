module Perspectives.User where

import Control.Monad.Aff.AVar (AVAR, AVar)
import Control.Monad.AvarMonadAsk (gets, modify)
import Perspectives.CouchdbState (MonadCouchdb)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Prelude (Unit, bind, unit, ($), pure, (<>), (>>>))

type UserCache = GLStrMap (AVar String)

userCache :: UserCache
userCache = new unit

getUser :: forall e f. MonadCouchdb (avar :: AVAR | e) f String
getUser = gets $ _.userInfo >>> _.userName

setUser :: forall e f. String -> MonadCouchdb (avar :: AVAR | e) f Unit
setUser n = modify \ps@{userInfo} -> ps {userInfo = userInfo {userName = n}}

getCouchdbPassword :: forall e f. MonadCouchdb (avar :: AVAR | e) f String
getCouchdbPassword = gets $ _.userInfo >>> _.couchdbPassword

setCouchdbPassword :: forall e f. String -> MonadCouchdb (avar :: AVAR | e) f Unit
setCouchdbPassword pw = modify \ps@{userInfo} -> ps {userInfo = userInfo {couchdbPassword = pw}}

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall e f. MonadCouchdb (avar :: AVAR | e) f String
getCouchdbBaseURL = gets $ _.userInfo >>> _.couchdbBaseURL

setCouchdbBaseURL :: forall e f. String -> MonadCouchdb (avar :: AVAR | e) f Unit
setCouchdbBaseURL pw = modify \ps@{userInfo} -> ps {userInfo = userInfo {couchdbBaseURL = pw}}

-- | Url terminated with a forward slash.
entitiesDatabase :: forall e f. MonadCouchdb (avar :: AVAR | e) f String
entitiesDatabase = do
  usr <- getUser
  cdbUrl <- getCouchdbBaseURL
  pure $ cdbUrl <> "user_" <> usr <> "_entities/"
