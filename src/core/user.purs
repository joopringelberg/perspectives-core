module Perspectives.User where

import Control.Monad.Aff.AVar (AVAR, AVar)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Control.Monad.AvarMonadAsk (gets, modify)

import Prelude (Unit, bind, unit, ($), pure, (<>), (>>>))

type UserCache = GLStrMap (AVar String)

userCache :: UserCache
userCache = new unit

getUser :: forall e. MonadPerspectives (avar :: AVAR | e) String
getUser = gets $ _.userInfo >>> _.userName

setUser :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setUser n = modify \ps@{userInfo} -> ps {userInfo = userInfo {userName = n}}

getCouchdbPassword :: forall e. MonadPerspectives (avar :: AVAR | e) String
getCouchdbPassword = gets $ _.userInfo >>> _.couchdbPassword

setCouchdbPassword :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setCouchdbPassword pw = modify \ps@{userInfo} -> ps {userInfo = userInfo {couchdbPassword = pw}}

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall e. MonadPerspectives (avar :: AVAR | e) String
getCouchdbBaseURL = gets $ _.userInfo >>> _.couchdbBaseURL

setCouchdbBaseURL :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setCouchdbBaseURL pw = modify \ps@{userInfo} -> ps {userInfo = userInfo {couchdbBaseURL = pw}}

-- | Url terminated with a forward slash.
entitiesDatabase :: forall e. MonadPerspectives (avar :: AVAR | e) String
entitiesDatabase = do
  usr <- getUser
  cdbUrl <- getCouchdbBaseURL
  pure $ cdbUrl <> "user_" <> usr <> "_entities/"
