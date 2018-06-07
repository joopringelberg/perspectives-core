module Perspectives.User where

import Control.Monad.Aff.AVar (AVAR, AVar)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Perspectives.PerspectivesState (getsPerspectivesState, modifyPerspectivesState)
import Prelude (Unit, bind, unit, ($), pure, (<>), (>>>))

type UserCache = GLStrMap (AVar String)

userCache :: UserCache
userCache = new unit

getUser :: forall e. MonadPerspectives (avar :: AVAR | e) String
getUser = getsPerspectivesState $ _.userInfo >>> _.userName

setUser :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setUser n = modifyPerspectivesState \ps@{userInfo} -> ps {userInfo = userInfo {userName = n}}

getCouchdbPassword :: forall e. MonadPerspectives (avar :: AVAR | e) String
getCouchdbPassword = getsPerspectivesState $ _.userInfo >>> _.couchdbPassword

setCouchdbPassword :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setCouchdbPassword pw = modifyPerspectivesState \ps@{userInfo} -> ps {userInfo = userInfo {couchdbPassword = pw}}

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall e. MonadPerspectives (avar :: AVAR | e) String
getCouchdbBaseURL = getsPerspectivesState $ _.userInfo >>> _.couchdbBaseURL

setCouchdbBaseURL :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setCouchdbBaseURL pw = modifyPerspectivesState \ps@{userInfo} -> ps {userInfo = userInfo {couchdbBaseURL = pw}}

-- | Url terminated with a forward slash.
entitiesDatabase :: forall e. MonadPerspectives (avar :: AVAR | e) String
entitiesDatabase = do
  usr <- getUser
  cdbUrl <- getCouchdbBaseURL
  pure $ cdbUrl <> "user_" <> usr <> "_entities/"
