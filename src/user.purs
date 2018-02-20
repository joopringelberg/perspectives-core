module Perspectives.User where

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.State.Trans (modify, gets)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Perspectives.PerspectivesState (MonadPerspectives)
import Prelude (Unit, bind, unit, ($), pure, (<>), (>>>))

type UserCache = GLStrMap (AVar String)

userCache :: UserCache
userCache = new unit

getUser :: forall e. MonadPerspectives e String
getUser = gets $ _.userInfo >>> _.userName

setUser :: forall e. String -> MonadPerspectives e Unit
setUser n = modify \ps@{userInfo} -> ps {userInfo = userInfo {userName = n}}

getCouchdbPassword :: forall e. MonadPerspectives e String
getCouchdbPassword = gets $ _.userInfo >>> _.couchdbPassword

setCouchdbPassword :: forall e. String -> MonadPerspectives e Unit
setCouchdbPassword pw = modify \ps@{userInfo} -> ps {userInfo = userInfo {couchdbPassword = pw}}

-- | Url terminated with a forward slash.
getCouchdbBaseURL :: forall e. MonadPerspectives e String
getCouchdbBaseURL = gets $ _.userInfo >>> _.couchdbBaseURL

setCouchdbBaseURL :: forall e. String -> MonadPerspectives e Unit
setCouchdbBaseURL pw = modify \ps@{userInfo} -> ps {userInfo = userInfo {couchdbBaseURL = pw}}

-- | Url terminated with a forward slash.
entitiesDatabase :: forall e. MonadPerspectives e String
entitiesDatabase = do
  usr <- getUser
  cdbUrl <- getCouchdbBaseURL
  pure $ cdbUrl <> "user_" <> usr <> "_entities/"
