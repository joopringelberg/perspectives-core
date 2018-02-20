module Perspectives.PerspectivesState where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.State.Trans (StateT, get, modify)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Perspectives.User (UserInfo)
import Prelude (Unit, pure, unit, (<<<), (>>=))

type PerspectivesState =
  { rolDefinitions :: GLStrMap (AVar PerspectRol)
  , contextDefinitions :: GLStrMap (AVar PerspectContext)
  , domeinCache :: GLStrMap (AVar DomeinFile)
  , userInfo :: UserInfo
  , couchdbSessionStarted :: Boolean
  }

newPerspectivesState :: UserInfo -> PerspectivesState
newPerspectivesState uinfo =
  { rolDefinitions: new unit
  , contextDefinitions: new unit
  , domeinCache: new unit
  , userInfo: uinfo
  , couchdbSessionStarted: false
  }

-- | MonadPerspectives is an instance of MonadAff.
-- | So, with liftAff we lift an operation in Aff to MonadPerspectives.
type MonadPerspectives e a = StateT PerspectivesState (Aff e) a

couchdbSessionStarted :: forall e. MonadPerspectives e Boolean
couchdbSessionStarted = get >>= (pure <<< _.couchdbSessionStarted)

setCouchdbSessionStarted :: forall e. Boolean -> MonadPerspectives e Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}
