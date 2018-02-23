module Perspectives.PerspectivesState where

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Effects (AvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, bind, flip, pure, unit, ($), (<<<), (>>=))

type ContextDefinitions = GLStrMap (AVar PerspectContext)
type RolDefinitions = GLStrMap (AVar PerspectRol)
type DomeinCache = GLStrMap (AVar DomeinFile)

type PerspectivesState =
  { rolDefinitions :: RolDefinitions
  , contextDefinitions :: ContextDefinitions
  , domeinCache :: DomeinCache
  , userInfo :: UserInfo
  , couchdbSessionStarted :: Boolean
  , memorizeQueryResults :: Boolean
  }

type UserInfo =
  { userName :: String
  , couchdbPassword :: String
  , couchdbBaseURL :: String
  }

newPerspectivesState :: UserInfo -> PerspectivesState
newPerspectivesState uinfo =
  { rolDefinitions: new unit
  , contextDefinitions: new unit
  , domeinCache: new unit
  , userInfo: uinfo
  , couchdbSessionStarted: false
  , memorizeQueryResults: true
  }

-- | MonadPerspectives is an instance of MonadAff.
-- | So, with liftAff we lift an operation in Aff to MonadPerspectives.
type MonadPerspectives e = ReaderT (Ref PerspectivesState) (Aff (ref :: REF | e))

-----------------------------------------------------------
-- GLOBAL STATE IN READERT
-----------------------------------------------------------
-- | Get the global state reference.
getRefWithGlobalState :: forall e. MonadPerspectives e (Ref PerspectivesState)
getRefWithGlobalState = ask

-- | Read the contents of the global state reference.
-- | Compares with StateT get.
getGlobalState :: forall e. MonadPerspectives e PerspectivesState
getGlobalState = getRefWithGlobalState >>= lift <<< liftEff' <<< readRef

-- | Apply a function to the contents of the global state reference.
-- | Compares with StateT gets.
getsGlobalState :: forall a e. (PerspectivesState -> a) -> MonadPerspectives e a
getsGlobalState f = getGlobalState >>= pure <<< f

-- | Write into the global state reference.
-- | Compares with StateT put.
putGlobalState :: forall e. PerspectivesState -> MonadPerspectives e Unit
putGlobalState v = getRefWithGlobalState >>= (lift <<< liftEff' <<< (flip writeRef v))

-- | Apply a function to the contents of the global state reference and save it.
-- | Compares with StateT modify.
modifyGlobalState :: forall e. (PerspectivesState -> PerspectivesState) -> MonadPerspectives e Unit
modifyGlobalState f = getGlobalState >>= putGlobalState <<< f

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: forall e. MonadPerspectives e Boolean
couchdbSessionStarted = getsGlobalState _.couchdbSessionStarted

setCouchdbSessionStarted :: forall e. Boolean -> MonadPerspectives e Unit
setCouchdbSessionStarted b = modifyGlobalState \ps -> ps {couchdbSessionStarted = b}

-- | If memorizeQueryResults == true, we will look up a result in the triple cache
-- | before computing it.
memorizeQueryResults :: forall e. MonadPerspectives e Boolean
memorizeQueryResults = getsGlobalState _.memorizeQueryResults

setMemorizeQueryResults :: forall e. Boolean -> MonadPerspectives e Unit
setMemorizeQueryResults b = modifyGlobalState \ps -> ps {memorizeQueryResults = b}

contextDefinitions :: forall e. MonadPerspectives e ContextDefinitions
contextDefinitions = getsGlobalState _.contextDefinitions

contextDefinitionsLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP | e) (Maybe (AVar PerspectContext))
contextDefinitionsLookup = lookup contextDefinitions

contextDefinitionsInsert :: forall e. String -> AVar PerspectContext -> MonadPerspectives (AvarCache e) (AVar PerspectContext)
contextDefinitionsInsert = insert contextDefinitions

rolDefinitions :: forall e. MonadPerspectives e RolDefinitions
rolDefinitions = getsGlobalState _.rolDefinitions

rolDefinitionsLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP | e) (Maybe (AVar PerspectRol))
rolDefinitionsLookup = lookup rolDefinitions

rolDefinitionsInsert :: forall e. String -> AVar PerspectRol -> MonadPerspectives (AvarCache e) (AVar PerspectRol)
rolDefinitionsInsert = insert rolDefinitions

domeinCache :: forall e. MonadPerspectives e DomeinCache
domeinCache = getsGlobalState _.domeinCache

domeinCacheLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP | e) (Maybe (AVar DomeinFile))
domeinCacheLookup = lookup domeinCache

domeinCacheInsert :: forall e. String -> AVar DomeinFile -> MonadPerspectives (AvarCache e) (AVar DomeinFile)
domeinCacheInsert = insert domeinCache

insert :: forall eff a.
  MonadPerspectives (gm :: GLOBALMAP | eff) (GLStrMap (AVar a)) ->
  String ->
  AVar a ->
  MonadPerspectives (gm :: GLOBALMAP | eff) (AVar a)
insert g ns av = do
  (dc :: (GLStrMap (AVar a))) <- g
  _ <- liftAff $ liftEff $ (poke dc ns av)
  pure av

lookup :: forall e a.
  MonadPerspectives (gm :: GLOBALMAP | e) (GLStrMap (AVar a)) ->
  String ->
  MonadPerspectives (gm :: GLOBALMAP | e) (Maybe (AVar a))
lookup g k = do
  dc <- g
  liftAff $ liftEff $ peek dc k
