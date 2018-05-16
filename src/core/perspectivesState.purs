module Perspectives.PerspectivesState where

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, readVar, takeVar, tryReadVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (ContextDefinitions, MonadPerspectives, PerspectivesState, RolDefinitions, DomeinCache, UserInfo)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Effects (AvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, bind, flip, pure, unit, ($), (<<<), (>>=))

newPerspectivesState :: UserInfo -> AVar String -> PerspectivesState
newPerspectivesState uinfo av =
  { rolDefinitions: new unit
  , contextDefinitions: new unit
  , domeinCache: new unit
  , userInfo: uinfo
  , couchdbSessionStarted: false
  , sessionCookie: av
  , memorizeQueryResults: true
  -- , queryCache: new unit
  }

-- | Run an action in MonadPerspectives, given a username and password.
runPerspectives :: forall a e. String -> String -> MonadPerspectives (avar :: AVAR | e) a
  -> Aff (avar :: AVAR, ref :: REF | e) a
runPerspectives userName password mp = do
  (av :: AVar String) <- makeEmptyVar
  (rf :: Ref PerspectivesState) <- liftEff' $ newRef $
    newPerspectivesState
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"}
      av
  runReaderT mp rf

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

sessionCookie :: forall e. MonadPerspectives e (AVar String)
sessionCookie = getsGlobalState _.sessionCookie

takeSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) String
takeSessionCookieValue = getsGlobalState _.sessionCookie >>= lift <<< takeVar

readSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) String
readSessionCookieValue = getsGlobalState _.sessionCookie >>= lift <<< readVar

tryReadSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) (Maybe String)
tryReadSessionCookieValue = getsGlobalState _.sessionCookie >>= lift <<< tryReadVar

setSessionCookie :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setSessionCookie c = sessionCookie >>= (lift <<< putVar c)

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
  MonadPerspectives (gm :: GLOBALMAP | eff) (GLStrMap a) ->
  String ->
  a ->
  MonadPerspectives (gm :: GLOBALMAP | eff) a
insert g ns av = do
  (dc :: (GLStrMap a)) <- g
  _ <- liftAff $ liftEff $ (poke dc ns av)
  pure av

lookup :: forall e a.
  MonadPerspectives (gm :: GLOBALMAP | e) (GLStrMap a) ->
  String ->
  MonadPerspectives (gm :: GLOBALMAP | e) (Maybe a)
lookup g k = do
  dc <- g
  liftAff $ liftEff $ peek dc k
