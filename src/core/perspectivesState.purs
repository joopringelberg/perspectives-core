module Perspectives.PerspectivesState where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, readVar, takeVar, tryReadVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (ContextDefinitions, MonadPerspectives, PerspectivesState, RolDefinitions, DomeinCache, UserInfo)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Effects (AvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, bind, pure, unit, ($), (<<<), (>>=), (>=>))

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
  -> Aff (avar :: AVAR | e) a
runPerspectives userName password mp = do
  (av :: AVar String) <- makeVar "This value will be removed on first authentication!"
  (rf :: AVar PerspectivesState) <- makeVar $
    newPerspectivesState
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"}
      av
  runReaderT mp rf

runPerspectivesWithState :: forall e a. MonadPerspectives (avar :: AVAR | e) a -> (AVar PerspectivesState) -> Aff (avar :: AVAR | e) a
runPerspectivesWithState = runReaderT

-----------------------------------------------------------
-- GLOBAL STATE IN READERT
-----------------------------------------------------------
-- | Returns the AVar holding the Perspectives state.
getAVarWithPerspectivesState :: forall e. MonadPerspectives e (AVar PerspectivesState)
getAVarWithPerspectivesState = ask

-- | Returns the Perspectives state. Does not block the AVar: it is not left empty.
-- | Compares with StateT get.
getPerspectivesState :: forall e. MonadPerspectives (avar :: AVAR | e) PerspectivesState
getPerspectivesState = getAVarWithPerspectivesState >>= lift <<< readVar

-- | Returns the result of applying a function to the Perspectives state.
-- | Compares with StateT gets.
getsPerspectivesState :: forall a e. (PerspectivesState -> a) -> MonadPerspectives (avar :: AVAR | e) a
getsPerspectivesState f = getPerspectivesState >>= pure <<< f

-- | Puts a new state in the AVar holding the Perspectives state.
-- | Compares with StateT put.
putPerspectivesState :: forall e. PerspectivesState -> MonadPerspectives (avar :: AVAR | e) Unit
putPerspectivesState state = getAVarWithPerspectivesState >>= (lift <<< (replaceAVarContent state))

-- | Make an AVar empty. Blocks as long as the AVar passed in is empty.
clearVar :: forall e a. AVar a -> Aff (avar :: AVAR | e) (AVar a)
clearVar v = do
  _ <- takeVar v
  pure v

-- | Replace the content of an AVar. Blocks as long as the AVar passed in is empty.
replaceAVarContent :: forall e a. a -> AVar a -> Aff (avar :: AVAR | e) Unit
replaceAVarContent value = clearVar >=> putVar value

-- | Replace the content of the AVar holding the Perspectives state, with the result of applying a function to its contents. Does not block.
-- | Compares with StateT modify.
modifyPerspectivesState :: forall e. (PerspectivesState -> PerspectivesState) -> MonadPerspectives (avar :: AVAR | e) Unit
modifyPerspectivesState f = getPerspectivesState >>= putPerspectivesState <<< f

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: forall e. MonadPerspectives (avar :: AVAR | e) Boolean
couchdbSessionStarted = getsPerspectivesState _.couchdbSessionStarted

setCouchdbSessionStarted :: forall e. Boolean -> MonadPerspectives (avar :: AVAR | e) Unit
setCouchdbSessionStarted b = modifyPerspectivesState \ps -> ps {couchdbSessionStarted = b}

sessionCookie :: forall e. MonadPerspectives (avar :: AVAR | e) (AVar String)
sessionCookie = getsPerspectivesState _.sessionCookie

takeSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) String
takeSessionCookieValue = getsPerspectivesState _.sessionCookie >>= lift <<< takeVar

readSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) String
readSessionCookieValue = getsPerspectivesState _.sessionCookie >>= lift <<< readVar

tryReadSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) (Maybe String)
tryReadSessionCookieValue = getsPerspectivesState _.sessionCookie >>= lift <<< tryReadVar

setSessionCookie :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setSessionCookie c = sessionCookie >>= (lift <<< putVar c)

contextDefinitions :: forall e. MonadPerspectives (avar :: AVAR | e) ContextDefinitions
contextDefinitions = getsPerspectivesState _.contextDefinitions

contextDefinitionsLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar PerspectContext))
contextDefinitionsLookup = lookup contextDefinitions

contextDefinitionsInsert :: forall e. String -> AVar PerspectContext -> MonadPerspectives (AvarCache e) (AVar PerspectContext)
contextDefinitionsInsert = insert contextDefinitions

rolDefinitions :: forall e. MonadPerspectives (avar :: AVAR | e) RolDefinitions
rolDefinitions = getsPerspectivesState _.rolDefinitions

rolDefinitionsLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar PerspectRol))
rolDefinitionsLookup = lookup rolDefinitions

rolDefinitionsInsert :: forall e. String -> AVar PerspectRol -> MonadPerspectives (AvarCache e) (AVar PerspectRol)
rolDefinitionsInsert = insert rolDefinitions

domeinCache :: forall e. MonadPerspectives (avar :: AVAR | e) DomeinCache
domeinCache = getsPerspectivesState _.domeinCache

domeinCacheLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar DomeinFile))
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
