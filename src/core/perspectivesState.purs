module Perspectives.PerspectivesState where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, readVar, takeVar, tryReadVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (ContextDefinitions, DomeinCache, MonadPerspectives, PerspectivesState, RolDefinitions, Transactie, TripleQueue, createTransactie)
import Perspectives.CouchdbState (UserInfo)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Effects (AvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke, delete)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, bind, pure, unit, ($), (<<<), (>>=))
import Unsafe.Coerce (unsafeCoerce)

newPerspectivesState :: forall c r b. UserInfo -> Transactie -> AVar String -> PerspectivesState
newPerspectivesState uinfo tr av =
  { rolDefinitions: new unit
  , contextDefinitions: new unit
  , domeinCache: new unit
  , userInfo: uinfo
  , couchdbSessionStarted: false
  , sessionCookie: av
  , memorizeQueryResults: true
  , transactie: tr
  , tripleQueue: []
  -- , queryCache: new unit
  }

-- | Run an action in MonadPerspectives, given a username and password.
runPerspectives :: forall a c r b e.
  String ->
  String ->
  MonadPerspectives (avar :: AVAR, now :: NOW | e) a ->
  Aff (avar :: AVAR, now :: NOW | e) a
runPerspectives userName password mp = do
  (av :: AVar String) <- makeVar "This value will be removed on first authentication!"
  (tr :: Transactie) <- createTransactie userName
  (rf :: AVar PerspectivesState) <- makeVar $
    newPerspectivesState
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"}
      tr
      av
  runReaderT mp rf

runPerspectivesWithState :: forall c r b e a. MonadPerspectives (avar :: AVAR | e) a -> (AVar PerspectivesState) -> Aff (avar :: AVAR | e) a
runPerspectivesWithState = runReaderT

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: forall e. MonadPerspectives (avar :: AVAR | e) Boolean
couchdbSessionStarted = gets _.couchdbSessionStarted

setCouchdbSessionStarted :: forall c r b e. Boolean -> MonadPerspectives (avar :: AVAR | e) Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}

sessionCookie :: forall e. MonadPerspectives (avar :: AVAR | e) (AVar String)
sessionCookie = gets _.sessionCookie

takeSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) String
takeSessionCookieValue = gets _.sessionCookie >>= lift <<< takeVar

readSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) String
readSessionCookieValue = gets _.sessionCookie >>= lift <<< readVar

tryReadSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) (Maybe String)
tryReadSessionCookieValue = gets _.sessionCookie >>= lift <<< tryReadVar

setSessionCookie :: forall c r b e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setSessionCookie c = sessionCookie >>= (lift <<< putVar c)

transactie :: forall e. MonadPerspectives (avar :: AVAR | e) Transactie
transactie = gets _.transactie

setTripleQueue :: forall c r b e. TripleQueue -> MonadPerspectives (avar :: AVAR | e) Unit
setTripleQueue t = modify \s -> s { tripleQueue = t }

tripleQueue :: forall e. MonadPerspectives (avar :: AVAR | e) TripleQueue
tripleQueue = gets _.tripleQueue

setTransactie :: forall c r b e. Transactie -> MonadPerspectives (avar :: AVAR | e) Unit
setTransactie t = modify \s -> s { transactie = t }

contextDefinitions :: forall e. MonadPerspectives (avar :: AVAR | e) ContextDefinitions
contextDefinitions = gets _.contextDefinitions

-- | Here, we cannot know what the type of the Context is. Hence we'll have to type it further up in the call-hierarchy.
contextDefinitionsLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar (PerspectContext String)))
contextDefinitionsLookup = lookup contextDefinitions

contextDefinitionsInsert :: forall c r b e. String -> AVar (PerspectContext c) -> MonadPerspectives (AvarCache e) Unit
contextDefinitionsInsert rid av = do
  (av' :: AVar (PerspectContext String)) <- pure $ unsafeCoerce av
  insert contextDefinitions rid av'

contextDefinitionsRemove :: forall c r b e. String -> MonadPerspectives (AvarCache e) Unit
contextDefinitionsRemove = remove contextDefinitions

rolDefinitions :: forall e. MonadPerspectives (avar :: AVAR | e) RolDefinitions
rolDefinitions = gets _.rolDefinitions

-- | Here, we cannot know what the type of the Rol is. Hence we'll have to type it further up in the call-hierarchy.
rolDefinitionsLookup :: forall e.
  String ->
  MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar (PerspectRol String String)))
rolDefinitionsLookup = lookup rolDefinitions

rolDefinitionsInsert :: forall c r b e.
  String ->
  AVar (PerspectRol r b) ->
  MonadPerspectives (AvarCache e) Unit
rolDefinitionsInsert rid av = do
  (av' :: AVar (PerspectRol String String)) <- pure $ unsafeCoerce av
  insert rolDefinitions rid av'

rolDefinitionsRemove :: forall r b e. String -> MonadPerspectives (AvarCache e) Unit
rolDefinitionsRemove = remove rolDefinitions

domeinCache :: forall e. MonadPerspectives (avar :: AVAR | e) DomeinCache
domeinCache = gets _.domeinCache

domeinCacheLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar DomeinFile))
domeinCacheLookup = lookup domeinCache

domeinCacheInsert :: forall e. String -> AVar DomeinFile -> MonadPerspectives (AvarCache e) Unit
domeinCacheInsert = insert domeinCache

domeinCacheRemove :: forall e. String -> MonadPerspectives (AvarCache e) Unit
domeinCacheRemove = remove domeinCache

insert :: forall e a.
  MonadPerspectives (gm :: GLOBALMAP | e) (GLStrMap a) ->
  String ->
  a ->
  MonadPerspectives (gm :: GLOBALMAP | e) Unit
insert g ns av = do
  (dc :: (GLStrMap a)) <- g
  _ <- liftAff $ liftEff $ (poke dc ns av)
  pure unit

lookup :: forall e a.
  MonadPerspectives (gm :: GLOBALMAP | e) (GLStrMap a) ->
  String ->
  MonadPerspectives (gm :: GLOBALMAP | e) (Maybe a)
lookup g k = do
  dc <- g
  liftAff $ liftEff $ peek dc k

remove :: forall e a.
  MonadPerspectives (gm :: GLOBALMAP | e) (GLStrMap a) ->
  String ->
  MonadPerspectives (gm :: GLOBALMAP | e) Unit
remove g k = do
  (dc :: (GLStrMap a)) <- g
  ma <- liftAff $ liftEff $ peek dc k
  _ <- liftAff $ liftEff $ (delete dc k)
  pure unit
