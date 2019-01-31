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

newPerspectivesState :: forall c r b. UserInfo -> (Transactie c r b) -> AVar String -> (PerspectivesState c r b)
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
  MonadPerspectives c r b (avar :: AVAR, now :: NOW | e) a ->
  Aff (avar :: AVAR, now :: NOW | e) a
runPerspectives userName password mp = do
  (av :: AVar String) <- makeVar "This value will be removed on first authentication!"
  (tr :: (Transactie c r b)) <- createTransactie userName
  (rf :: AVar (PerspectivesState c r b)) <- makeVar $
    newPerspectivesState
      { userName: userName
      , couchdbPassword: password
      , couchdbBaseURL: "http://127.0.0.1:5984/"}
      tr
      av
  runReaderT mp rf

runPerspectivesWithState :: forall c r b e a. MonadPerspectives c r b (avar :: AVAR | e) a -> (AVar (PerspectivesState c r b)) -> Aff (avar :: AVAR | e) a
runPerspectivesWithState = runReaderT

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) Boolean
couchdbSessionStarted = gets _.couchdbSessionStarted

setCouchdbSessionStarted :: forall c r b e. Boolean -> MonadPerspectives c r b (avar :: AVAR | e) Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}

sessionCookie :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) (AVar String)
sessionCookie = gets _.sessionCookie

takeSessionCookieValue :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) String
takeSessionCookieValue = gets _.sessionCookie >>= lift <<< takeVar

readSessionCookieValue :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) String
readSessionCookieValue = gets _.sessionCookie >>= lift <<< readVar

tryReadSessionCookieValue :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) (Maybe String)
tryReadSessionCookieValue = gets _.sessionCookie >>= lift <<< tryReadVar

setSessionCookie :: forall c r b e. String -> MonadPerspectives c r b (avar :: AVAR | e) Unit
setSessionCookie c = sessionCookie >>= (lift <<< putVar c)

transactie :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) (Transactie c r b)
transactie = gets _.transactie

setTripleQueue :: forall c r b e. TripleQueue -> MonadPerspectives c r b (avar :: AVAR | e) Unit
setTripleQueue t = modify \s -> s { tripleQueue = t }

tripleQueue :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) TripleQueue
tripleQueue = gets _.tripleQueue

setTransactie :: forall c r b e. (Transactie c r b) -> MonadPerspectives c r b (avar :: AVAR | e) Unit
setTransactie t = modify \s -> s { transactie = t }

contextDefinitions :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) (ContextDefinitions c)
contextDefinitions = gets _.contextDefinitions

contextDefinitionsLookup :: forall c r b e. String -> MonadPerspectives c r b (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar (PerspectContext c)))
contextDefinitionsLookup = lookup contextDefinitions

contextDefinitionsInsert :: forall c r b e. String -> AVar (PerspectContext c) -> MonadPerspectives c r b (AvarCache e) (AVar (PerspectContext c))
contextDefinitionsInsert = insert contextDefinitions

contextDefinitionsRemove :: forall c r b e. String -> MonadPerspectives c r b (AvarCache e) (Maybe (AVar (PerspectContext c)))
contextDefinitionsRemove = remove contextDefinitions

rolDefinitions :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) (RolDefinitions r b)
rolDefinitions = gets _.rolDefinitions

rolDefinitionsLookup :: forall c r b e.
  String ->
  MonadPerspectives c r b (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar (PerspectRol r b)))
rolDefinitionsLookup = lookup rolDefinitions

rolDefinitionsInsert :: forall c r b e.
  String ->
  AVar (PerspectRol r b) ->
  MonadPerspectives c r b (AvarCache e) (AVar (PerspectRol r b))
rolDefinitionsInsert = insert rolDefinitions

rolDefinitionsRemove :: forall c r b e. String -> MonadPerspectives c r b (AvarCache e) (Maybe (AVar (PerspectRol r b)))
rolDefinitionsRemove = remove rolDefinitions

domeinCache :: forall c r b e. MonadPerspectives c r b (avar :: AVAR | e) (DomeinCache c r b)
domeinCache = gets _.domeinCache

domeinCacheLookup :: forall c r b e. String -> MonadPerspectives c r b (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar (DomeinFile c r b)))
domeinCacheLookup = lookup domeinCache

domeinCacheInsert :: forall c r b e. String -> AVar (DomeinFile c r b) -> MonadPerspectives c r b (AvarCache e) (AVar (DomeinFile c r b))
domeinCacheInsert = insert domeinCache

domeinCacheRemove :: forall c r b e. String -> MonadPerspectives c r b (AvarCache e) (Maybe (AVar (DomeinFile c r b)))
domeinCacheRemove = remove domeinCache

insert :: forall c r b e a.
  MonadPerspectives c r b (gm :: GLOBALMAP | e) (GLStrMap a) ->
  String ->
  a ->
  MonadPerspectives c r b (gm :: GLOBALMAP | e) a
insert g ns av = do
  (dc :: (GLStrMap a)) <- g
  _ <- liftAff $ liftEff $ (poke dc ns av)
  pure av

lookup :: forall c r b e a.
  MonadPerspectives c r b (gm :: GLOBALMAP | e) (GLStrMap a) ->
  String ->
  MonadPerspectives c r b (gm :: GLOBALMAP | e) (Maybe a)
lookup g k = do
  dc <- g
  liftAff $ liftEff $ peek dc k

remove :: forall c r b e a.
  MonadPerspectives c r b (gm :: GLOBALMAP | e) (GLStrMap a) ->
  String ->
  MonadPerspectives c r b (gm :: GLOBALMAP | e) (Maybe a)
remove g k = do
  (dc :: (GLStrMap a)) <- g
  ma <- liftAff $ liftEff $ peek dc k
  _ <- liftAff $ liftEff $ (delete dc k)
  pure ma
