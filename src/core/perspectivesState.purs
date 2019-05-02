module Perspectives.PerspectivesState where

import Effect.Aff.AVar (AVAR, AVar, putVar, readVar, takeVar, tryReadVar)
import Effect.Aff.Class (liftAff)
import Control.Monad.AvarMonadAsk (gets, modify)
import Effect.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons)
import Data.Maybe (Maybe)
import Perspectives.CoreTypes (ContextDefinitions, DomeinCache, MonadPerspectives, PerspectivesState, RolDefinitions, Transactie, TripleQueue, TripleRef)
import Perspectives.CouchdbState (UserInfo)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Effects (AvarCache)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, peek, poke, delete)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, bind, pure, unit, ($), (<<<), (>>=))

newPerspectivesState :: UserInfo -> Transactie -> AVar String -> PerspectivesState
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
  -- For debugging purposes only:
  , recomputed: []
  -- , queryCache: new unit
  }

-----------------------------------------------------------
-- FUNCTIONS THAT GET OR MODIFY PARTS OF PERSPECTIVESSTATE
-----------------------------------------------------------
couchdbSessionStarted :: forall e. MonadPerspectives (avar :: AVAR | e) Boolean
couchdbSessionStarted = gets _.couchdbSessionStarted

setCouchdbSessionStarted :: forall e. Boolean -> MonadPerspectives (avar :: AVAR | e) Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}

sessionCookie :: forall e. MonadPerspectives (avar :: AVAR | e) (AVar String)
sessionCookie = gets _.sessionCookie

takeSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) String
takeSessionCookieValue = gets _.sessionCookie >>= lift <<< takeVar

readSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) String
readSessionCookieValue = gets _.sessionCookie >>= lift <<< readVar

tryReadSessionCookieValue :: forall e. MonadPerspectives (avar :: AVAR | e) (Maybe String)
tryReadSessionCookieValue = gets _.sessionCookie >>= lift <<< tryReadVar

setSessionCookie :: forall e. String -> MonadPerspectives (avar :: AVAR | e) Unit
setSessionCookie c = sessionCookie >>= (lift <<< putVar c)

transactie :: forall e. MonadPerspectives (avar :: AVAR | e) Transactie
transactie = gets _.transactie

setTripleQueue :: forall e. TripleQueue -> MonadPerspectives (avar :: AVAR | e) Unit
setTripleQueue t = modify \s -> s { tripleQueue = t }

tripleQueue :: forall e. MonadPerspectives (avar :: AVAR | e) TripleQueue
tripleQueue = gets _.tripleQueue

setTransactie :: forall e. Transactie -> MonadPerspectives (avar :: AVAR | e) Unit
setTransactie t = modify \s -> s { transactie = t }

contextDefinitions :: forall e. MonadPerspectives (avar :: AVAR | e) ContextDefinitions
contextDefinitions = gets _.contextDefinitions

contextDefinitionsLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar PerspectContext))
contextDefinitionsLookup = lookup contextDefinitions

contextDefinitionsInsert :: forall e. String -> AVar PerspectContext -> MonadPerspectives (AvarCache e) (AVar PerspectContext)
contextDefinitionsInsert = insert contextDefinitions

contextDefinitionsRemove :: forall e. String -> MonadPerspectives (AvarCache e) (Maybe (AVar PerspectContext))
contextDefinitionsRemove = remove contextDefinitions

rolDefinitions :: forall e. MonadPerspectives (avar :: AVAR | e) RolDefinitions
rolDefinitions = gets _.rolDefinitions

rolDefinitionsLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar PerspectRol))
rolDefinitionsLookup = lookup rolDefinitions

rolDefinitionsInsert :: forall e. String -> AVar PerspectRol -> MonadPerspectives (AvarCache e) (AVar PerspectRol)
rolDefinitionsInsert = insert rolDefinitions

rolDefinitionsRemove :: forall e. String -> MonadPerspectives (AvarCache e) (Maybe (AVar PerspectRol))
rolDefinitionsRemove = remove rolDefinitions

domeinCache :: forall e. MonadPerspectives (avar :: AVAR | e) DomeinCache
domeinCache = gets _.domeinCache

domeinCacheLookup :: forall e. String -> MonadPerspectives (gm :: GLOBALMAP, avar :: AVAR | e) (Maybe (AVar DomeinFile))
domeinCacheLookup = lookup domeinCache

domeinCacheInsert :: forall e. String -> AVar DomeinFile -> MonadPerspectives (AvarCache e) (AVar DomeinFile)
domeinCacheInsert = insert domeinCache

domeinCacheRemove :: forall e. String -> MonadPerspectives (AvarCache e) (Maybe (AVar DomeinFile))
domeinCacheRemove = remove domeinCache

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

remove :: forall eff a.
  MonadPerspectives (gm :: GLOBALMAP | eff) (GLStrMap a) ->
  String ->
  MonadPerspectives (gm :: GLOBALMAP | eff) (Maybe a)
remove g k = do
  (dc :: (GLStrMap a)) <- g
  ma <- liftAff $ liftEff $ peek dc k
  _ <- liftAff $ liftEff $ (delete dc k)
  pure ma

-----------------------------------------------------------
-- FOR DEBUGGING ONLY
-----------------------------------------------------------
setRecomputed :: forall e. Array TripleRef -> MonadPerspectives (avar :: AVAR | e) Unit
setRecomputed t = modify \s -> s { recomputed = t }

getRecomputed :: forall e. MonadPerspectives (avar :: AVAR | e) (Array TripleRef)
getRecomputed = gets _.recomputed

addToRecomputed :: forall e. TripleRef -> MonadPerspectives (avar :: AVAR | e) Unit
addToRecomputed i = do
  r <- getRecomputed
  setRecomputed (cons i r)
