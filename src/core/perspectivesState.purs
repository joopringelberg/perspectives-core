module Perspectives.PerspectivesState where

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons)
import Data.Maybe (Maybe)
import Effect.Aff.AVar (AVar, put, read, take, tryRead)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (ContextDefinitions, DomeinCache, MonadPerspectives, PerspectivesState, RolDefinitions, Transactie, TripleQueue, TripleRef)
import Perspectives.CouchdbState (UserInfo)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke, delete)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Prelude (Unit, bind, pure, unit, ($), (<<<), (>>=))

newPerspectivesState :: UserInfo -> Transactie -> AVar String -> PerspectivesState
newPerspectivesState uinfo tr av =
  {
  -- weghalen:
  rolDefinitions: new unit
  , contextDefinitions: new unit
  -- Aanvullen met Perspectives types
  , contexts: new unit
  , enumeratedRoles: new unit
  , calculatedRoles: new unit
  , enumeratedProperties: new unit
  , calculatedProperties: new unit
  , views: new unit

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
couchdbSessionStarted :: MonadPerspectives Boolean
couchdbSessionStarted = gets _.couchdbSessionStarted

setCouchdbSessionStarted :: Boolean -> MonadPerspectives Unit
setCouchdbSessionStarted b = modify \ps -> ps {couchdbSessionStarted = b}

sessionCookie :: MonadPerspectives (AVar String)
sessionCookie = gets _.sessionCookie

takeSessionCookieValue :: MonadPerspectives String
takeSessionCookieValue = gets _.sessionCookie >>= lift <<< take

readSessionCookieValue :: MonadPerspectives String
readSessionCookieValue = gets _.sessionCookie >>= lift <<< read

tryReadSessionCookieValue :: MonadPerspectives (Maybe String)
tryReadSessionCookieValue = gets _.sessionCookie >>= lift <<< tryRead

setSessionCookie :: String -> MonadPerspectives Unit
setSessionCookie c = sessionCookie >>= (lift <<< put c)

transactie :: MonadPerspectives Transactie
transactie = gets _.transactie

setTripleQueue :: TripleQueue -> MonadPerspectives Unit
setTripleQueue t = modify \s -> s { tripleQueue = t }

tripleQueue :: MonadPerspectives TripleQueue
tripleQueue = gets _.tripleQueue

setTransactie :: Transactie -> MonadPerspectives Unit
setTransactie t = modify \s -> s { transactie = t }

contextDefinitions :: MonadPerspectives ContextDefinitions
contextDefinitions = gets _.contextDefinitions

contextDefinitionsLookup :: String -> MonadPerspectives (Maybe (AVar PerspectContext))
contextDefinitionsLookup = lookup contextDefinitions

contextDefinitionsInsert :: String -> AVar PerspectContext -> MonadPerspectives (AVar PerspectContext)
contextDefinitionsInsert = insert contextDefinitions

contextDefinitionsRemove :: String -> MonadPerspectives (Maybe (AVar PerspectContext))
contextDefinitionsRemove = remove contextDefinitions

rolDefinitions :: MonadPerspectives RolDefinitions
rolDefinitions = gets _.rolDefinitions

rolDefinitionsLookup :: String -> MonadPerspectives (Maybe (AVar PerspectRol))
rolDefinitionsLookup = lookup rolDefinitions

rolDefinitionsInsert :: String -> AVar PerspectRol -> MonadPerspectives (AVar PerspectRol)
rolDefinitionsInsert = insert rolDefinitions

rolDefinitionsRemove :: String -> MonadPerspectives (Maybe (AVar PerspectRol))
rolDefinitionsRemove = remove rolDefinitions

domeinCache :: MonadPerspectives DomeinCache
domeinCache = gets _.domeinCache

domeinCacheLookup :: String -> MonadPerspectives (Maybe (AVar DomeinFile))
domeinCacheLookup = lookup domeinCache

domeinCacheInsert :: String -> AVar DomeinFile -> MonadPerspectives (AVar DomeinFile)
domeinCacheInsert = insert domeinCache

domeinCacheRemove :: String -> MonadPerspectives (Maybe (AVar DomeinFile))
domeinCacheRemove = remove domeinCache

-----------------------------------------------------------
-- FUNCTIONS TO MODIFY GLOBAL UNSAFE STRMAPS IN PERSPECTIVESSTATE
-----------------------------------------------------------
insert :: forall a.
  MonadPerspectives (GLStrMap a) ->
  String ->
  a ->
  MonadPerspectives a
insert g ns av = do
  (dc :: (GLStrMap a)) <- g
  _ <- liftEffect $ (poke dc ns av)
  pure av

lookup :: forall a.
  MonadPerspectives (GLStrMap a) ->
  String ->
  MonadPerspectives (Maybe a)
lookup g k = do
  dc <- g
  liftEffect $ peek dc k

remove :: forall a.
  MonadPerspectives (GLStrMap a) ->
  String ->
  MonadPerspectives (Maybe a)
remove g k = do
  (dc :: (GLStrMap a)) <- g
  ma <- liftEffect $ peek dc k
  _ <- liftEffect $ (delete dc k)
  pure ma

-----------------------------------------------------------
-- FOR DEBUGGING ONLY
-----------------------------------------------------------
setRecomputed :: Array TripleRef -> MonadPerspectives Unit
setRecomputed t = modify \s -> s { recomputed = t }

getRecomputed :: MonadPerspectives (Array TripleRef)
getRecomputed = gets _.recomputed

addToRecomputed :: TripleRef -> MonadPerspectives Unit
addToRecomputed i = do
  r <- getRecomputed
  setRecomputed (cons i r)
