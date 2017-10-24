module Test.GlobalAVar where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref(REF, Ref, newRef, writeRef, readRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, peekVar, putVar, takeVar)
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax)

-- Dit is een computation in Eff met het REF effect, die een Ref zal opleveren die leeg kan zijn, of een AVar kan bevatten (Ref (Maybe (AVar String))).
t :: forall r. Eff (ref :: REF | r) (Ref (Maybe (AVar String)))
t = newRef Nothing
-- Hier verwijderen we eerst het REF effect met unsafeRunRef en dan voeren we de resulterende computation uit met runPure. Het resultaat is dus een waarde met type Ref (Maybe (AVar String)).
ref :: Ref (Maybe (AVar String))
ref = runPure (unsafeRunRef t)

-- Met deze 'ref' simuleren we de index van resources. We kunnen er overal in schrijven en uit lezen.

-- 'makeVar' is een asynchrone computation van een AVar.
-- type AffAVar e a = Aff (avar :: AVAR | e) a
-- makeVar :: forall e a. AffAVar e (AVar a)
-- OFTEWEL:
-- makeVar :: forall e a. Aff (avar :: AVAR | e) (AVar a)

fillRefWithAvar :: forall e. Aff (avar :: AVAR, ref :: REF | e) (AVar String)
fillRefWithAvar = do
  av <- makeVar
  liftEff (writeRef ref (Just av))
  pure av

-- Het resultaat is de string. Dat komt goed uit voor readAvarInRef.
fillAvar :: forall e. String -> Aff (avar :: AVAR, ref :: REF | e) String
fillAvar s = do
  av <- fillRefWithAvar
  _ <- putVar av s
--  _ <- forkAff (delay (Milliseconds 1000.0) *> putVar av s)
  pure s

-- Lees de globaal beschikbare REF 'ref' uit. Als er niets is, haal dan asynchroon content op. Anders geef je de inhoud
-- van de AVar in ref.
-- Vergelijk met het ophalen van propDefs uit een Resource.
readAvarInRef :: forall e. Aff(avar :: AVAR, ref :: REF, ajax :: AJAX | e) String
readAvarInRef = do
  maybeA <- liftEff $ readRef ref
  case maybeA of
    Nothing -> do
                r <- fetchContent
                fillAvar r
    (Just a) -> peekVar a

fetchContent :: forall e. Aff (avar :: AVAR, ajax :: AJAX | e) String
fetchContent = do
  v <- makeVar
  -- Het vermoeden bestaat dat het effect van forken beperkt blijft tot deze computation; d.w.z. dat een computation waar dit een component van is, wel degelijk wacht.
  _ <- forkAff do
        res <- affjax $ userResourceRequest
        putVar v res.response
--  res <- affjax $ userResourceRequest
  takeVar v

userResourceRequest :: AffjaxRequest Unit
userResourceRequest =
  { method: Left GET
  , url: "http://localhost:5984/user_cor_contexts2/user:xGebruiker"
  , headers: []
  , content: Nothing
  , username: Just "cor"
  , password: Just "geheim"
  , withCredentials: true
  }

baseURL :: String
baseURL = "http://localhost:5984/user_cor_contexts2/"
