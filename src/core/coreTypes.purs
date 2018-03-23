module Perspectives.CoreTypes where

import Perspectives.EntiteitAndRDFAliases

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.StrMap (StrMap)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (class Eq, class Show, show, (&&), (<>), (==))

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextDefinitions = GLStrMap (AVar PerspectContext)
type RolDefinitions = GLStrMap (AVar PerspectRol)
type DomeinCache = GLStrMap (AVar DomeinFile)

type PerspectivesState =
  { rolDefinitions :: RolDefinitions
  , contextDefinitions :: ContextDefinitions
  , domeinCache :: DomeinCache
  , userInfo :: UserInfo
  , couchdbSessionStarted :: Boolean
  , sessionCookie :: AVar String
  , memorizeQueryResults :: Boolean
  -- , queryFunctions :: StrMap QueryFunction
  }

-----------------------------------------------------------
-- USERINFO
-----------------------------------------------------------
type UserInfo =
  { userName :: String
  , couchdbPassword :: String
  , couchdbBaseURL :: String
  }

-----------------------------------------------------------
-- MONADPERSPECTIVES, MONADPERSPECTIVESQUERY
-----------------------------------------------------------
-- | MonadPerspectives is an instance of MonadAff.
-- | So, with liftAff we lift an operation in Aff to MonadPerspectives.
type MonadPerspectives e = ReaderT (Ref PerspectivesState) (Aff (ref :: REF | e))

-- | The QueryEnvironment is a set of bindings of variableNames (Strings) to either ID's or the string representation of simple values.
type QueryEnvironment = StrMap (Array String)

type MonadPerspectivesQuery e =  StateT QueryEnvironment (MonadPerspectives e)

type MonadPerspectivesObjects e = MonadPerspectives (AjaxAvarCache e) (Array ID)

-----------------------------------------------------------
-- TRIPLEGETTER
-----------------------------------------------------------
type TripleGetter e = Subject -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)

data NamedFunction f = NamedFunction String f

newtype Triple e = Triple
  { subject :: Subject
  , predicate :: Predicate
  , object :: Array Value
  , dependencies :: Array TripleRef
  , supports :: Array TripleRef
  , tripleGetter :: TripleGetter e}

instance showTriple :: Show (Triple e) where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: Eq (Triple e) where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)


-----------------------------------------------------------
-- TRIPLEREF
-----------------------------------------------------------
newtype TripleRef = TripleRef { subject :: Subject, predicate :: Predicate}

instance eqTripleRef :: Eq TripleRef where
  eq (TripleRef({subject: s1, predicate: p1})) (TripleRef({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

instance showTripleRef :: Show TripleRef where
  show (TripleRef {subject, predicate}) = "{" <> show subject <> ", " <> show predicate <> "}"

-----------------------------------------------------------
-- QUERYFUNCTION
-----------------------------------------------------------
type QueryFunction =
  { function :: TripleGetter ()
  , domain :: String
  , range :: String}
