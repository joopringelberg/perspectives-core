module Perspectives.CoreTypes where

import Perspectives.EntiteitAndRDFAliases

import Control.Monad.Aff (Aff, error)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, evalStateT, gets, modify, get, put)
import Data.Array (head)
import Data.Either (Either)
import Data.Maybe (Maybe, fromJust)
import Data.StrMap (StrMap, empty, insert, lookup, singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.Identifiers (LocalName)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Perspectives.Utilities (onNothing')
import Prelude (class Eq, class Functor, class Monad, class Show, Unit, bind, discard, flip, pure, show, (&&), (<<<), (<>), (==), (>=>))

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextDefinitions = GLStrMap (AVar PerspectContext)
type RolDefinitions = GLStrMap (AVar PerspectRol)
type DomeinCache = GLStrMap (AVar DomeinFile)
type QueryCache e = GLStrMap (TypedTripleGetter e)

type PerspectivesState =
  { rolDefinitions :: RolDefinitions
  , contextDefinitions :: ContextDefinitions
  , domeinCache :: DomeinCache
  , userInfo :: UserInfo
  , couchdbSessionStarted :: Boolean
  , sessionCookie :: AVar String
  , memorizeQueryResults :: Boolean
  -- , queryCache :: QueryCache
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
-- MONADPERSPECTIVES
-----------------------------------------------------------
-- | MonadPerspectives is an instance of MonadAff.
-- | So, with liftAff we lift an operation in Aff to MonadPerspectives.
type MonadPerspectives e = ReaderT (Ref PerspectivesState) (Aff (ref :: REF | e))

type MP e = MonadPerspectives (AjaxAvarCache e)
-----------------------------------------------------------
-- MONADPERSPECTIVESQUERY
-----------------------------------------------------------
-- | The QueryEnvironment is a set of bindings of variableNames (Strings) to references to Triples.
type QueryEnvironment = StrMap TripleRef

type MonadPerspectivesQuery e =  StateT QueryEnvironment (MonadPerspectives e)

type MonadPerspectivesObjects e = MonadPerspectives e (Array ID)

-- | Run the function in a QueryEnvironment that has Subject as the value of "#start".
runMonadPerspectivesQuery :: forall e a.
  Subject
  -> (Subject -> MonadPerspectivesQuery e a)
  -> (MonadPerspectives e a)
runMonadPerspectivesQuery a f = evalStateT (f a) (singleton "#start" t)
  where
    t :: TripleRef
    t = TripleRef
          { subject: a
          , predicate: "model:Perspectives$start"
        }

putQueryVariable :: forall e. VariableName -> TripleRef -> MonadPerspectivesQuery e Unit
putQueryVariable var t = modify \env -> insert var t env

readQueryVariable :: forall e. VariableName -> MonadPerspectivesQuery e TripleRef
readQueryVariable var = gets \env -> unsafePartial (fromJust (lookup var env))
-----------------------------------------------------------
-- MONADPERSPECTIVESQUERYCOMPILER
-----------------------------------------------------------
-- | The QueryCompilerEnvironment contains the domain of the queryStep. It also holds
-- | an array of variables that have been declared.
type Domain = String
type Range = String
type VariableName = String

type QueryCompilerEnvironment =
  { domain :: Domain
  , declaredVariables :: StrMap ContextID
  }

type MonadPerspectivesQueryCompiler e = StateT QueryCompilerEnvironment (MonadPerspectivesQuery e)

runMonadPerspectivesQueryCompiler :: forall e a.
  ContextID
  -> (MonadPerspectivesQueryCompiler e a)
  -> MonadPerspectivesQuery e a
runMonadPerspectivesQueryCompiler domainId a = evalStateT a { domain: domainId, declaredVariables: empty}

putQueryStepDomain :: forall e. Domain -> MonadPerspectivesQueryCompiler e Unit
putQueryStepDomain d = modify \env -> env { domain = d}

getQueryStepDomain :: forall e. MonadPerspectivesQueryCompiler e Domain
getQueryStepDomain = gets \{domain} -> domain

putQueryVariableType :: forall e. VariableName -> ContextID -> MonadPerspectivesQueryCompiler e Unit
putQueryVariableType var typeId = modify \s@{declaredVariables} -> s { declaredVariables = insert var typeId declaredVariables }

getQueryVariableType :: forall e. VariableName -> MonadPerspectivesQueryCompiler e (Maybe String)
getQueryVariableType var = gets \{declaredVariables} -> lookup var declaredVariables

withQueryCompilerEnvironment :: forall e a. MonadPerspectivesQueryCompiler e a -> MonadPerspectivesQueryCompiler e a
withQueryCompilerEnvironment a = do
  env <- get
  result <- a
  put env
  pure result

-----------------------------------------------------------
-- OBJECT(S)GETTER
-----------------------------------------------------------
type ObjectsGetter e = ID -> MonadPerspectives (AjaxAvarCache e) (Array Value)

type ObjectGetter e = ID -> MonadPerspectives (AjaxAvarCache e) String

-----------------------------------------------------------
-- TRIPLEGETTER
-----------------------------------------------------------
type TripleGetter e = Subject -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple e)

type Name = String
data NamedFunction f = NamedFunction Name f

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

tripleObjects :: forall e. Triple e -> Array String
tripleObjects (Triple{object}) = object

tripleObjects_  :: forall e m. Monad m => (Triple e) -> m (Array ID)
tripleObjects_ (Triple{object}) = pure object

-- Run the TypedTripleGetter in a QueryEnvironment that has Subject as the value of "#start".
runTypedTripleGetter :: forall e.
  TypedTripleGetter e
  -> Subject
  -> (MonadPerspectives (AjaxAvarCache e)) (Triple e)
runTypedTripleGetter (TypedTripleGetter _ f) a = runMonadPerspectivesQuery a f

runQuery :: forall e.
  Subject
  -> TypedTripleGetter e
  -> (MonadPerspectives (AjaxAvarCache e)) (Triple e)
runQuery = (flip runTypedTripleGetter)

infix 0 runQuery as ##

tripleGetter2function :: forall e. TypedTripleGetter e -> ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Maybe String)
tripleGetter2function (TypedTripleGetter name tg)= tg >=> tripleObjects_ >=> (pure <<< head)

tripleGetter2function' :: forall e. TypedTripleGetter e -> ID -> MonadPerspectivesQuery (AjaxAvarCache e) String
tripleGetter2function' (TypedTripleGetter name tg)= tg >=> tripleObjects_ >=> (pure <<< head) >=> (onNothing' (error ("No result for " <> name)))

-----------------------------------------------------------
-- TYPED GETTERS
-----------------------------------------------------------

data TypedTripleGetter e = TypedTripleGetter Name (TripleGetter e)

typedTripleGetterName :: forall e. TypedTripleGetter e -> String
typedTripleGetterName (TypedTripleGetter n _) = n

applyTypedTripleGetter :: forall e.
  Subject
  -> TypedTripleGetter e
  -> (MonadPerspectivesQuery (AjaxAvarCache e)) (Triple e)
applyTypedTripleGetter a (TypedTripleGetter _ f) = f a

infix 0 applyTypedTripleGetter as @@

-----------------------------------------------------------
-- TRIPLEREF
-----------------------------------------------------------
newtype TripleRef = TripleRef { subject :: Subject, predicate :: Predicate}

instance eqTripleRef :: Eq TripleRef where
  eq (TripleRef({subject: s1, predicate: p1})) (TripleRef({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

instance showTripleRef :: Show TripleRef where
  show (TripleRef {subject, predicate}) = "{" <> show subject <> ", " <> show predicate <> "}"

-----------------------------------------------------------
-- TYPE CHECKING
-----------------------------------------------------------
type Aspect = String
type TypeID = String

data UserMessage =
    MissingVariableDeclaration String
  | VariableAlreadyDeclaredAs VariableName TypeID
  | MissingAspect TypeID Aspect
  | MissingMogelijkeBinding TypeID
  | MultipleDefinitions (Array TypeID)
  | MissingUnqualifiedProperty LocalName RolName
  | MissingQualifiedProperty PropertyName RolName
  | MissingQualifiedRol RolName ContextID
  | MissingUnqualifiedRol RolName ContextID
  | MissingType ContextID
  | MissingRolInstance RolName ContextID

type FD = Either UserMessage ID
