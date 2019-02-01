module Perspectives.CoreTypes where

import Perspectives.EntiteitAndRDFAliases

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, evalStateT, gets, modify, get, put)
import Data.Array (head)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Encode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.StrMap (StrMap, empty, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CouchdbState (CouchdbState)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.Identifiers (LocalName)
import Perspectives.PerspectivesTypesInPurescript (class Object, class Predicate, class Subject, Context)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Perspectives.TypesForDeltas (Delta, encodeDefault)
import Prelude (class Eq, class Monad, class Show, Unit, bind, discard, pure, show, (&&), (<<<), (<>), (==), (>>=), ($))

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextDefinitions = GLStrMap (AVar (PerspectContext String))
type RolDefinitions = GLStrMap (AVar (PerspectRol String String))
type DomeinCache = GLStrMap (AVar DomeinFile)
type QueryCache s p o e = GLStrMap (TypedTripleGetter s p o e )

type PerspectivesState = CouchdbState (
  rolDefinitions :: RolDefinitions
  , contextDefinitions :: ContextDefinitions
  , domeinCache :: DomeinCache
  , memorizeQueryResults :: Boolean
  , transactie :: Transactie
  , tripleQueue :: TripleQueue
  )

-----------------------------------------------------------
-- MONADPERSPECTIVES
-----------------------------------------------------------
-- | MonadPerspectives is an instance of MonadAff.
-- | So, with liftAff we lift an operation in Aff to MonadPerspectives.
type MonadPerspectives  e = ReaderT (AVar (PerspectivesState )) (Aff e)

type MP  e = MonadPerspectives  (AjaxAvarCache e)
-----------------------------------------------------------
-- MONADPERSPECTIVESQUERY
-----------------------------------------------------------
-- | The QueryEnvironment is a set of bindings of variableNames (Strings) to references to Triples.
type QueryEnvironment = StrMap TripleRef

type MonadPerspectivesQuery  e =  StateT QueryEnvironment (MonadPerspectives  e)

type MonadPerspectivesObjects  e = MonadPerspectives  e (Array ID)

putQueryVariable :: forall  e. VariableName -> TripleRef -> MonadPerspectivesQuery  e Unit
putQueryVariable var t = modify \env -> insert var t env

readQueryVariable :: forall  e. VariableName -> MonadPerspectivesQuery  e TripleRef
readQueryVariable var = gets \env -> unsafePartial (fromJust (lookup var env))
-----------------------------------------------------------
-- MONADPERSPECTIVESQUERYCOMPILER
-----------------------------------------------------------
-- | The QueryCompilerEnvironment contains the domain of the queryStep. It also holds
-- | an array of variables that have been declared.
type Domain = Context
type Range = String
type VariableName = String

type QueryCompilerEnvironment =
  { domain :: Domain
  , declaredVariables :: StrMap Context
  }

type MonadPerspectivesQueryCompiler  e = StateT QueryCompilerEnvironment (MonadPerspectives  e)

runMonadPerspectivesQueryCompiler :: forall s p o e a.
  Context
  -> (MonadPerspectivesQueryCompiler  e) a
  -> (MonadPerspectives  e) a
runMonadPerspectivesQueryCompiler domainId a = evalStateT a { domain: domainId, declaredVariables: empty}

putQueryStepDomain :: forall s p o e. Domain -> MonadPerspectivesQueryCompiler  e Unit
putQueryStepDomain d = modify \env -> env { domain = d}

getQueryStepDomain :: forall s p o e. MonadPerspectivesQueryCompiler  e Domain
getQueryStepDomain = gets \{domain} -> domain

putQueryVariableType :: forall s p o e. VariableName -> Context -> MonadPerspectivesQueryCompiler  e Unit
putQueryVariableType var typeId = modify \s@{declaredVariables} -> s { declaredVariables = insert var typeId declaredVariables }

getQueryVariableType :: forall s p o e. VariableName -> MonadPerspectivesQueryCompiler  e (Maybe Context)
getQueryVariableType var = gets \{declaredVariables} -> lookup var declaredVariables

withQueryCompilerEnvironment :: forall s p o e a. MonadPerspectivesQueryCompiler  e a -> MonadPerspectivesQueryCompiler  e a
withQueryCompilerEnvironment a = do
  env <- get
  result <- a
  put env
  pure result

-----------------------------------------------------------
-- OBJECT(S)GETTER
-----------------------------------------------------------
-- | Usage: contextType :: forall e. (Context ~~> ContextDef) e
type ObjectsGetter s o  e = s -> MonadPerspectives  (AjaxAvarCache e) (Array o)

type ObjectGetter s o  e = s -> MonadPerspectives  (AjaxAvarCache e) o

infixr 4 type ObjectsGetter as ~~>

applyObjectsGetter :: forall s o  e. s -> ObjectsGetter s o  e -> MonadPerspectives  (AjaxAvarCache e) (Array o)
applyObjectsGetter id g = g id

infix 0 applyObjectsGetter as %%
infix 0 applyObjectsGetter as %%=

applyObjectsGetterToMaybeObject :: forall s o  e. s -> ObjectsGetter s o  e -> MonadPerspectives  (AjaxAvarCache e) (Maybe o)
applyObjectsGetterToMaybeObject id g = g id >>= pure <<< head

infix 0 applyObjectsGetterToMaybeObject as %%>

applyObjectGetterToObject :: forall s o  e. Show s => s -> ObjectsGetter s o  e -> MonadPerspectives  (AjaxAvarCache e) o
applyObjectGetterToObject id g = g id >>= \objs ->
  case head objs of
    Nothing -> throwError $ error $ "ObjectsGetter returns no values for '" <> show id <> "'."
    (Just obj) -> pure obj

infix 0 applyObjectGetterToObject as %%>>

-----------------------------------------------------------
-- NAMEDFUNCTION
-----------------------------------------------------------
type Name = String
data NamedFunction f = NamedFunction Name f

-----------------------------------------------------------
-- TRIPLE
-----------------------------------------------------------
-- | The type parameter s of Triple should be constrained by Class Subject in every function.
-- | The type parameter p of Triple should be constrained by Class Predicate in every function.
-- | The type parameter o of Triple should be constrained by Class Object in every function.
newtype Triple s p o e = Triple
  { subject :: s
  , predicate :: p
  , object :: Array o
  , dependencies :: Array TripleRef
  , supports :: Array TripleRef
  , tripleGetter :: TripleGetter s p o e}

instance showTriple :: (Subject s, Predicate p, Object o) => Show (Triple s p o e) where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: (Eq s, Eq p) => Eq (Triple s p o e) where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

tripleObjects :: forall s p o e. Triple s p o e -> Array o
tripleObjects (Triple{object}) = object

tripleObjects_  :: forall s p o e m. Monad m => (Triple s p o e) -> m (Array o)
tripleObjects_ (Triple{object}) = pure object

mtripleObject :: forall s p o e. Triple s p o e -> Maybe o
mtripleObject = head <<< tripleObjects

tripleObject :: forall s p o e. Triple s p o e -> o
tripleObject (Triple{object}) = unsafePartial (fromJust (head object))

-----------------------------------------------------------
-- TRIPLEQUEUE
-----------------------------------------------------------
type TripleQueue = Array TripleQueueElement

newtype TripleQueueElement = TripleQueueElement { subject :: Subject, predicate :: Predicate, dependencies :: Array TripleRef}

instance eqTripleQueueElement :: Eq TripleQueueElement where
  eq (TripleQueueElement({subject: s1, predicate: p1})) (TripleQueueElement({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

-----------------------------------------------------------
-- TRIPLEGETTER
-----------------------------------------------------------
type TripleGetter s p o e = s -> MonadPerspectivesQuery  (AjaxAvarCache e) (Triple s p o e)

-----------------------------------------------------------
-- TYPED GETTERS
-----------------------------------------------------------

data TypedTripleGetter s p o e = TypedTripleGetter Name (TripleGetter s p o e)

typedTripleGetterName :: forall s p o e. TypedTripleGetter s p o e -> String
typedTripleGetterName (TypedTripleGetter n _) = n

-- | NB. If the TripleGetter uses the #start queryvariable, this will not work, because
-- | it will only be bound in runMonadPerspectivesQuery.
applyTypedTripleGetter :: forall s p o e.
  s
  -> TypedTripleGetter s p o e
  -> (MonadPerspectivesQuery  (AjaxAvarCache e)) (Triple s p o e)
applyTypedTripleGetter a (TypedTripleGetter _ f) = f a

infix 0 applyTypedTripleGetter as @@

applyTypedTripleGetterToObjects :: forall s p o e.
  s
  -> TypedTripleGetter s p o e
  -> (MonadPerspectivesQuery  (AjaxAvarCache e)) (Array o)
applyTypedTripleGetterToObjects a (TypedTripleGetter _ f) = f a >>= pure <<< tripleObjects

infix 0 applyTypedTripleGetterToObjects as @@=

applyTypedTripleGetterToMaybeObject :: forall s p o e.
  s
  -> TypedTripleGetter s p o e
  -> (MonadPerspectivesQuery  (AjaxAvarCache e)) (Maybe o)
applyTypedTripleGetterToMaybeObject a (TypedTripleGetter _ f) = f a >>= pure <<< head <<< tripleObjects

infix 0 applyTypedTripleGetterToMaybeObject as @@>

applyTypedTripleGetterToObject :: forall s p o e. Show s =>
  s
  -> TypedTripleGetter s p o e
  -> (MonadPerspectivesQuery (AjaxAvarCache e)) o
applyTypedTripleGetterToObject a (TypedTripleGetter n f) = f a >>= \(Triple{object}) ->
  case head object of
    Nothing -> throwError $ error $ "TypedTripleGetter '" <> n <> "' returns no values for '" <> show a <> "'."
    (Just obj) -> pure obj

infix 0 applyTypedTripleGetterToObject as @@>>

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
  | MultipleDefinitions LocalName (Array TypeID)
  | MissingUnqualifiedProperty LocalName RolName
  | MissingQualifiedProperty PropertyName RolName
  | MissingQualifiedRol RolName ContextID
  | MissingUnqualifiedRol RolName ContextID
  | MissingType ContextID
  | MissingRolInstance RolName ContextID
  | IncorrectBinding ContextID RolName TypeID TypeID TypeID
  | RolNotDefined RolName ContextID TypeID
  | MissingPropertyValue ContextID PropertyName RolName
  | IncorrectPropertyValue ContextID PropertyName TypeID String
  | TooManyPropertyValues ContextID PropertyName
  | PropertyNotDefined ContextID PropertyName RolID RolName
  | AspectRolNotFromAspect RolName RolName ContextID
  | CycleInAspects ContextID (Array TypeID)
  | RolWithoutContext RolName
  | ContextExists ID
  | NotAValidIdentifier String

type FD = Either UserMessage ID

instance showUserMessage :: Show UserMessage where
  show (MissingVariableDeclaration s) = "(MissingVariableDeclaration) De variabele '" <> s <> "' wordt gebruikt voor hij is gedefinieerd."
  show (VariableAlreadyDeclaredAs var tp) = "(VariableAlreadyDeclaredAs) De variabele '" <> var <> "' is al gedeclareerd als een waarde van het type '" <> tp <> "'"
  show (MissingAspect tp as) = "(MissingAspect) Het type '" <> tp <> "' mist het aspect '" <> as <> "'."
  show (MissingMogelijkeBinding tp) = "(MissingMogelijkeBinding) Voor Rol '" <> tp <> "' is geen mogelijkeBinding gedefinieerd."
  show (MultipleDefinitions ln aspectArray) = "(MultipleDefinitions) Elk van de volgende Aspecten heeft een definitie voor '" <> ln <> "': " <> show aspectArray
  show (MissingUnqualifiedProperty ln rn) = "(MissingUnqualifiedProperty) Er is geen definitie voor de property '" <> ln <> "' voor de rol '" <> rn <> "'."
  show (MissingQualifiedProperty pn rn) = "(MissingQualifiedProperty) Er is geen definitie voor de property '" <> pn <> "' voor de rol '" <> rn <> "'."
  show (MissingQualifiedRol rn cid) = "(MissingQualifiedRol) Er is geen definitie voor de rol '" <> rn <> "' in de context '" <> cid <> "'."
  show (MissingUnqualifiedRol rn cid) = "(MissingUnqualifiedRol) Er is geen definitie voor de rol '" <> rn <> "' in de context '" <> cid <> "'."
  show (MissingType cid) = "(MissingType) De context '" <> cid <> "' heeft geen type."
  show (MissingRolInstance rn cid) = "(MissingRolInstance) De verplichte Rol '" <> rn <> "' komt niet voor in de context '" <> cid <> "'."
  show (IncorrectBinding cid rn bd tp mb) = "(IncorrectBinding) In de context '" <> cid <> "' is de Rol '" <> rn <> "' gebonden aan '" <> bd <> "'(type: '" <> tp <> "') maar moet worden gebonden aan een instantie van type '" <> mb <> "'."
  show (RolNotDefined rn cid tp) = "(RolNotDefined) De context '" <> cid <> "' heeft een instantie van rol '" <> rn <> "' maar die is niet gedefinieerd voor '" <> tp <> "'."
  show (MissingPropertyValue cid pn rid) = "(MissingPropertyValue) De verplichte Property '" <> pn <> "' komt niet voor in de rol '" <> rid <> "' van de context '" <> cid <> "'."
  show (IncorrectPropertyValue cid pn sv val) = "(IncorrectPropertyValue) De Property '" <> pn <> "' is gebonden aan de waarde '" <> val <> "' maar moet worden gebonden aan een waarde van type '" <> sv <> "' (in de context '" <> cid <> "')."
  show (TooManyPropertyValues cid pn) = "(TooManyPropertyValues) De Property '" <> pn <> "' is functioneel maar heeft méér dan 1 waarde (in de context '" <> cid <> "')."
  show (PropertyNotDefined cid pn rid rn) = "(PropertyNotDefined) De Rol '" <> rid <> "' van de context '" <> cid <> "' geeft een waarde aan Property '" <> pn <> "' maar die is niet gedefinieerd voor '" <> rn <> "'."
  show (AspectRolNotFromAspect rn arn cid) = "(AspectRolNotFromAspect) De Rol '" <> rn <> "' gebruikt de Rol '" <> arn <> "' als aspectrol, maar die is niet beschikbaar in de Aspecten van '" <> cid <> "'."
  show (CycleInAspects cid asps) = "(CycleInAspects) De Context '" <> cid <> "' heeft een Aspect dat (indirect) weer '" <> cid <> "' als Aspect heeft. De betrokken Aspecten zijn: " <> show asps <> "."
  show (RolWithoutContext cid) = "(RolWithoutContext) De Rol '" <> cid <> "' heeft geen definiërende Context."
  -- show _ = "This is a usermessage"
  show (ContextExists id) = "(ContextExists) De Context: '" <> id <> "' bestaat al."
  show (NotAValidIdentifier id) =  "(NotAValidIdentifier) De string '" <> id <> "' is geen geldige identifier."

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transactie = Transactie
  { author :: String
  , timeStamp :: SerializableDateTime
  , deltas :: Array Delta
  , createdContexts :: Array (PerspectContext String)
  , createdRoles :: Array (PerspectRol String String)
  , deletedContexts :: Array Context
  , deletedRoles :: Array (PerspectRol String String)
  , changedDomeinFiles :: Array ID
  }

derive instance genericRepTransactie :: Generic (Transactie ) _

instance showTransactie :: (Show c, Show r, Show b) => Show (Transactie ) where
  show = genericShow

instance encodeTransactie :: (Encode c, Encode r, Encode b) => Encode (Transactie ) where
  encode = encodeDefault

createTransactie :: forall  e. String -> Aff (now :: NOW | e) (Transactie )
createTransactie author =
  do
    n <- liftEff $ now
    pure $ Transactie{ author: author, timeStamp: SerializableDateTime (toDateTime n), deltas: [], createdContexts: [], createdRoles: [], deletedContexts: [], deletedRoles: [], changedDomeinFiles: []}

transactieID :: Transactie -> String
transactieID (Transactie{author, timeStamp}) = author <> "_" <> show timeStamp

-----------------------------------------------------------
-- DATETIME
-- We need a newtype for DateTime in order to be able to serialize and show it.
-----------------------------------------------------------
newtype SerializableDateTime = SerializableDateTime DateTime

instance encodeSerializableDateTime :: Encode SerializableDateTime where
  encode d = toForeign $ show d

instance showSerializableDateTime :: Show SerializableDateTime where
  show (SerializableDateTime d) = "todo"

-- instance showSerializableDateTime :: Show SerializableDateTime where
--   show (SerializableDateTime d) = runPure (catchException handleError (toISOString (fromDateTime d)))
--
-- handleError :: forall eff. Error -> Eff eff String
-- handleError e = pure "Could not serialize DateTime"
