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
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Perspectives.TypesForDeltas (Delta, encodeDefault)
import Prelude (class Eq, class Monad, class Show, Unit, bind, discard, pure, show, (&&), (<<<), (<>), (==), (>>=), ($))
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextDefinitions = GLStrMap (AVar PerspectContext)
type RolDefinitions = GLStrMap (AVar PerspectRol)
type DomeinCache = GLStrMap (AVar DomeinFile)
type QueryCache e = GLStrMap (TypedTripleGetter String String e)

type PerspectivesState = CouchdbState (
  rolDefinitions :: RolDefinitions
  , contextDefinitions :: ContextDefinitions
  , domeinCache :: DomeinCache
  , memorizeQueryResults :: Boolean
  , transactie :: Transactie
  , tripleQueue :: TripleQueue
  , recomputed :: Array TripleRef
  )

-----------------------------------------------------------
-- MONADPERSPECTIVES
-----------------------------------------------------------
-- | MonadPerspectives is an instance of MonadAff.
-- | So, with liftAff we lift an operation in Aff to MonadPerspectives.
type MonadPerspectives e = ReaderT (AVar PerspectivesState) (Aff e)

type MP e = MonadPerspectives (AjaxAvarCache e)
-----------------------------------------------------------
-- MONADPERSPECTIVESQUERY
-----------------------------------------------------------
-- | The QueryEnvironment is a set of bindings of variableNames (Strings) to references to Triples.
type QueryEnvironment = StrMap TripleRef

type MonadPerspectivesQuery e =  StateT QueryEnvironment (MonadPerspectives e)

type MPQ e = MonadPerspectivesQuery (AjaxAvarCache e)

type MonadPerspectivesObjects e = MonadPerspectives e (Array ID)

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

type MonadPerspectivesQueryCompiler e = StateT QueryCompilerEnvironment (MonadPerspectives e)

runMonadPerspectivesQueryCompiler :: forall e a.
  ContextID
  -> (MonadPerspectivesQueryCompiler e a)
  -> MonadPerspectives e a
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
-- TODO: FASEER UIT.
type ObjectsGetter e = ID -> MonadPerspectives (AjaxAvarCache e) (Array Value)

type ObjectGetter e = ID -> MonadPerspectives (AjaxAvarCache e) String

applyObjectsGetter :: forall e. ID -> ObjectsGetter e -> MonadPerspectives (AjaxAvarCache e) (Array Value)
applyObjectsGetter id g = g id

infix 0 applyObjectsGetter as %%
infix 0 applyObjectsGetter as %%=

applyObjectsGetterToMaybeObject :: forall e. ID -> ObjectsGetter e -> MonadPerspectives (AjaxAvarCache e) (Maybe Value)
applyObjectsGetterToMaybeObject id g = g id >>= pure <<< head

infix 0 applyObjectsGetterToMaybeObject as %%>

applyObjectsGetterToObject :: forall s o e. s -> (s ~~> o) e -> MonadPerspectives (AjaxAvarCache e) o
applyObjectsGetterToObject id g = g id >>= \objs ->
  case head objs of
    Nothing -> throwError $ error $ "applyObjectsGetterToObject: no values for '" <> (unsafeCoerce id) <> "'."
    (Just obj) -> pure obj

infix 0 applyObjectsGetterToObject as %%>>

-----------------------------------------------------------
-- TYPEDOBJECT(S)GETTER
-----------------------------------------------------------
type TypedObjectsGetter s o e = s -> MonadPerspectives (AjaxAvarCache e) (Array o)

infixl 5 type TypedObjectsGetter as ~~>

type TypedObjectGetter s o e = s -> MonadPerspectives (AjaxAvarCache e) o

-- | -- | Apply (s ~~> o) e to s to get an Array of o, possibly empty.
applyTypedObjectsGetter :: forall s o e. s -> (s ~~> o) e -> MonadPerspectives (AjaxAvarCache e) (Array o)
applyTypedObjectsGetter id g = g id

infix 0 applyTypedObjectsGetter as ##
infix 0 applyTypedObjectsGetter as ##=

-- | Apply (s ~~> o) e to s to get (a single) o wrapped in Just, Nothing otherwise.
applyTypedObjectsGetterToMaybeObject :: forall s o e. s -> (s ~~> o) e -> MonadPerspectives (AjaxAvarCache e) (Maybe o)
applyTypedObjectsGetterToMaybeObject id g = g id >>= pure <<< head

infix 0 applyTypedObjectsGetterToMaybeObject as ##>

-- | Apply (s ~~> o) e to s to get (a single) o. Throws an error of no o is available.
applyTypedObjectsGetterToObject :: forall s o e. s -> (s ~~> o) e -> MonadPerspectives (AjaxAvarCache e) o
applyTypedObjectsGetterToObject id g = g id >>= \objs ->
  case head objs of
    Nothing -> throwError $ error $ "applyTypedObjectsGetterToObject: no values for '" <> unsafeCoerce id <> "'."
    (Just obj) -> pure obj

infix 0 applyTypedObjectsGetterToObject as ##>>

-----------------------------------------------------------
-- NAMEDFUNCTION
-----------------------------------------------------------
type Name = String
data NamedFunction f = NamedFunction Name f

-----------------------------------------------------------
-- TRIPLE
-----------------------------------------------------------
newtype Triple s o e = Triple
  { subject :: s
  , predicate :: Predicate
  , object :: Array o
  , dependencies :: Array TripleRef
  , supports :: Array TripleRef
  , tripleGetter :: TripleGetter s o e}

instance showTriple :: (Show s, Show o) => Show (Triple s o e) where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: (Eq s, Eq o) => Eq (Triple s o e) where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

tripleObjects :: forall s o e. Triple s o e -> Array o
tripleObjects (Triple{object}) = object

tripleObjects_  :: forall s o e m. Monad m => (Triple s o e) -> m (Array o)
tripleObjects_ (Triple{object}) = pure object

mtripleObject :: forall s o e. Triple s o e -> Maybe o
mtripleObject = head <<< tripleObjects

tripleObject :: forall s o e. Triple s o e -> o
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
type TripleGetter s o e = s -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple s o e)

-----------------------------------------------------------
-- TYPED GETTERS
-----------------------------------------------------------

data TypedTripleGetter s o e = TypedTripleGetter Name (TripleGetter s o e)

infixl 5 type TypedTripleGetter as **>

typedTripleGetterName :: forall s o e. TypedTripleGetter s o e -> String
typedTripleGetterName (TypedTripleGetter n _) = n

-- | NB. If the TripleGetter uses the #start queryvariable, this will not work, because
-- | it will only be bound in runMonadPerspectivesQuery.
applyTypedTripleGetter :: forall s o e.
  s
  -> TypedTripleGetter s o e
  -> (MonadPerspectivesQuery (AjaxAvarCache e)) (Triple s o e)
applyTypedTripleGetter a (TypedTripleGetter _ f) = f a

infix 0 applyTypedTripleGetter as @@

applyTypedTripleGetterToObjects :: forall s o e.
  s
  -> TypedTripleGetter s o e
  -> (MonadPerspectivesQuery (AjaxAvarCache e)) (Array o)
applyTypedTripleGetterToObjects a (TypedTripleGetter _ f) = f a >>= pure <<< tripleObjects

infix 0 applyTypedTripleGetterToObjects as @@=

applyTypedTripleGetterToMaybeObject :: forall s o e.
  s
  -> TypedTripleGetter s o e
  -> (MonadPerspectivesQuery (AjaxAvarCache e)) (Maybe o)
applyTypedTripleGetterToMaybeObject a (TypedTripleGetter _ f) = f a >>= pure <<< head <<< tripleObjects

infix 0 applyTypedTripleGetterToMaybeObject as @@>

applyTypedTripleGetterToObject :: forall s o e.
  Show s =>
  s
  -> TypedTripleGetter s o e
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
type SimpleValueName = String

data UserMessage =
  -- TypeDefChecker messages
    MissingAspect TypeID Aspect
  | MissingMogelijkeBinding TypeID
  | MissingType ContextID
  | MissingRolInstance RolName ContextID
  | IncorrectRolinContextBinding ContextID RolName TypeID TypeID TypeID
  | IncorrectContextRolBinding ContextID RolName TypeID TypeID TypeID
  | RolNotDefined RolName ContextID TypeID
  | MissingPropertyValue ContextID PropertyName RolName
  | MissingExternalPropertyValue PropertyName ContextID
  | MissingInternalPropertyValue PropertyName ContextID
  | IncorrectPropertyValue ContextID PropertyName TypeID String
  | TooManyPropertyValues ContextID PropertyName
  | PropertyNotDefined ContextID PropertyName RolID RolName
  | AspectRolNotFromAspect RolName RolName ContextID
  | AspectPropertyNotFromAspectRol PropertyName PropertyName RolName
  | CycleInAspects ContextID (Array TypeID)
  | CycleInAspectRoles RolName (Array TypeID)
  | CycleInAspectProperties PropertyName (Array TypeID)
  | RolWithoutContext RolName
  | PropertyWithoutRol PropertyName
  | CannotOverrideBooleanAspectProperty PropertyName PropertyName
  | BindingPropertyCannotOverrideBooleanAspectProperty PropertyName PropertyName PropertyName
  | CannotOverideBooleanRolProperty RolName PropertyName
  | MissingRange PropertyName
  | RangeNotSubsumed SimpleValueName PropertyName SimpleValueName PropertyName
  | RangeNotSubsumedByBindingProperty PropertyName SimpleValueName PropertyName SimpleValueName PropertyName
  | MogelijkeBindingNotSubsumed String RolName String RolName
  | MissingAspectPropertyForBindingProperty PropertyName PropertyName
  | BindingPropertyNotAvailable PropertyName PropertyName
  | IncompatiblePrototype ContextID ContextID ContextID

  -- Other messages
  | MultipleDefinitions LocalName (Array TypeID)
  | MissingVariableDeclaration String
  | VariableAlreadyDeclaredAs VariableName TypeID
  | MissingUnqualifiedProperty LocalName RolName
  | MissingQualifiedProperty PropertyName RolName
  | MissingQualifiedRol RolName ContextID
  | MissingUnqualifiedRol RolName ContextID
  | ContextExists ID
  | NotAValidIdentifier String
  | NotWellFormedContextSerialization String

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
  show (IncorrectRolinContextBinding cid rn bd tp mb) = "(IncorrectRolinContextBinding) In de context '" <> cid <> "' is de RolInContext '" <> rn <> "' gebonden aan '" <> bd <> "'(type: '" <> tp <> "') maar moet worden gebonden aan een instantie van (één van de) type(s) '" <> mb <> "'."
  show (IncorrectContextRolBinding cid rn bd tp mb) = "(IncorrectContextRolBinding) In de context '" <> cid <> "' is de ContextRol '" <> rn <> "' gebonden aan '" <> bd <> "'(type: '" <> tp <> "') maar moet worden gebonden aan een instantie van (één van de) type(s) '" <> mb <> "'."
  show (RolNotDefined rn cid tp) = "(RolNotDefined) De context '" <> cid <> "' heeft een instantie van rol '" <> rn <> "' maar die is niet gedefinieerd voor '" <> tp <> "'."
  show (MissingPropertyValue cid pn rid) = "(MissingPropertyValue) De verplichte Property '" <> pn <> "' komt niet voor in de rol '" <> rid <> "' van de context '" <> cid <> "'."
  show (MissingExternalPropertyValue pn cid) = "(MissingExternalPropertyValue) De verplichte externe Property '" <> pn <> "' komt niet voor in de context '" <> cid <> "'."
  show (MissingInternalPropertyValue pn cid) = "(MissingInternalPropertyValue) De verplichte interne Property '" <> pn <> "' komt niet voor in de context '" <> cid <> "'."
  show (IncorrectPropertyValue cid pn sv val) = "(IncorrectPropertyValue) De Property '" <> pn <> "' is gebonden aan de waarde '" <> val <> "' maar moet worden gebonden aan een waarde van type '" <> sv <> "' (in de context '" <> cid <> "')."
  show (TooManyPropertyValues cid pn) = "(TooManyPropertyValues) De Property '" <> pn <> "' is functioneel maar heeft méér dan 1 waarde (in de context '" <> cid <> "')."
  show (PropertyNotDefined cid pn rid rn) = "(PropertyNotDefined) De Rol '" <> rid <> "' van de context '" <> cid <> "' geeft een waarde aan Property '" <> pn <> "' maar die is niet gedefinieerd voor '" <> rn <> "'."
  show (AspectRolNotFromAspect rn arn cid) = "(AspectRolNotFromAspect) De Rol '" <> rn <> "' gebruikt de Rol '" <> arn <> "' als aspectrol, maar die is niet beschikbaar in de Aspecten van '" <> cid <> "'."
  show (AspectPropertyNotFromAspectRol pn apn rid) = "(AspectPropertyNotFromAspectRol) De Property '" <> pn <> "' gebruikt de Property '" <> apn <> "' als aspectproperty, maar die is niet beschikbaar in de AspectenRollen van '" <> rid <> "'."
  show (CycleInAspects cid asps) = "(CycleInAspects) De Context '" <> cid <> "' heeft een Aspect dat (indirect) weer '" <> cid <> "' als Aspect heeft. De betrokken Aspecten zijn: " <> show asps <> "."
  show (CycleInAspectRoles cid asps) = "(CycleInAspectRoles) De Rol '" <> cid <> "' heeft een AspectRol die (indirect) weer '" <> cid <> "' als AspectRol heeft. De betrokken AspectRollen zijn: " <> show asps <> "."
  show (CycleInAspectProperties cid asps) = "(CycleInAspectProperties) De Property '" <> cid <> "' heeft een AspectProperty die (indirect) weer '" <> cid <> "' als AspectProperty heeft. De betrokken AspectProperties zijn: " <> show asps <> "."
  show (RolWithoutContext cid) = "(RolWithoutContext) De Rol-definitie '" <> cid <> "' heeft geen definiërende Context."
  show (PropertyWithoutRol pid) = "(PropertyWithoutRol) De Property-definitie '" <> pid <> "' heeft geen definiërende Rol."
  -- show _ = "This is a usermessage"
  show (ContextExists id) = "(ContextExists) De Context: '" <> id <> "' bestaat al."
  show (NotAValidIdentifier id) =  "(NotAValidIdentifier) De string '" <> id <> "' is geen geldige identifier."
  show (NotWellFormedContextSerialization m) = "(NotWellFormedContextSerialization) De string '" <> m <> "' is geen geldige ContextSerialization."
  show (CannotOverrideBooleanAspectProperty pn pp) = "(CannotOverrideBooleanAspectProperty) Er is een aspect van property '" <> pn <> "' dat aan '" <> pp <> "' al de waarde 'true' heeft gegeven ()."
  show (BindingPropertyCannotOverrideBooleanAspectProperty bp pn pp) = "(BindingPropertyCannotOverrideBooleanAspectProperty) Er is een aspect van property '" <> pn <> "' dat aan '" <> pp <> "' al de waarde 'true' heeft gegeven (deze property wordt als BindingProperty aan die AspectProperty gebonden in de property '" <> bp <> "')."
  show (CannotOverideBooleanRolProperty rn pp) = "(CannotOverideBooleanRolProperty) Er is een aspect van rol '" <> rn <> "' dat aan '" <> pp <> "' al de waarde 'true' heeft gegeven ()."
  show (MissingRange pn) = "(MissingRange) Propery '" <> pn <> "' has not been given a range."
  show (RangeNotSubsumed ownRange aspect aspectRange property) = "(RangeNotSubsumed) De range '" <> aspectRange <> "' van de AspectProperty '" <> aspect <> "' is geen aspect van de range '" <> ownRange <> "' van de property '" <> property <> "'!"
  show (RangeNotSubsumedByBindingProperty property ownRange aspect aspectRange bindingprop) = "(RangeNotSubsumedByBindingProperty) De range '" <> aspectRange <> "' van de AspectProperty '" <> aspect <> "' is geen aspect van de range '" <> ownRange <> "' van de BindingProperty '" <> bindingprop <> " (de BindingProperty wordt gebonden aan de AspectProperty in de property '" <> property <> "')'!"
  show (MogelijkeBindingNotSubsumed ownBinding aspect aspectBinding rol) = "(MogelijkeBindingNotSubsumed) De mogelijke binding '" <> aspectBinding <> "' van de AspectRol '" <> aspect <> "' is geen aspect van de mogelijke binding '" <> ownBinding <> "' van de rol '" <> rol <> "'!"
  show (MissingAspectPropertyForBindingProperty property bindingproperty) = "(MissingAspectPropertyForBindingProperty) De property '" <> property <> "' heeft BindingProperty '" <> bindingproperty <> "' maar geen AspectProperty!"
  show (BindingPropertyNotAvailable pdef bindingproperty) = "(BindingPropertyNotAvailable) Property '" <> pdef <> "' definieert binding property '" <> bindingproperty <> "' maar die is niet beschikbaar in deze definitie!"
  show (IncompatiblePrototype def deftype ptype) = "(IncompatiblePrototype) Definition '" <> def <> "' has type '" <> deftype <> "', but its prototype '" <> ptype <> "' does not have that type!"

-----------------------------------------------------------
-- TRANSACTIE
-----------------------------------------------------------
newtype Transactie = Transactie
  { author :: String
  , timeStamp :: SerializableDateTime
  , deltas :: Array Delta
  , createdContexts :: Array PerspectContext
  , createdRoles :: Array PerspectRol
  , deletedContexts :: Array ID
  , deletedRoles :: Array ID
  , changedDomeinFiles :: Array ID
  }

derive instance genericRepTransactie :: Generic Transactie _

instance showTransactie :: Show Transactie where
  show = genericShow

instance encodeTransactie :: Encode Transactie where
  encode = encodeDefault

createTransactie :: forall e. String -> Aff (now :: NOW | e) Transactie
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
