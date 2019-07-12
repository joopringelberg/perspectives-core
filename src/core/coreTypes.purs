module Perspectives.CoreTypes where

import Perspectives.EntiteitAndRDFAliases

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, evalStateT, gets, modify, get, put)
import Data.Array (head)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)
import Foreign (unsafeToForeign)
import Foreign.Class (class Encode)
import Foreign.Object (Object, empty, insert, lookup) as O
import Partial.Unsafe (unsafePartial)
import Perspectives.CouchdbState (CouchdbState)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.Identifiers (LocalName)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.TypesForDeltas (Delta)
import Prelude (class Eq, class Monad, class Show, Unit, bind, discard, pure, show, void, ($), (&&), (<<<), (<>), (==), (>>=))
import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextDefinitions = GLStrMap (AVar PerspectContext)
type RolDefinitions = GLStrMap (AVar PerspectRol)
type Contexts = GLStrMap (AVar Context)
type EnumeratedRoles = GLStrMap (AVar EnumeratedRole)
type CalculatedRoles = GLStrMap (AVar CalculatedRole)
type DomeinCache = GLStrMap (AVar DomeinFile)
type QueryCache = GLStrMap (TypedTripleGetter String String)

type PerspectivesState = CouchdbState (
  rolDefinitions :: RolDefinitions
  , contextDefinitions :: ContextDefinitions
  -- Perspectives types aanvullen
  , contexts :: Contexts
  , enumeratedRoles :: EnumeratedRoles
  , calculatedRoles :: CalculatedRoles

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
type MonadPerspectives = ReaderT (AVar PerspectivesState) Aff

type MP = MonadPerspectives
-----------------------------------------------------------
-- MONADPERSPECTIVESQUERY
-----------------------------------------------------------
-- | The QueryEnvironment is a set of bindings of variableNames (Strings) to references to Triples.
type QueryEnvironment = O.Object TripleRef

type MonadPerspectivesQuery =  StateT QueryEnvironment MonadPerspectives

type MPQ = MonadPerspectivesQuery

type MonadPerspectivesObjects = MonadPerspectives (Array ID)

putQueryVariable :: VariableName -> TripleRef -> MonadPerspectivesQuery Unit
putQueryVariable var t = void $ modify \env -> O.insert var t env

readQueryVariable :: VariableName -> MonadPerspectivesQuery TripleRef
readQueryVariable var = gets \env -> unsafePartial (fromJust (O.lookup var env))
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
  , declaredVariables :: O.Object ContextID
  }

type MonadPerspectivesQueryCompiler = StateT QueryCompilerEnvironment MonadPerspectives

runMonadPerspectivesQueryCompiler :: forall a.
  ContextID
  -> (MonadPerspectivesQueryCompiler a)
  -> MonadPerspectives a
runMonadPerspectivesQueryCompiler domainId a = evalStateT a { domain: domainId, declaredVariables: O.empty}

putQueryStepDomain :: Domain -> MonadPerspectivesQueryCompiler Unit
putQueryStepDomain d = void $ modify \env -> env { domain = d}

getQueryStepDomain :: MonadPerspectivesQueryCompiler Domain
getQueryStepDomain = gets \{domain} -> domain

putQueryVariableType :: VariableName -> ContextID -> MonadPerspectivesQueryCompiler Unit
putQueryVariableType var typeId = void $ modify \s@{declaredVariables} -> s { declaredVariables = O.insert var typeId declaredVariables }

getQueryVariableType :: VariableName -> MonadPerspectivesQueryCompiler (Maybe String)
getQueryVariableType var = gets \{declaredVariables} -> O.lookup var declaredVariables

withQueryCompilerEnvironment :: forall a. MonadPerspectivesQueryCompiler a -> MonadPerspectivesQueryCompiler a
withQueryCompilerEnvironment a = do
  env <- get
  result <- a
  put env
  pure result

-----------------------------------------------------------
-- OBJECT(S)GETTER
-----------------------------------------------------------
-- TODO: FASEER UIT.
type ObjectsGetter = ID -> MonadPerspectives (Array Value)

type ObjectGetter = ID -> MonadPerspectives String

applyObjectsGetter :: ID -> ObjectsGetter -> MonadPerspectives (Array Value)
applyObjectsGetter id g = g id

infix 0 applyObjectsGetter as %%
infix 0 applyObjectsGetter as %%=

applyObjectsGetterToMaybeObject :: ID -> ObjectsGetter -> MonadPerspectives (Maybe Value)
applyObjectsGetterToMaybeObject id g = g id >>= pure <<< head

infix 0 applyObjectsGetterToMaybeObject as %%>

applyObjectsGetterToObject :: forall s o. s -> (s ~~> o) -> MonadPerspectives o
applyObjectsGetterToObject id g = g id >>= \objs ->
  case head objs of
    Nothing -> throwError $ error $ "applyObjectsGetterToObject: no values for '" <> (unsafeCoerce id) <> "'."
    (Just obj) -> pure obj

infix 0 applyObjectsGetterToObject as %%>>

-----------------------------------------------------------
-- TYPEDOBJECT(S)GETTER
-----------------------------------------------------------
type TypedObjectsGetter s o = s -> MonadPerspectives (Array o)

infixl 5 type TypedObjectsGetter as ~~>

type TypedObjectGetter s o = s -> MonadPerspectives o

-- | -- | Apply (s ~~> o) to s to get an Array of o, possibly O.empty.
applyTypedObjectsGetter :: forall s o. s -> (s ~~> o) -> MonadPerspectives (Array o)
applyTypedObjectsGetter id g = g id

infix 0 applyTypedObjectsGetter as ##
infix 0 applyTypedObjectsGetter as ##=

-- | Apply (s ~~> o) to s to get (a single) o wrapped in Just, Nothing otherwise.
applyTypedObjectsGetterToMaybeObject :: forall s o. s -> (s ~~> o) -> MonadPerspectives (Maybe o)
applyTypedObjectsGetterToMaybeObject id g = g id >>= pure <<< head

infix 0 applyTypedObjectsGetterToMaybeObject as ##>

-- | Apply (s ~~> o) to s to get (a single) o. Throws an error of no o is available.
applyTypedObjectsGetterToObject :: forall s o. s -> (s ~~> o) -> MonadPerspectives o
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
newtype Triple s o = Triple
  { subject :: s
  , predicate :: Predicate
  , object :: Array o
  , dependencies :: Array TripleRef
  , supports :: Array TripleRef
  , tripleGetter :: TripleGetter s o}

instance showTriple :: (Show s, Show o) => Show (Triple s o) where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: (Eq s, Eq o) => Eq (Triple s o) where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

tripleObjects :: forall s o. Triple s o -> Array o
tripleObjects (Triple{object}) = object

tripleObjects_  :: forall s o m. Monad m => (Triple s o) -> m (Array o)
tripleObjects_ (Triple{object}) = pure object

mtripleObject :: forall s o. Triple s o -> Maybe o
mtripleObject = head <<< tripleObjects

tripleObject :: forall s o. Triple s o -> o
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
type TripleGetter s o = s -> MonadPerspectivesQuery (Triple s o)

-----------------------------------------------------------
-- TYPED GETTERS
-----------------------------------------------------------

data TypedTripleGetter s o = TypedTripleGetter Name (TripleGetter s o)

infixl 5 type TypedTripleGetter as **>

typedTripleGetterName :: forall s o. TypedTripleGetter s o -> String
typedTripleGetterName (TypedTripleGetter n _) = n

-- | NB. If the TripleGetter uses the #start queryvariable, this will not work, because
-- | it will only be bound in runMonadPerspectivesQuery.
applyTypedTripleGetter :: forall s o.
  s
  -> TypedTripleGetter s o
  -> (MonadPerspectivesQuery) (Triple s o)
applyTypedTripleGetter a (TypedTripleGetter _ f) = f a

infix 0 applyTypedTripleGetter as @@

applyTypedTripleGetterToObjects :: forall s o.
  s
  -> TypedTripleGetter s o
  -> (MonadPerspectivesQuery) (Array o)
applyTypedTripleGetterToObjects a (TypedTripleGetter _ f) = f a >>= pure <<< tripleObjects

infix 0 applyTypedTripleGetterToObjects as @@=

applyTypedTripleGetterToMaybeObject :: forall s o.
  s
  -> TypedTripleGetter s o
  -> (MonadPerspectivesQuery) (Maybe o)
applyTypedTripleGetterToMaybeObject a (TypedTripleGetter _ f) = f a >>= pure <<< head <<< tripleObjects

infix 0 applyTypedTripleGetterToMaybeObject as @@>

applyTypedTripleGetterToObject :: forall s o.
  Show s =>
  s
  -> TypedTripleGetter s o
  -> (MonadPerspectivesQuery) o
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
  show (TripleRef {subject, predicate}) = "<" <> subject <> " - " <> predicate <> ">"

-----------------------------------------------------------
-- TYPE CHECKING
-----------------------------------------------------------
type Aspect = String
type TypeID = String
type SimpleValueName = String

data UserMessage =
  -- TypeDefChecker messages
    MissingAspect TypeID Aspect
  | MissingType TypeID TypeID
  | MissingMogelijkeBinding TypeID
  | NoType ContextID
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
  show (MissingType tp as) = "(MissingType) Het type '" <> tp <> "' heeft niet het type '" <> as <> "'."
  show (MissingMogelijkeBinding tp) = "(MissingMogelijkeBinding) Voor Rol '" <> tp <> "' is geen mogelijkeBinding gedefinieerd."
  show (MultipleDefinitions ln aspectArray) = "(MultipleDefinitions) Elk van de volgende Aspecten heeft een definitie voor '" <> ln <> "': " <> show aspectArray
  show (MissingUnqualifiedProperty ln rn) = "(MissingUnqualifiedProperty) Er is geen definitie voor de property '" <> ln <> "' voor de rol '" <> rn <> "'."
  show (MissingQualifiedProperty pn rn) = "(MissingQualifiedProperty) Er is geen definitie voor de property '" <> pn <> "' voor de rol '" <> rn <> "'."
  show (MissingQualifiedRol rn cid) = "(MissingQualifiedRol) Er is geen definitie voor de rol '" <> rn <> "' in de context '" <> cid <> "'."
  show (MissingUnqualifiedRol rn cid) = "(MissingUnqualifiedRol) Er is geen definitie voor de rol '" <> rn <> "' in de context '" <> cid <> "'."
  show (NoType cid) = "(NoType) De context '" <> cid <> "' heeft geen type."
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

derive newtype instance writeForeignTransactie :: WriteForeign Transactie

createTransactie :: String -> Aff Transactie
createTransactie author =
  do
    n <- liftEffect $ now
    pure $ Transactie{ author: author, timeStamp: SerializableDateTime (toDateTime n), deltas: [], createdContexts: [], createdRoles: [], deletedContexts: [], deletedRoles: [], changedDomeinFiles: []}

transactieID :: Transactie -> String
transactieID (Transactie{author, timeStamp}) = author <> "_" <> show timeStamp

-----------------------------------------------------------
-- DATETIME
-- We need a newtype for DateTime in order to be able to serialize and show it.
-----------------------------------------------------------
newtype SerializableDateTime = SerializableDateTime DateTime

instance encodeSerializableDateTime :: Encode SerializableDateTime where
  encode d = unsafeToForeign $ show d

instance showSerializableDateTime :: Show SerializableDateTime where
  show (SerializableDateTime d) = "todo"

instance writeForeignSerializableDateTime :: WriteForeign SerializableDateTime where
  writeImpl (SerializableDateTime dt) = unsafeToForeign $ show dt
