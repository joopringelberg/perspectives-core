module Perspectives.CoreTypes where

import Perspectives.EntiteitAndRDFAliases (ID, Value, Predicate, ContextID, RolName, PropertyName, RolID) as Alias

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, gets, modify)
import Data.Array (head)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Exception (error)
import Foreign.Object (Object, insert, lookup) as O
import Partial.Unsafe (unsafePartial)
import Perspectives.CouchdbState (CouchdbState)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.GlobalUnsafeStrMap (GLStrMap)
import Perspectives.Identifiers (LocalName)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.Resource (Object, Objects, Subject, firstObject, justObject, noObject)
import Perspectives.Representation.View (View)
import Perspectives.Sync.Transactie (Transactie)
import Prelude (class Eq, class Monad, class Show, Unit, pure, show, void, ($), (&&), (<<<), (<>), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- PERSPECTIVESSTATE
-----------------------------------------------------------
type ContextDefinitions = GLStrMap (AVar PerspectContext)
type RolDefinitions = GLStrMap (AVar PerspectRol)
type Contexts = GLStrMap (AVar Context)
type EnumeratedRoles = GLStrMap (AVar EnumeratedRole)
type CalculatedRoles = GLStrMap (AVar CalculatedRole)
type EnumeratedProperties = GLStrMap (AVar EnumeratedProperty)
type CalculatedProperties = GLStrMap (AVar CalculatedProperty)
type Views = GLStrMap (AVar View)
type Actions = GLStrMap (AVar Action)
type DomeinCache = GLStrMap (AVar DomeinFile)
type QueryCache = GLStrMap (TypedTripleGetter String String)

type PerspectivesState = CouchdbState (
  rolDefinitions :: RolDefinitions
  , contextDefinitions :: ContextDefinitions
  -- Perspectives types aanvullen
  , contexts :: Contexts
  , enumeratedRoles :: EnumeratedRoles
  , calculatedRoles :: CalculatedRoles
  , enumeratedProperties :: EnumeratedProperties
  , calculatedProperties :: CalculatedProperties
  , views :: Views
  , actions :: Actions

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

type MonadPerspectivesObjects = MonadPerspectives (Array Alias.ID)

type VariableName = String

putQueryVariable :: VariableName -> TripleRef -> MonadPerspectivesQuery Unit
putQueryVariable var t = void $ modify \env -> O.insert var t env

readQueryVariable :: VariableName -> MonadPerspectivesQuery TripleRef
readQueryVariable var = gets \env -> unsafePartial (fromJust (O.lookup var env))

-----------------------------------------------------------
-- OBJECT(S)GETTER
-----------------------------------------------------------
-- TODO: FASEER UIT.
type ObjectsGetter = Alias.ID -> MonadPerspectives (Array Alias.Value)

type ObjectGetter = Alias.ID -> MonadPerspectives String

applyObjectsGetter :: Alias.ID -> ObjectsGetter -> MonadPerspectives (Array Alias.Value)
applyObjectsGetter id g = g id

infix 0 applyObjectsGetter as %%
infix 0 applyObjectsGetter as %%=

applyObjectsGetterToMaybeObject :: Alias.ID -> ObjectsGetter -> MonadPerspectives (Maybe Alias.Value)
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
  , predicate :: Alias.Predicate
  , object :: o
  , dependencies :: Array TripleRef
  , supports :: Array TripleRef
  , tripleGetter :: TripleGetter s o}

instance showTriple :: (Show s, Show o) => Show (Triple s o) where
  show (Triple{subject, predicate, object}) = "<" <> show subject <> ";" <> show predicate <> ";" <> show object <> ">"

instance eqTriple :: (Eq s, Eq o) => Eq (Triple s o) where
  eq (Triple({subject: s1, predicate: p1})) (Triple({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

tripleObjects :: forall s o. Triple s o -> o
tripleObjects (Triple{object}) = object

tripleObjects_  :: forall s o m. Monad m => (Triple s o) -> m o
tripleObjects_ (Triple{object}) = pure object

-----------------------------------------------------------
-- TRIPLEQUEUE
-----------------------------------------------------------
type TripleQueue = Array TripleQueueElement

newtype TripleQueueElement = TripleQueueElement { subject :: Subject, predicate :: Alias.Predicate, dependencies :: Array TripleRef}

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
applyTypedTripleGetter ::
  Subject
  -> TypedTripleGetter Subject Objects
  -> (MonadPerspectivesQuery) (Triple Subject Objects)
applyTypedTripleGetter a (TypedTripleGetter _ f) = f a

infix 0 applyTypedTripleGetter as @@

applyTypedTripleGetterToObjects ::
  Subject
  -> TypedTripleGetter Subject Objects
  -> MonadPerspectivesQuery Objects
applyTypedTripleGetterToObjects a (TypedTripleGetter _ f) = f a >>= pure <<< tripleObjects

infix 0 applyTypedTripleGetterToObjects as @@=

applyTypedTripleGetterToMaybeObject ::
  Subject
  -> TypedTripleGetter Subject Objects
  -> (MonadPerspectivesQuery) (Maybe Object)
applyTypedTripleGetterToMaybeObject a (TypedTripleGetter _ f) = f a >>= \os -> if (noObject $ firstObject $ tripleObjects os)
  then pure Nothing
  else pure $ Just $ justObject $ firstObject $ tripleObjects os

infix 0 applyTypedTripleGetterToMaybeObject as @@>

applyTypedTripleGetterToObject ::
  Subject
  -> TypedTripleGetter Subject Objects
  -> (MonadPerspectivesQuery) Object
applyTypedTripleGetterToObject a (TypedTripleGetter n f) = f a >>= \(Triple{object}) ->
  if (noObject $ firstObject object)
    then  throwError $ error $ "TypedTripleGetter '" <> n <> "' returns no values for '" <> show a <> "'."
    else pure $ justObject $ firstObject object

infix 0 applyTypedTripleGetterToObject as @@>>

-----------------------------------------------------------
-- TRIPLEREF
-----------------------------------------------------------
newtype TripleRef = TripleRef { subject :: Subject, predicate :: Alias.Predicate}

instance eqTripleRef :: Eq TripleRef where
  eq (TripleRef({subject: s1, predicate: p1})) (TripleRef({subject: s2, predicate: p2})) = (s1 == s2) && (p1 == p2)

instance showTripleRef :: Show TripleRef where
  show (TripleRef {subject, predicate}) = "<" <> show subject <> " - " <> predicate <> ">"

-----------------------------------------------------------
-- STRINGTYPEDTRIPLEGETTER
-----------------------------------------------------------
type StringTriple = Triple String String
type StringTripleGetter = TripleGetter String String
type StringTypedTripleGetter = (String **> String)

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
  | NoType Alias.ContextID
  | MissingRolInstance Alias.RolName Alias.ContextID
  | IncorrectRolinContextBinding Alias.ContextID Alias.RolName TypeID TypeID TypeID
  | IncorrectContextRolBinding Alias.ContextID Alias.RolName TypeID TypeID TypeID
  | RolNotDefined Alias.RolName Alias.ContextID TypeID
  | MissingPropertyValue Alias.ContextID Alias.PropertyName Alias.RolName
  | MissingExternalPropertyValue Alias.PropertyName Alias.ContextID
  | MissingInternalPropertyValue Alias.PropertyName Alias.ContextID
  | IncorrectPropertyValue Alias.ContextID Alias.PropertyName TypeID String
  | TooManyPropertyValues Alias.ContextID Alias.PropertyName
  | PropertyNotDefined Alias.ContextID Alias.PropertyName Alias.RolID Alias.RolName
  | AspectRolNotFromAspect Alias.RolName Alias.RolName Alias.ContextID
  | AspectPropertyNotFromAspectRol Alias.PropertyName Alias.PropertyName Alias.RolName
  | CycleInAspects Alias.ContextID (Array TypeID)
  | CycleInAspectRoles Alias.RolName (Array TypeID)
  | CycleInAspectProperties Alias.PropertyName (Array TypeID)
  | RolWithoutContext Alias.RolName
  | PropertyWithoutRol Alias.PropertyName
  | CannotOverrideBooleanAspectProperty Alias.PropertyName Alias.PropertyName
  | BindingPropertyCannotOverrideBooleanAspectProperty Alias.PropertyName Alias.PropertyName Alias.PropertyName
  | CannotOverideBooleanRolProperty Alias.RolName Alias.PropertyName
  | MissingRange Alias.PropertyName
  | RangeNotSubsumed SimpleValueName Alias.PropertyName SimpleValueName Alias.PropertyName
  | RangeNotSubsumedByBindingProperty Alias.PropertyName SimpleValueName Alias.PropertyName SimpleValueName Alias.PropertyName
  | MogelijkeBindingNotSubsumed String Alias.RolName String Alias.RolName
  | MissingAspectPropertyForBindingProperty Alias.PropertyName Alias.PropertyName
  | BindingPropertyNotAvailable Alias.PropertyName Alias.PropertyName
  | IncompatiblePrototype Alias.ContextID Alias.ContextID Alias.ContextID

  -- Other messages
  | MultipleDefinitions LocalName (Array TypeID)
  | MissingVariableDeclaration String
  | VariableAlreadyDeclaredAs VariableName TypeID
  | MissingUnqualifiedProperty LocalName Alias.RolName
  | MissingQualifiedProperty Alias.PropertyName Alias.RolName
  | MissingQualifiedRol Alias.RolName Alias.ContextID
  | MissingUnqualifiedRol Alias.RolName Alias.ContextID
  | ContextExists Alias.ID
  | NotAValidIdentifier String
  | NotWellFormedContextSerialization String

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
