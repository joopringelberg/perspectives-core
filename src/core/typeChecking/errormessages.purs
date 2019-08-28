module Perspectives.Checking.PerspectivesTypeChecker.Messages where

import Perspectives.EntiteitAndRDFAliases (ID, ContextID, RolName, PropertyName, RolID) as Alias
import Perspectives.Identifiers (LocalName)
import Prelude (class Show, (<>), show)
-----------------------------------------------------------
-- TYPE CHECKING
-----------------------------------------------------------
type Aspect = String
type TypeID = String
type SimpleValueName = String
type VariableName = String

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
