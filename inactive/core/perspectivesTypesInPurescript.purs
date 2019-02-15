module Perspectives.PerspectivesTypesInPurescript where

import Data.Array (singleton)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Perspectives.ContextAndRole (context_pspType, rol_binding, rol_context)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (~~>), ObjectsGetter)
import Prelude (class Show, show, ($), class Eq, (==))
import Unsafe.Coerce (unsafeCoerce)

typeWithPerspectivesTypes :: forall a b. a -> b
typeWithPerspectivesTypes = unsafeCoerce

-----------------------------------------------------------
-- CONTEXT AS CLASS (THE TYPE OF CONTEXTTYPES)
-----------------------------------------------------------
-- | The class of all definitions of a Context (all Context types) in
-- | the models model:Perspectives and model:QueryAST.
class (Newtype a String, Eq a) <= ContextType a

-----------------------------------------------------------
-- ROL AS CLASS (THE TYPE OF ROLTYPES)
-----------------------------------------------------------
-- | The class of all definitions of a Rol (all Rol types) in
-- | the models model:Perspectives and model:QueryAST.
class (Eq a) <= RolType a

-----------------------------------------------------------
-- SIMPLEVALUE AS CLASS (THE TYPE OF SIMPLEVALUETYPES)
-----------------------------------------------------------
-- | The class of all definitions of a Simple Value (all SimpleValue types) in
-- | the models model:Perspectives and model:QueryAST.
class (Eq a) <= SimpleValueType a

-----------------------------------------------------------
-- THE CLASS HASCONTEXTTYPE
-----------------------------------------------------------
-- | The class that connects a Context to its type.
class (ContextType tp) <= HasContextType context tp | context -> tp where
  contextType :: forall e. (context ~~> tp) e

genericContextType :: forall e. ObjectsGetter e
genericContextType = getContextMember \context -> [context_pspType context]

-----------------------------------------------------------
-- THE CLASS ROLVANCONTEXT
-----------------------------------------------------------
-- | The class that connects a Rol to its Context, typing both correctly.
class (RolType rol, ContextType context) <= RolVanContext rol context | rol -> context where
		context :: forall e. (rol ~~> context) e

genericContext :: forall e. ObjectsGetter e
genericContext = getRolMember \rol -> [rol_context rol]

-----------------------------------------------------------
-- THE CLASS BINDING
-----------------------------------------------------------
-- | The class that connects a Rol to its binding, typing both correctly.
class (RolType binder, RolType bound) <= Binding binder bound | binder -> bound where
  binding :: forall e. (binder ~~> bound)e

genericBinding :: forall e. ObjectsGetter e
genericBinding = getRolMember \rol -> maybe [] singleton (rol_binding rol)

-----------------------------------------------------------
-- SIMPLEVALUE AS TYPE
-----------------------------------------------------------
-- |  Use this type if the exact SimpleValue type is unknown.
newtype SimpleValueDef = SimpleValueDef String
derive instance newtypeSimpleValueDef :: Newtype SimpleValueDef _
instance eqSimpleValueDef :: Eq SimpleValueDef where
  eq (SimpleValueDef c1) (SimpleValueDef c2) = c1 == c2

instance contextDefSimpleValue :: ContextType SimpleValueDef

-- BuitenRol
newtype SimpleValueDefBR = SimpleValueDefBR String
derive instance newtypeSimpleValueDefBr :: Newtype SimpleValueDefBR _
instance eqSimpleValueDefBR :: Eq SimpleValueDefBR where
  eq (SimpleValueDefBR c1) (SimpleValueDefBR c2) = c1 == c2
instance rolTypeSimpleValueDefBR :: RolType SimpleValueDefBR
instance rolVanContextSimpleValueDefBRSimpleValue :: RolVanContext SimpleValueDefBR SimpleValueDef where
  context = typeWithPerspectivesTypes  genericContext

-----------------------------------------------------------
-- PBOOL
-----------------------------------------------------------
newtype PBool = PBool String
derive instance newtypePBool :: Newtype PBool _
instance eqPBool :: Eq PBool where
  eq (PBool c1) (PBool c2) = c1 == c2

instance contextDefPBool :: ContextType PBool
instance valPBool :: SimpleValueType PBool
instance hctPBool :: HasContextType PBool SimpleValueDef where
  contextType = typeWithPerspectivesTypes genericContextType

newtype PBoolBR = PBoolBR String
derive instance newtypePBoolBr :: Newtype PBoolBR _
instance eqPBoolBR :: Eq PBoolBR where
  eq (PBoolBR c1) (PBoolBR c2) = c1 == c2
instance rolTypePBoolBR :: RolType PBoolBR
instance rolVanContextPBoolBRSimpleValue :: RolVanContext PBoolBR PBool where
  context = typeWithPerspectivesTypes  genericContext

-----------------------------------------------------------
-- PSTRING
-----------------------------------------------------------
newtype PString = PString String
derive instance newtypePString :: Newtype PString _
instance eqPString :: Eq PString where
  eq (PString c1) (PString c2) = c1 == c2

instance contextDefPString :: ContextType PString
instance valPString :: SimpleValueType PString
instance hctPString :: HasContextType PString  SimpleValueDef where
  contextType = typeWithPerspectivesTypes genericContextType

newtype PStringBR = PStringBR String
derive instance newtypePStringBr :: Newtype PStringBR _
instance eqPStringBR :: Eq PStringBR where
  eq (PStringBR c1) (PStringBR c2) = c1 == c2
instance rolTypePStringBR :: RolType PStringBR
instance rolVanContextPStringBRSimpleValue :: RolVanContext PStringBR PString where
  context = typeWithPerspectivesTypes  genericContext

-----------------------------------------------------------
-- PDATE
-----------------------------------------------------------
newtype PDate = PDate String
derive instance newtypePDate :: Newtype PDate _
instance eqPDate :: Eq PDate where
  eq (PDate c1) (PDate c2) = c1 == c2

instance contextDefPDate :: ContextType PDate
instance valPDate :: SimpleValueType PDate
instance hctPDate :: HasContextType PDate  SimpleValueDef where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- PNUMBER
-----------------------------------------------------------
newtype PNumber = PNumber String
derive instance newtypePNumber :: Newtype PNumber _
instance eqPNumber :: Eq PNumber where
  eq (PNumber c1) (PNumber c2) = c1 == c2

instance contextDefPNumber :: ContextType PNumber
instance valPNumber :: SimpleValueType PNumber
instance hctPNumber :: HasContextType PNumber SimpleValueDef where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- CONTEXT AS A TYPE
-----------------------------------------------------------
-- | The definition of Context. Use this type if the exact type of the context is
-- | unknown. It is also the type of many types.
newtype ContextDef = ContextDef String

derive instance genericRepContextDef :: Generic ContextDef _
instance showContextDef :: Show ContextDef where
  show (ContextDef s) = show s
instance decodeContextDef :: Decode ContextDef where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeContextDef :: Encode ContextDef where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
derive instance newtypeContextDef :: Newtype ContextDef _
instance eqContextDef :: Eq ContextDef where
  eq (ContextDef c1) (ContextDef c2) = c1 == c2

instance contextDefContextType :: ContextType ContextDef
instance hctContextType :: HasContextType ContextDef ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-- ROLES FOR CONTEXTDEF
newtype BuitenRol = BuitenRol String
instance rolTypeBuitenRol :: RolType BuitenRol
instance eqBuitenRol :: Eq BuitenRol where
  eq (BuitenRol b1) (BuitenRol b2) = b1 == b2

instance buitenRolBinding :: Binding BuitenRol BuitenRol where
  binding = typeWithPerspectivesTypes genericBinding
instance buitenRolContextRolVanContext :: RolVanContext BuitenRol ContextDef where
  context = typeWithPerspectivesTypes  genericContext

newtype BinnenRol = BinnenRol String
instance rolTypeBinnenRol :: RolType BinnenRol
instance eqBinnenRol :: Eq BinnenRol where
  eq (BinnenRol b1) (BinnenRol b2) = b1 == b2

instance binnenRolBinding :: Binding BinnenRol BuitenRol where
  binding = typeWithPerspectivesTypes genericBinding
instance binnenRolContextRolVanContext :: RolVanContext BinnenRol ContextDef where
  context = typeWithPerspectivesTypes  genericContext

newtype RolInContext = RolInContext String
instance rolTypeRolInContext :: RolType RolInContext
instance eqRolInContext :: Eq RolInContext where
  eq (RolInContext b1) (RolInContext b2) = b1 == b2

instance rolInContextBinding :: Binding RolInContext RolDef where
  binding = typeWithPerspectivesTypes genericBinding
instance rolInContextContextRolVanContext :: RolVanContext RolInContext ContextDef where
  context = typeWithPerspectivesTypes  genericContext

newtype Prototype = Prototype String
instance rolTypePrototype :: RolType Prototype
instance eqPrototype :: Eq Prototype where
  eq (Prototype b1) (Prototype b2) = b1 == b2

instance prototypeBinding :: Binding Prototype BuitenRol where
  binding = typeWithPerspectivesTypes genericBinding
instance prototypeContextRolVanContext :: RolVanContext Prototype ContextDef where
  context = typeWithPerspectivesTypes  genericContext

newtype Aspect = Aspect String
instance rolTypeAspect :: RolType Aspect
instance eqAspect :: Eq Aspect where
  eq (Aspect b1) (Aspect b2) = b1 == b2

instance aspectBinding :: Binding Aspect RolDef where
  binding = typeWithPerspectivesTypes genericBinding
instance aspectContextRolVanContext :: RolVanContext Aspect ContextDef where
  context = typeWithPerspectivesTypes  genericContext

-----------------------------------------------------------
-- ROL AS A TYPE
-----------------------------------------------------------
-- | Use this type for roles that have no specific Purescript type.
newtype RolDef = RolDef String

derive instance genericRepRolDef :: Generic RolDef _
instance showRolDef :: Show RolDef where
  show (RolDef s) = show s
instance decodeRolDef :: Decode RolDef where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeRolDef :: Encode RolDef where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance eqRolDef :: Eq RolDef where
  eq (RolDef c1) (RolDef c2) = c1 == c2
derive instance newtypeRolDef :: Newtype RolDef _

instance contextTypeRolDef :: ContextType RolDef
instance hctRolDef :: HasContextType RolDef ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-- ROLDEF AS 'VANILLA ROL'
instance rolTypeRolDef :: RolType RolDef
instance rolVanContextRolDef :: RolVanContext RolDef ContextDef where
  context = typeWithPerspectivesTypes  genericContext
instance bindingRolDef :: Binding RolDef RolDef where
  binding = typeWithPerspectivesTypes genericBinding
-- But this is equally valid, semantically. However, we can only have one instance for RolDef.
-- instance bindingRolDef :: Binding RolDef BuitenRol where
--   binding = typeWithPerspectivesTypes genericBinding

-- ROLES FOR ROLDEF
newtype RolProperty = RolProperty String
instance rolTypeRolProperty :: RolType RolProperty
instance eqRolProperty :: Eq RolProperty where
  eq (RolProperty b1) (RolProperty b2) = b1 == b2
instance rolPropertyRolRolVanContext :: RolVanContext RolProperty RolDef where
  context = typeWithPerspectivesTypes  genericContext

newtype MogelijkeBinding = MogelijkeBinding String
instance rolTypeMogelijkeBinding :: RolType MogelijkeBinding
instance eqMogelijkeBinding :: Eq MogelijkeBinding where
  eq (MogelijkeBinding b1) (MogelijkeBinding b2) = b1 == b2

newtype ViewInRol = ViewInRol String
instance rolTypeViewInRol :: RolType ViewInRol
instance eqViewInRol :: Eq ViewInRol where
  eq (ViewInRol b1) (ViewInRol b2) = b1 == b2

newtype AspectRol = AspectRol String
instance roltypeAspectRol :: RolType AspectRol
instance eqAspectRol :: Eq AspectRol where
  eq (AspectRol b1) (AspectRol b2) = b1 == b2

newtype Constraint = Constraint String
instance roltypeConstraint :: RolType Constraint
instance eqConstraint :: Eq Constraint where
  eq (Constraint b1) (Constraint b2) = b1 == b2

newtype ObjectRol = ObjectRol String
instance roltypeObjectRol :: RolType ObjectRol
instance eqObjectRol :: Eq ObjectRol where
  eq (ObjectRol b1) (ObjectRol b2) = b1 == b2

newtype SubjectRol = SubjectRol String
instance roltypeSubjectRol :: RolType SubjectRol
instance eqSubjectRol :: Eq SubjectRol where
  eq (SubjectRol b1) (SubjectRol b2) = b1 == b2


-----------------------------------------------------------
-- PROPERTY
-----------------------------------------------------------
-- | The type of all Properties.
newtype PropertyDef = PropertyDef String

derive instance genericRepPropertyDef :: Generic PropertyDef _
instance showPropertyDef :: Show PropertyDef where
  show (PropertyDef s) = show s
instance decodePropertyDef :: Decode PropertyDef where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodePropertyDef :: Encode PropertyDef where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
derive instance newtypePropertyDef :: Newtype PropertyDef _
instance eqPropertyDef :: Eq PropertyDef where
  eq (PropertyDef c1) (PropertyDef c2) = c1 == c2

instance contextTypePropertyDef :: ContextType PropertyDef
instance hctPropertyDef :: HasContextType RolDef PropertyDef where
  contextType = typeWithPerspectivesTypes genericContextType

newtype PropertyDefBR = PropertyDefBR String
derive instance newtypePropertyDefBr :: Newtype PropertyDefBR _
instance eqPropertyDefBR :: Eq PropertyDefBR where
  eq (PropertyDefBR c1) (PropertyDefBR c2) = c1 == c2
instance rolTypePropertyDefBR :: RolType PropertyDefBR
instance rolVanContextPropertyDefBRSimpleValue :: RolVanContext PropertyDefBR PropertyDef where
  context = typeWithPerspectivesTypes  genericContext

-- ROLES FOR PROPERTYDEF
newtype Range = Range String
instance rolTypeRange :: RolType Range
instance eqRange :: Eq Range where
  eq (Range b1) (Range b2) = b1 == b2
instance rangeBinding :: Binding Range PBoolBR where
  binding = typeWithPerspectivesTypes genericBinding

newtype AspectProperty = AspectProperty String
instance rolTypeAspectProperty :: RolType AspectProperty
instance eqAspectProperty :: Eq AspectProperty where
  eq (AspectProperty b1) (AspectProperty b2) = b1 == b2
instance aspectPropertyBinding :: Binding AspectProperty PropertyDefBR where
  binding = typeWithPerspectivesTypes genericBinding

newtype BindingProperty = BindingProperty String
instance rolTypeBindingProperty :: RolType BindingProperty
instance eqBindingProperty :: Eq BindingProperty where
  eq (BindingProperty b1) (BindingProperty b2) = b1 == b2
instance bindingPropertyBinding :: Binding BindingProperty PropertyDefBR where
  binding = typeWithPerspectivesTypes genericBinding

-----------------------------------------------------------
-- SYSTEEMBOT
-----------------------------------------------------------
newtype SysteemBot = SysteemBot String
derive instance newtypeSysteemBot :: Newtype SysteemBot _
instance eqSysteemBot :: Eq SysteemBot where
  eq (SysteemBot c1) (SysteemBot c2) = c1 == c2

instance contextTypeSysteemBot :: ContextType SysteemBot
instance hctSysteemBot :: HasContextType RolDef SysteemBot where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------
newtype View = View String
derive instance newtypeView :: Newtype View _
instance eqView :: Eq View where
  eq (View c1) (View c2) = c1 == c2

instance contextTypeView :: ContextType View
instance hctView :: HasContextType View ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- ACTIE
-----------------------------------------------------------
newtype Actie = Actie String
derive instance newtypeRolActie :: Newtype Actie _
instance eqActie :: Eq Actie where
  eq (Actie c1) (Actie c2) = c1 == c2

instance contextTypeActie :: ContextType Actie
instance hctActie :: HasContextType RolDef Actie where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- ZAAK
-----------------------------------------------------------
newtype Zaak = Zaak String
derive instance newtypeZaak :: Newtype Zaak _
instance eqZaak :: Eq Zaak where
  eq (Zaak c1) (Zaak c2) = c1 == c2

instance contextTypeZaak :: ContextType Zaak
instance hctZaak :: HasContextType Zaak ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- QUERYFUNCTION
-----------------------------------------------------------
newtype QueryFunction = QueryFunction String
derive instance newtypeQueryFunction :: Newtype QueryFunction _
instance eqQueryFunction :: Eq QueryFunction where
  eq (QueryFunction c1) (QueryFunction c2) = c1 == c2

instance contextTypeQueryFunction :: ContextType QueryFunction
instance hctQueryFunction :: HasContextType QueryFunction ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- ELKTYPE
-----------------------------------------------------------
newtype ElkType = ElkType String
derive instance newtypeElkType :: Newtype ElkType _
instance eqElkType :: Eq ElkType where
  eq (ElkType c1) (ElkType c2) = c1 == c2

instance contextTypeElkType :: ContextType ElkType
instance hctElkType :: HasContextType ElkType ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- SYSTEEM
-----------------------------------------------------------
newtype Systeem = Systeem String
derive instance newtypeSysteem :: Newtype Systeem _
instance eqSysteem :: Eq Systeem where
  eq (Systeem c1) (Systeem c2) = c1 == c2

instance contextTypeSysteem :: ContextType Systeem
instance hctSysteem :: HasContextType Systeem ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-- ROLES FOR SYSTEEM
newtype Gebruiker = Gebruiker String
instance rolTypeGebruiker :: RolType Gebruiker
instance eqGebruiker :: Eq Gebruiker where
  eq (Gebruiker b1) (Gebruiker b2) = b1 == b2

-----------------------------------------------------------
-- TRUSTEDCLUSTER
-----------------------------------------------------------
newtype TrustedCluster = TrustedCluster String
derive instance newtypeTrustedCluster :: Newtype TrustedCluster _
instance eqTrustedCluster :: Eq TrustedCluster where
  eq (TrustedCluster c1) (TrustedCluster c2) = c1 == c2

instance contextTypeTrustedCluster :: ContextType TrustedCluster
instance hctTrustedCluster :: HasContextType TrustedCluster ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-- ROLES FOR TRUSTEDCLUSTER
newtype ClusterGenoot = ClusterGenoot String
instance rolTypeClusterGenoot :: RolType ClusterGenoot
instance eqClusterGenoot :: Eq ClusterGenoot where
  eq (ClusterGenoot b1) (ClusterGenoot b2) = b1 == b2

instance clusterGenootBinding :: Binding ClusterGenoot Gebruiker where
  binding = typeWithPerspectivesTypes genericBinding

-----------------------------------------------------------
-- ASSIGNTOROL
-----------------------------------------------------------
newtype AssignToRol = AssignToRol String
derive instance newtypeAssignToRol :: Newtype AssignToRol _
instance eqAssignToRol :: Eq AssignToRol where
  eq (AssignToRol c1) (AssignToRol c2) = c1 == c2

instance contextTypeAssignToRol :: ContextType AssignToRol
instance hctAssignToRol :: HasContextType AssignToRol ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- ASSIGNTOPROPERTY
-----------------------------------------------------------
newtype AssignToProperty = AssignToProperty String
derive instance newtypeAssignToProperty :: Newtype AssignToProperty _
instance eqAssignToProperty :: Eq AssignToProperty where
  eq (AssignToProperty c1) (AssignToProperty c2) = c1 == c2

instance contextTypeAssignToProperty :: ContextType AssignToProperty
instance hctAssignToProperty :: HasContextType AssignToProperty ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType

-----------------------------------------------------------
-- EFFECTFULLFUNCTION
-----------------------------------------------------------
newtype EffectFullFunction = EffectFullFunction String
derive instance newtypeEffectFullFunction :: Newtype EffectFullFunction _
instance eqEffectFullFunction :: Eq EffectFullFunction where
  eq (EffectFullFunction c1) (EffectFullFunction c2) = c1 == c2

instance contextTypeEffectFullFunction :: ContextType EffectFullFunction
instance hctEffectFullFunction :: HasContextType EffectFullFunction ContextDef where
  contextType = typeWithPerspectivesTypes genericContextType


-----------------------------------------------------------
-- MODEL:QUERYAST
-----------------------------------------------------------
newtype DataTypeGetter = DataTypeGetter String
newtype PropertyGetter = PropertyGetter String
newtype RolGetter = RolGetter String
newtype ComputedRolGetter = ComputedRolGetter String
newtype ComputedPropertyGetter = ComputedPropertyGetter String
newtype UnaryCombinator = UnaryCombinator String
newtype NAryCombinator = NAryCombinator String

newtype Filter = Filter String
newtype RolesOf = RolesOf String
newtype Contains = Contains String
newtype Constant = Constant String
newtype Variable = Variable String
newtype SetVariable = SetVariable String
