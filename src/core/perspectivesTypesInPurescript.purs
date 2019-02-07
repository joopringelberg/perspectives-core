module Perspectives.PerspectivesTypesInPurescript where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Prelude (class Show, show, ($), class Eq, (==))
import Unsafe.Coerce (unsafeCoerce)

typeWithPerspectivesTypes :: forall a b. a -> b
typeWithPerspectivesTypes = unsafeCoerce

-- MODEL:PERSPECTIVES : ROLES
class RolType a

-- MODEL:PERSPECTIVES : CONTEXTS
newtype SimpleValue = SimpleValue String
derive instance newtypeSimpleValue :: Newtype SimpleValue _
instance eqSimpleValue :: Eq SimpleValue where
  eq (SimpleValue c1) (SimpleValue c2) = c1 == c2

newtype PBool = PBool String
derive instance newtypePBool :: Newtype PBool _
instance eqPBool :: Eq PBool where
  eq (PBool c1) (PBool c2) = c1 == c2

newtype PString = PString String
derive instance newtypePString :: Newtype PString _
instance eqPString :: Eq PString where
  eq (PString c1) (PString c2) = c1 == c2

newtype PDate = PDate String
derive instance newtypePDate :: Newtype PDate _
instance eqPDate :: Eq PDate where
  eq (PDate c1) (PDate c2) = c1 == c2

newtype PNumber = PNumber String
derive instance newtypePNumber :: Newtype PNumber _
instance eqPNumber :: Eq PNumber where
  eq (PNumber c1) (PNumber c2) = c1 == c2

class (Eq a) <= Val a

instance valPBool :: Val PBool
instance valPNumber :: Val PNumber
instance valPDate :: Val PDate
instance valPString :: Val PString

-- | The definition of Context.
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

-- ROLES FOR CONTEXTDEF
newtype RolInContext = RolInContext String
instance rolTypeRolInContext :: RolType RolInContext

newtype Prototype = Prototype String
instance rolTypePrototype :: RolType Prototype

newtype Aspect = Aspect String
instance rolTypeAspect :: RolType Aspect


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

-- ROLES FOR ROLDEF
newtype RolProperty = RolProperty String
instance rolTypeRolProperty :: RolType RolProperty

newtype MogelijkeBinding = MogelijkeBinding String
instance rolTypeMogelijkeBinding :: RolType MogelijkeBinding

newtype ViewInRol = ViewInRol String
instance rolTypeViewInRol :: RolType ViewInRol

newtype AspectRol = AspectRol String
instance roltypeAspectRol :: RolType AspectRol

newtype Constraint = Constraint String
instance roltypeConstraint :: RolType Constraint

newtype ObjectRol = ObjectRol String
instance roltypeObjectRol :: RolType ObjectRol

newtype SubjectRol = SubjectRol String
instance roltypeSubjectRol :: RolType SubjectRol


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

-- ROLES FOR PROPERTYDEF
newtype Range = Range String
instance rolTypeRange :: RolType Range

newtype AspectProperty = AspectProperty String
instance rolTypeAspectProperty :: RolType AspectProperty

newtype BindingProperty = BindingProperty String
instance rolTypeBindingProperty :: RolType BindingProperty

newtype SysteemBot = SysteemBot String
derive instance newtypeSysteemBot :: Newtype SysteemBot _
instance eqSysteemBot :: Eq SysteemBot where
  eq (SysteemBot c1) (SysteemBot c2) = c1 == c2

newtype View = View String
derive instance newtypeView :: Newtype View _
instance eqView :: Eq View where
  eq (View c1) (View c2) = c1 == c2

newtype Actie = Actie String
derive instance newtypeRolActie :: Newtype Actie _
instance eqActie :: Eq Actie where
  eq (Actie c1) (Actie c2) = c1 == c2

newtype Zaak = Zaak String
derive instance newtypeZaak :: Newtype Zaak _
instance eqZaak :: Eq Zaak where
  eq (Zaak c1) (Zaak c2) = c1 == c2

newtype QueryFunction = QueryFunction String
derive instance newtypeQueryFunction :: Newtype QueryFunction _
instance eqQueryFunction :: Eq QueryFunction where
  eq (QueryFunction c1) (QueryFunction c2) = c1 == c2

newtype ElkType = ElkType String
derive instance newtypeElkType :: Newtype ElkType _
instance eqElkType :: Eq ElkType where
  eq (ElkType c1) (ElkType c2) = c1 == c2

newtype Systeem = Systeem String
derive instance newtypeSysteem :: Newtype Systeem _
instance eqSysteem :: Eq Systeem where
  eq (Systeem c1) (Systeem c2) = c1 == c2

newtype TrustedCluster = TrustedCluster String
derive instance newtypeTrustedCluster :: Newtype TrustedCluster _
instance eqTrustedCluster :: Eq TrustedCluster where
  eq (TrustedCluster c1) (TrustedCluster c2) = c1 == c2

newtype AssignToRol = AssignToRol String
derive instance newtypeAssignToRol :: Newtype AssignToRol _
instance eqAssignToRol :: Eq AssignToRol where
  eq (AssignToRol c1) (AssignToRol c2) = c1 == c2

newtype AssignToProperty = AssignToProperty String
derive instance newtypeAssignToProperty :: Newtype AssignToProperty _
instance eqAssignToProperty :: Eq AssignToProperty where
  eq (AssignToProperty c1) (AssignToProperty c2) = c1 == c2

newtype EffectFullFunction = EffectFullFunction String
derive instance newtypeEffectFullFunction :: Newtype EffectFullFunction _
instance eqEffectFullFunction :: Eq EffectFullFunction where
  eq (EffectFullFunction c1) (EffectFullFunction c2) = c1 == c2

-- | The class of all definitions of a Context (all Context types) in
-- | the models model:Perspectives and model:QueryAST.
class (Newtype a String, Eq a) <= ContextType a
instance contextDefSimpleValue :: ContextType SimpleValue
instance contextDefPBool :: ContextType PBool
instance contextDefPNumber :: ContextType PNumber
instance contextDefPDate :: ContextType PDate
instance contextDefPString :: ContextType PString
instance contextDefContextType :: ContextType ContextDef
instance contextTypeRolDef :: ContextType RolDef
instance contextTypePropertyDef :: ContextType PropertyDef
instance contextTypeSysteemBot :: ContextType SysteemBot
instance contextTypeView :: ContextType View
instance contextTypeActie :: ContextType Actie
instance contextTypeZaak :: ContextType Zaak
instance contextTypeQueryFunction :: ContextType QueryFunction
instance contextTypeElkType :: ContextType ElkType
instance contextTypeSysteem :: ContextType Systeem
instance contextTypeTrustedCluster :: ContextType TrustedCluster
instance contextTypeAssignToRol :: ContextType AssignToRol
instance contextTypeAssignToProperty :: ContextType AssignToProperty
instance contextTypeEffectFullFunction :: ContextType EffectFullFunction

-- MODEL:QUERYAST
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
