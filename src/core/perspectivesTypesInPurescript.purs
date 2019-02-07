module Perspectives.PerspectivesTypesInPurescript where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Prelude (class Show, show, ($), class Eq, eq, (==))
import Unsafe.Coerce (unsafeCoerce)

typeWithPerspectivesTypes :: forall a b. a -> b
typeWithPerspectivesTypes = unsafeCoerce

-- PRIMARY REPRESENTATION
newtype Context = Context String

-- TODO: dit type is overbodig!
derive instance genericRepContext :: Generic Context _
derive instance newtypeContext :: Newtype Context _
instance showContext :: Show Context where
  show (Context s) = show s
instance decodeContext :: Decode Context where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeContext :: Encode Context where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance eqContext :: Eq Context where
  eq (Context c1) (Context c2) = c1 == c2

newtype RolInContext = RolInContext String

derive instance genericRepRolInContext :: Generic RolInContext _
derive instance newtypeRolInContext :: Newtype RolInContext _
instance showRolInContext :: Show RolInContext where
  show (RolInContext s) = show s
instance decodeRolInContext :: Decode RolInContext where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeRolInContext :: Encode RolInContext where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance eqRolInContext :: Eq RolInContext where
  eq (RolInContext r1) (RolInContext r2) = eq r1 r2

newtype BuitenRol = BuitenRol String

derive instance genericRepBuitenRol :: Generic BuitenRol _
derive instance newtypeBuitenRol :: Newtype BuitenRol _
instance showBuitenRol :: Show BuitenRol where
  show (BuitenRol s) = show s
instance decodeBuitenRol :: Decode BuitenRol where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeBuitenRol :: Encode BuitenRol where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance eqBuitenRol :: Eq BuitenRol where
  eq (BuitenRol r1) (BuitenRol r2) = eq r1 r2

-- | Class Binding should constrain all functions that manipulate a Rol.
class RolKind a <= Binding a

instance bindingRol :: Binding RolInContext
instance bindingBuitenRol :: Binding BuitenRol

newtype BinnenRol = BinnenRol String

derive instance genericRepBinnenRol :: Generic BinnenRol _
derive instance newtypeBinnenRol :: Newtype BinnenRol _
instance showBinnenRol :: Show BinnenRol where
  show (BinnenRol s) = show s
instance decodeBinnenRol :: Decode BinnenRol where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeBinnenRol :: Encode BinnenRol where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance eqBinnenRol :: Eq BinnenRol where
  eq (BinnenRol r1) (BinnenRol r2) = eq r1 r2

class (Newtype a String, Eq a) <= RolKind a

instance rolInContextRol :: RolKind RolInContext
instance buitenRolRol :: RolKind BuitenRol
instance binnenRol :: RolKind BinnenRol

-- MODEL:PERSPECTIVES
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
