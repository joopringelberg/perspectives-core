module Perspectives.PerspectivesTypesInPurescript where

import Prelude (class Show, show, ($), class Eq, eq, (==))

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

-- PRIMARY REPRESENTATION
newtype Context = Context String

derive instance genericRepContext :: Generic Context _
derive instance newtypeContext :: Newtype Context _
instance showContext :: Show Context where
  show (Context s) = show s
instance decodeContext :: Decode Context where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeContext :: Encode Context where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

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
class Binding a

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

class (Newtype a String, Eq a) <= RolType a

instance rolInContextRol :: RolType RolInContext
instance buitenRolRol :: RolType BuitenRol
instance binnenRol :: RolType BinnenRol

class (Show a, Newtype a String) <= Subject a

instance contextSubject :: Subject Context
instance rolinContextSubject :: Subject RolInContext
instance binnenRolSubject :: Subject BinnenRol
instance buitenRolSubject :: Subject BuitenRol

newtype Val = Val String

derive instance genericRepVal :: Generic Val _
instance showVal :: Show Val where
  show (Val s) = show s
instance decodeVal :: Decode Val where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeVal :: Encode Val where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance eqVal :: Eq Val where
  eq (Val v1) (Val v2) = v1 == v2


class (Show a) <= Object a

instance rolinContextObject :: Object RolInContext
instance binnenRolObject :: Object BinnenRol
instance buitenRolObject :: Object BuitenRol
instance valObject :: Object Val
instance contextObject :: Object Context


-- MODEL:PERSPECTIVES
newtype SimpleValue = SimpleValue String
derive instance newtypeSimpleValue :: Newtype SimpleValue _

newtype PBool = PBool String
newtype PString = PString String
newtype PDate = PDate String
newtype PNumber = PNumber String

newtype ContextDef = ContextDef String

derive instance genericRepContextDef :: Generic ContextDef _
instance showContextDef :: Show ContextDef where
  show (ContextDef s) = show s
instance decodeContextDef :: Decode ContextDef where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeContextDef :: Encode ContextDef where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
derive instance newtypeContextDef :: Newtype ContextDef _

newtype RolDef = RolDef String

derive instance genericRepRolDef :: Generic RolDef _
instance showRolDef :: Show RolDef where
  show (RolDef s) = show s
instance decodeRolDef :: Decode RolDef where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeRolDef :: Encode RolDef where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype PropertyDef = PropertyDef String

derive instance genericRepPropertyDef :: Generic PropertyDef _
instance showPropertyDef :: Show PropertyDef where
  show (PropertyDef s) = show s
instance decodePropertyDef :: Decode PropertyDef where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodePropertyDef :: Encode PropertyDef where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
derive instance newtypePropertyDef :: Newtype PropertyDef _

class (Show a) <= Predicate a

instance rolDefPredicate :: Predicate RolDef
derive instance newtypeRolDef :: Newtype RolDef _

instance propertyDefPredicate :: Predicate PropertyDef

newtype SysteemBot = SysteemBot String
derive instance newtypeSysteemBot :: Newtype SysteemBot _

newtype View = View String
derive instance newtypeRolView :: Newtype View _

newtype Actie = Actie String
derive instance newtypeRolActie :: Newtype Actie _

newtype Zaak = Zaak String
derive instance newtypeZaak :: Newtype Zaak _

newtype QueryFunction = Function String
derive instance newtypeQueryFunction :: Newtype QueryFunction _

newtype ElkType = ElkType String
derive instance newtypeElkType :: Newtype ElkType _

newtype Systeem = Systeem String
derive instance newtypeSysteem :: Newtype Systeem _

newtype TrustedCluster = TrustedCluster String
derive instance newtypeTrustedCluster :: Newtype TrustedCluster _

newtype AssignToRol = AssignToRol String
derive instance newtypeAssignToRol :: Newtype AssignToRol _

newtype AssignToProperty = AssignToProperty String
derive instance newtypeAssignToProperty :: Newtype AssignToProperty _

newtype EffectFullFunction = EffectFullFunction String
derive instance newtypeEffectFullFunction :: Newtype EffectFullFunction _

class Newtype a String <= ContextType a
instance contextDefSimpleValue :: ContextType SimpleValue
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
