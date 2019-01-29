module Perspectives.PerspectivesTypesInPurescript where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)

-- PRIMARY REPRESENTATION
newtype Context = Context String

derive instance genericRepContext :: Generic Context _
instance showContext :: Show Context where
  show (Context s) = show s
instance decodeContext :: Decode Context where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeContext :: Encode Context where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype RolInContext = RolInContext String

derive instance genericRepRolInContext :: Generic RolInContext _
instance showRolInContext :: Show RolInContext where
  show (RolInContext s) = show s
instance decodeRolInContext :: Decode RolInContext where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeRolInContext :: Encode RolInContext where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype BuitenRol = BuitenRol String

derive instance genericRepBuitenRol :: Generic BuitenRol _
instance showBuitenRol :: Show BuitenRol where
  show (BuitenRol s) = show s
instance decodeBuitenRol :: Decode BuitenRol where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeBuitenRol :: Encode BuitenRol where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- | Class Binding should constrain all functions that manipulate a Rol.
class Binding a

instance bindingRol :: Binding RolInContext
instance bindingBuitenRol :: Binding BuitenRol

newtype BinnenRol = BinnenRol String

derive instance genericRepBinnenRol :: Generic BinnenRol _
instance showBinnenRol :: Show BinnenRol where
  show (BinnenRol s) = show s
instance decodeBinnenRol :: Decode BinnenRol where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeBinnenRol :: Encode BinnenRol where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

class Rol a

instance rolInContextRol :: Rol RolInContext
instance buitenRolRol :: Rol BuitenRol
instance binnenRol :: Rol BinnenRol

class Subject a

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

class Object a

instance rolinContextObject :: Object RolInContext
instance binnenRolObject :: Object BinnenRol
instance buitenRolObject :: Object BuitenRol
instance valObject :: Object Val
instance contextObject :: Object Context


-- MODEL:PERSPECTIVES
newtype SimpleValue = SimpleValue String

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

class Predicate a

instance rolDefPredicate :: Predicate RolDef
instance propertyDefPredicate :: Predicate PropertyDef

newtype SysteemBot = SysteemBot String

newtype View = View String
newtype Actie = Actie String

newtype Zaak = Zaak String

newtype Function = Function String
newtype ElkType = ElkType String

newtype Systeem = Systeem String

newtype TrustedCluster = TrustedCluster String

newtype AssignToRol = AssignToRol String

newtype AssignToProperty = AssignToProperty String

newtype EffectFullFunction = EffectFullFunction String

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
