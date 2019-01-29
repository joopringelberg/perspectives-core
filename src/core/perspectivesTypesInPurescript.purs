module Perspectives.PerspectivesTypesInPurescript where


-- PRIMARY REPRESENTATION
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

newtype Rol = Rol String

derive instance genericRepRol :: Generic Rol _
instance showRol :: Show Rol where
  show (Rol s) = show s
instance decodeRol :: Decode Rol where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeRol :: Encode Rol where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype Val = Val String

derive instance genericRepVal :: Generic Val _
instance showVal :: Show Val where
  show (Val s) = show s
instance decodeVal :: Decode Val where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeVal :: Encode Val where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

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
