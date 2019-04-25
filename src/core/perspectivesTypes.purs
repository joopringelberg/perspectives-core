module Perspectives.PerspectivesTypes where

import Data.Array (findIndex, index, singleton)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (keys, lookup)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_binnenRol, rol_binding, rol_properties)
import Perspectives.ContextRolAccessors (getRolMember, getContextMember')
import Perspectives.CoreTypes (type (~~>), ObjectsGetter)
import Perspectives.Identifiers (deconstructBinnenRol, LocalName)
import Perspectives.Syntax (PerspectRol(..), propertyValue)
import Prelude (class Eq, class Show, bind, pure, show, ($), (==), (<>))
import Unsafe.Coerce (unsafeCoerce)

typeWithPerspectivesTypes :: forall a b. a -> b
typeWithPerspectivesTypes = unsafeCoerce

-----------------------------------------------------------
-- CONTEXT AS A TYPE
-----------------------------------------------------------
-- | The type of definitions of contexts. A specific type of context, say 'Meeting', is a ContextDef (has the type
-- | ContextDef in the Purescript code).
-- | An instance of Meeting would be typed with the Purescript type Context.
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

type ZaakDef = ContextDef
type ActieDef = ContextDef
type FunctionDef = ContextDef
-----------------------------------------------------------
-- ROL AS A TYPE
-----------------------------------------------------------
-- | The type of definitions of roles.
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

type UserRolDef = RolDef
-----------------------------------------------------------
-- PROPERTY AS A TYPE
-----------------------------------------------------------
-- | The type of definitions of properties.
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

-----------------------------------------------------------
-- SIMPLEVALUE AS TYPE
-----------------------------------------------------------
-- | The type of definitions of values. In Perspectives, a psp:Bool has the type psp:SimpleValue.
newtype SimpleValueDef = SimpleValueDef String
derive instance newtypeSimpleValueDef :: Newtype SimpleValueDef _
instance eqSimpleValueDef :: Eq SimpleValueDef where
  eq (SimpleValueDef c1) (SimpleValueDef c2) = c1 == c2

-----------------------------------------------------------
-- PBOOL AS TYPE
-----------------------------------------------------------
-- | The Purescript type of the value of a Boolean-typed property, in Perspectives.
-- | Notice that in Purescript, all representations of Perspectives values are strings.
-- | We need this because in large parts of the application code, Perspectives data are not typed.
-- | There is (in terms of Purescript types) no formalised relation between SimpleValueDef and PBool.
-- | However, in Perspectives the type of psp:PBool is psp:SimpleValue.
newtype PBool = PBool String
derive instance newtypePBool :: Newtype PBool _
instance eqPBool :: Eq PBool where
  eq (PBool c1) (PBool c2) = c1 == c2
instance showPBool :: Show PBool where
  show (PBool b) = show b

-----------------------------------------------------------
-- PSTRING AS TYPE
-----------------------------------------------------------
-- | The Purescript type of the value of a Boolean-typed property, in Perspectives.
-- | Notice that in Purescript, all representations of Perspectives values are strings.
-- | We need this because in large parts of the application code, Perspectives data are not typed.
-- | There is (in terms of Purescript types) no formalised relation between SimpleValueDef and PBool.
-- | However, in Perspectives the type of psp:PBool is psp:SimpleValue.
newtype PString = PString String
derive instance newtypePString :: Newtype PString _
instance eqPString :: Eq PString where
  eq (PString c1) (PString c2) = c1 == c2
instance showPString :: Show PString where
  show (PString b) = show b

-----------------------------------------------------------
-- CONTEXT AS INSTANCE
-----------------------------------------------------------
-- | An instance of Context. To be used if one knows the context not to be a definition.
newtype Context = Context String

derive instance genericRepContext :: Generic Context _
derive instance newtypeContext :: Newtype Context _
instance eqContext :: Eq Context where
  eq (Context c1) (Context c2) = c1 == c2

type FunctionInstance = Context
-----------------------------------------------------------
-- A CONVENIENT TYPE ALIAS FOR ANY CONTEXT
-----------------------------------------------------------
-- | To be used when all is known is that we deal with a context (the weakest form of typing; actually
-- | no typing at all, just signalling to the programmer).
type AnyContext = String

-----------------------------------------------------------
-- A CONVENIENT TYPE ALIAS FOR ANY DEFINITION
-----------------------------------------------------------
type AnyDefinition = String

-----------------------------------------------------------
-- THE CLASS ROLCLASS
-----------------------------------------------------------
-- | The class of which all types that represent an instance of a rol are a member.
class (Newtype rol String, Eq rol) <= RolClass rol where
  getProperty :: forall e. PropertyDef -> (rol ~~> Value) e
  getUnqualifiedProperty :: forall e. LocalName -> (rol ~~> Value) e

genericGetProperty :: forall e. String -> (String ~~> String) e
genericGetProperty pn = getRolMember \(rol :: PerspectRol) -> maybe [] propertyValue (lookup pn (rol_properties rol))

-- | Get the values for the property with the local name that are directly represented on the instance of a rol of type r.
-- | NOTICE that this does not return AspectProperties! These are not qualified with
-- | The name of the rol.
-- | E.g. getUnqualifiedProperty "voornaam"
genericGetUnqualifiedLocalProperty :: forall e. LocalName -> (String ~~> String) e
genericGetUnqualifiedLocalProperty ln = getRolMember \rol -> maybe [] propertyValue (lookup (ln `qualifiedWith` rol) (rol_properties rol))
  where
    qualifiedWith :: LocalName -> PerspectRol -> String
    qualifiedWith ln (PerspectRol {pspType}) = pspType <> "$" <> ln

-- | Get the values for the property with the local name that are directly represented on the instance of a rol of type r, including AspectProperties.
-- | E.g. getUnqualifiedProperty "voornaam"
genericGetUnqualifiedProperty :: forall e. LocalName -> (String ~~> String) e
genericGetUnqualifiedProperty ln = getRolMember $ getUnQualifiedPropertyFromPerspectRol ln

getUnQualifiedPropertyFromPerspectRol :: forall e. LocalName -> PerspectRol -> Array String
getUnQualifiedPropertyFromPerspectRol ln rol =
  case findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys $ rol_properties rol) of
    Nothing -> []
    (Just i) -> maybe [] propertyValue (lookup (unsafePartial $ fromJust (index (keys $ rol_properties rol) i)) (rol_properties rol))

-----------------------------------------------------------
-- THE CLASS BINDING
-----------------------------------------------------------
-- | The class that connects two instances of the RolClass. `binder` binds `bound`.
-- | `bound` depends functionally on `binder`. We therefore have four instances.
-- | In other words, with this class we represent for each instance of RolClass to what type of role it can be bound.
class (RolClass binder, RolClass bound) <= Binding binder bound | binder -> bound where
  binding :: forall e. (binder ~~> bound) e

genericBinding :: forall e. ObjectsGetter e
genericBinding = getRolMember \rol -> maybe [] singleton (rol_binding rol)

-----------------------------------------------------------
-- BINNENROL AS INSTANCE
-----------------------------------------------------------
-- | A rolinstance that has the position of the BinnenRol.
newtype BinnenRol = BinnenRol String

derive instance genericRepBinnenRol :: Generic BinnenRol _
derive instance newtypeBinnenRol :: Newtype BinnenRol _
instance eqBinnenRol :: Eq BinnenRol where
  eq (BinnenRol c1) (BinnenRol c2) = c1 == c2

instance rolClassBinnenRol :: RolClass BinnenRol where
  getProperty pn rn = typeWithPerspectivesTypes $ do
    cid <- pure $ deconstructBinnenRol (unwrap rn)
    (mbr :: Maybe PerspectRol) <- getContextMember' context_binnenRol cid
    case mbr of
      Nothing -> pure []
      (Just rol) -> pure $ (maybe [] propertyValue) (lookup (unwrap pn) (rol_properties rol))

  getUnqualifiedProperty ln rn = typeWithPerspectivesTypes $ do
    cid <- pure $ deconstructBinnenRol (unwrap rn)
    (mbr :: Maybe PerspectRol) <- getContextMember' context_binnenRol cid
    case mbr of
      Nothing -> pure []
      (Just rol) -> pure $ getUnQualifiedPropertyFromPerspectRol ln rol

instance bindingBinnenRol :: Binding BinnenRol BuitenRol where
  binding = typeWithPerspectivesTypes genericBinding

-----------------------------------------------------------
-- BUITENROL AS INSTANCE
-----------------------------------------------------------
-- | A rolinstance that has the position of the BuitenRol.
newtype BuitenRol = BuitenRol String

derive instance genericRepBuitenRol :: Generic BuitenRol _
derive instance newtypeBuitenRol :: Newtype BuitenRol _
instance eqBuitenRol :: Eq BuitenRol where
  eq (BuitenRol c1) (BuitenRol c2) = c1 == c2

instance rolClassBuitenRol :: RolClass BuitenRol where
  getProperty = typeWithPerspectivesTypes genericGetProperty
  getUnqualifiedProperty = typeWithPerspectivesTypes genericGetUnqualifiedProperty

instance bindingBuitenRol :: Binding BuitenRol BuitenRol where
  binding = typeWithPerspectivesTypes genericBinding

instance showBuitenRol :: Show BuitenRol where
  show (BuitenRol b) = show b

-----------------------------------------------------------
-- ROLINCONTEXT AS INSTANCE
-----------------------------------------------------------
-- | A rolinstance that has the position of A RolInContext and is bound to another RolInContext.
newtype RolInContext = RolInContext String

derive instance genericRepRolInContext :: Generic RolInContext _
derive instance newtypeRolInContext :: Newtype RolInContext _
instance eqRolInContext :: Eq RolInContext where
  eq (RolInContext c1) (RolInContext c2) = c1 == c2
instance showRolInContext :: Show RolInContext where
  show (RolInContext r) = show r

instance rolClassRolInContext :: RolClass RolInContext where
  getProperty = typeWithPerspectivesTypes genericGetProperty
  getUnqualifiedProperty = typeWithPerspectivesTypes genericGetUnqualifiedProperty

instance bindingRolInContext :: Binding RolInContext RolInContext where
  binding = typeWithPerspectivesTypes genericBinding

-----------------------------------------------------------
-- CONTEXTROL AS INSTANCE
-----------------------------------------------------------
-- | A rolinstance that has the position of A RolInContext and is bound to the BuitenRol of a Context.
newtype ContextRol = ContextRol String

derive instance genericRepContextRol :: Generic ContextRol _
derive instance newtypeContextRol :: Newtype ContextRol _
instance eqContextRol :: Eq ContextRol where
  eq (ContextRol c1) (ContextRol c2) = c1 == c2
instance showContextRol :: Show ContextRol where
  show (ContextRol b) = show b

instance rolClassContextRol :: RolClass ContextRol where
  getProperty = typeWithPerspectivesTypes genericGetProperty
  getUnqualifiedProperty = typeWithPerspectivesTypes genericGetUnqualifiedProperty

instance bindingContextRol :: Binding ContextRol BuitenRol where
  binding = typeWithPerspectivesTypes genericBinding

-----------------------------------------------------------
-- VALUE AS INSTANCE
-----------------------------------------------------------
-- | The general type of the value of a Property-instance. Notice that even though we have four SimpleValue types
-- | in Perspectives, we only represent one of them in Purescript: PBool. In the code of Perspectives, we have no
-- | need for the other types: we use Value instead.
newtype Value = Value String

derive instance genericRepValue :: Generic Value _
derive instance newtypeValue :: Newtype Value _
instance eqValue :: Eq Value where
  eq (Value c1) (Value c2) = c1 == c2
instance showValue :: Show Value where
  show (Value c) = show c
