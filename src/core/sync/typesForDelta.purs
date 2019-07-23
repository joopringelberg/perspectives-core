module Perspectives.TypesForDeltas where

-----------------------------------------------------------
-- DELTA
-----------------------------------------------------------
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Generic (defaultOptions, genericEncode)
import Foreign.Generic.Class (class GenericEncode)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (class Show, ($))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- DELTA
-----------------------------------------------------------
newtype Delta = Delta
  { id :: ID
  , memberName :: String
  , value :: Maybe String
  , deltaType :: DeltaType
  , isContext :: Boolean
  }

derive instance genericDelta :: Generic Delta _

instance showDelta :: Show Delta where
  show = genericShow

derive instance eqDelta :: Eq Delta

derive newtype instance writeForeignDelta :: WriteForeign Delta

data Delta' = CD ContextDelta | BD BindingDelta | PD PropertyDelta

-----------------------------------------------------------
-- CONTEXTDELTA
-----------------------------------------------------------
newtype ContextDelta = ContextDelta
  { id :: ContextInstance
  , role :: EnumeratedRoleType
  , instance :: RoleInstance
  , deltaType :: DeltaType
  }

derive instance genericContextDelta :: Generic ContextDelta _

instance showContextDelta :: Show ContextDelta where
  show = genericShow

derive instance eqContextDelta :: Eq ContextDelta

derive newtype instance writeForeignContextDelta :: WriteForeign ContextDelta

-----------------------------------------------------------
-- BINDINGDELTA
-----------------------------------------------------------
newtype BindingDelta = BindingDelta
  { id :: RoleInstance
  , binding :: RoleInstance
  , deltaType :: DeltaType
  }

derive instance genericBindingDelta :: Generic BindingDelta _

instance showBindingDelta :: Show BindingDelta where
  show = genericShow

derive instance eqBindingDelta :: Eq BindingDelta

derive newtype instance writeForeignBindingDelta :: WriteForeign BindingDelta

-----------------------------------------------------------
-- INVERSEBINDINGDELTA
-----------------------------------------------------------
newtype InverseBindingDelta = InverseBindingDelta
  { id :: RoleInstance
  , binding :: RoleInstance
  , role :: EnumeratedRoleType
  , deltaType :: DeltaType
  }

derive instance genericInverseBindingDelta :: Generic InverseBindingDelta _

instance showInverseBindingDelta :: Show InverseBindingDelta where
  show = genericShow

derive instance eqInverseBindingDelta :: Eq InverseBindingDelta

derive newtype instance writeForeignInverseBindingDelta :: WriteForeign InverseBindingDelta

-----------------------------------------------------------
-- PROPERTYDELTA
-----------------------------------------------------------
newtype PropertyDelta = PropertyDelta
  { id :: RoleInstance
  , property :: EnumeratedPropertyType
  , value :: Value
  , deltaType :: DeltaType
  }

derive instance genericPropertyDelta :: Generic PropertyDelta _

instance showPropertyDelta :: Show PropertyDelta where
  show = genericShow

derive instance eqPropertyDelta :: Eq PropertyDelta

derive newtype instance writeForeignPropertyDelta :: WriteForeign PropertyDelta

-----------------------------------------------------------
-- DELTATYPE
-----------------------------------------------------------
data DeltaType = Add | Remove | Change

derive instance genericDeltaType :: Generic DeltaType _
derive instance eqDeltaType :: Eq DeltaType

instance showDeltaType :: Show DeltaType where
  show = genericShow

instance writeForeignDeltaType :: WriteForeign DeltaType where
  writeImpl Add = unsafeToForeign "Add"
  writeImpl Remove = unsafeToForeign "Remove"
  writeImpl Change = unsafeToForeign "Change"

encodeDefault :: forall t a. Generic a t => GenericEncode t => a -> Foreign
encodeDefault = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
