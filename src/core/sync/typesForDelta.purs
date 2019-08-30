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
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (class Show, ($))
import Simple.JSON (class WriteForeign)

-----------------------------------------------------------
-- ROLEDELTA
-----------------------------------------------------------
newtype RoleDelta = RoleDelta
  { id :: ContextInstance
  , role :: EnumeratedRoleType
  , instance :: RoleInstance
  , deltaType :: DeltaType
  }

derive instance genericRoleDelta :: Generic RoleDelta _

instance showRoleDelta :: Show RoleDelta where
  show = genericShow

derive instance eqRoleDelta :: Eq RoleDelta

derive newtype instance writeForeignRoleDelta :: WriteForeign RoleDelta

-----------------------------------------------------------
-- BINDINGDELTA
-----------------------------------------------------------
newtype BindingDelta = BindingDelta
  { id :: RoleInstance
  , binding :: Maybe RoleInstance
  , deltaType :: DeltaType
  }

derive instance genericBindingDelta :: Generic BindingDelta _

instance showBindingDelta :: Show BindingDelta where
  show = genericShow

derive instance eqBindingDelta :: Eq BindingDelta

derive newtype instance writeForeignBindingDelta :: WriteForeign BindingDelta

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
