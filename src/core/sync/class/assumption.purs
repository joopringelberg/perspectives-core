module Perspectives.Sync.Class.Assumption where

import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (Assumption)
import Perspectives.TypesForDeltas (BindingDelta(..), PropertyDelta(..), RoleDelta(..))

class DeltaAssumption d where
  assumption :: d -> Assumption

instance roleDeltaAssumption :: DeltaAssumption RoleDelta where
  assumption (RoleDelta{id, role}) = Tuple (unwrap id) (unwrap role)

instance bindingDeltaAssumption :: DeltaAssumption BindingDelta where
  assumption (BindingDelta{id, binding}) = Tuple (unwrap id) "model:Perspectives$Role$binding"

instance propertyDeltaAssumption :: DeltaAssumption PropertyDelta where
  assumption (PropertyDelta{id, property}) = Tuple (unwrap id) (unwrap property)
