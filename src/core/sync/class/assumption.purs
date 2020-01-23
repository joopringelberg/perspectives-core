-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Sync.Class.Assumption where

import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (Assumption)
import Perspectives.TypesForDeltas (RoleDelta(..), PropertyDelta(..), ContextDelta(..))

-- | Class `DeltaAssumption` abstracts over Deltas to provide a function that turns a Delta into an Assumption.
class DeltaAssumption d where
  assumption :: d -> Assumption

instance roleDeltaAssumption :: DeltaAssumption ContextDelta where
  assumption (ContextDelta{id, roleType}) = Tuple (unwrap id) (unwrap roleType)

instance bindingDeltaAssumption :: DeltaAssumption RoleDelta where
  assumption (RoleDelta{id, binding}) = Tuple (unwrap id) "model:System$Role$binding"

instance propertyDeltaAssumption :: DeltaAssumption PropertyDelta where
  assumption (PropertyDelta{id, property}) = Tuple (unwrap id) (unwrap property)
