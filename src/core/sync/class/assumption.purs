-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Sync.Class.Assumption where

import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (Assumption)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RolePropertyDelta(..), ContextDelta(..))

-- | Class `DeltaAssumption` abstracts over Deltas to provide a function that turns a Delta into an Assumption.
class DeltaAssumption d where
  assumption :: d -> Assumption

instance contextDeltaAssumption :: DeltaAssumption ContextDelta where
  assumption (ContextDelta{contextInstance, roleType}) = Tuple (unwrap contextInstance) (unwrap roleType)

instance roleBindingDeltaAssumption :: DeltaAssumption RoleBindingDelta where
  assumption (RoleBindingDelta{filled}) = Tuple (unwrap filled) "model://perspectives.domains#System$Role$binding"

instance rolePropertyDeltaAssumption :: DeltaAssumption RolePropertyDelta where
  assumption (RolePropertyDelta{id, property}) = Tuple (unwrap id) (unwrap property)
