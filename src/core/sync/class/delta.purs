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

module Perspectives.Sync.Class.DeltaClass where

import Perspectives.TypesForDeltas (ContextDelta(..), RoleBindingDelta(..), RolePropertyDelta(..), UniverseContextDelta(..), UniverseRoleDelta(..))
import Prelude ((+))

class DeltaClass d where
  addBase :: Int -> d -> d
  setIndex :: Int -> d -> d
  getSequenceNumber :: d -> Int

instance contextDeltaDeltaUsers :: DeltaClass ContextDelta where
  addBase i (ContextDelta r@{sequenceNumber}) = ContextDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (ContextDelta r) = ContextDelta r {sequenceNumber = i}
  getSequenceNumber (ContextDelta{sequenceNumber}) = sequenceNumber

instance roleBindingDeltaDeltaUsers :: DeltaClass RoleBindingDelta where
  addBase i (RoleBindingDelta r@{sequenceNumber}) = RoleBindingDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (RoleBindingDelta r) = RoleBindingDelta r {sequenceNumber = i}
  getSequenceNumber (RoleBindingDelta{sequenceNumber}) = sequenceNumber

instance rolePropertyDeltaDeltaUsers :: DeltaClass RolePropertyDelta where
  addBase i (RolePropertyDelta r@{sequenceNumber}) = RolePropertyDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (RolePropertyDelta r) = RolePropertyDelta r {sequenceNumber = i}
  getSequenceNumber (RolePropertyDelta{sequenceNumber}) = sequenceNumber

instance deltaUsersUniverseContextDelta :: DeltaClass UniverseContextDelta where
  addBase i (UniverseContextDelta r@{sequenceNumber}) = UniverseContextDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (UniverseContextDelta r) = UniverseContextDelta r {sequenceNumber = i}
  getSequenceNumber (UniverseContextDelta{sequenceNumber}) = sequenceNumber

instance deltaUsersUniverseRoleDelta :: DeltaClass UniverseRoleDelta where
  addBase i (UniverseRoleDelta r@{sequenceNumber}) = UniverseRoleDelta r {sequenceNumber = sequenceNumber + i}
  setIndex i (UniverseRoleDelta r) = UniverseRoleDelta r {sequenceNumber = i}
  getSequenceNumber (UniverseRoleDelta{sequenceNumber}) = sequenceNumber
