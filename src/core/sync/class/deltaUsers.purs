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

module Perspectives.Sync.Class.DeltaUsers where

import Data.Array (cons, union)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (ContextDelta(..), RoleBindingDelta(..), RolePropertyDelta(..), UniverseContextDelta(..), UniverseRoleDelta(..))
import Prelude ((+))
class DeltaUsers d where
  users :: d -> Array RoleInstance
  addToTransaction :: d -> Transaction -> Transaction
  transactionCloneWithDelta :: d -> Transaction -> Transaction
  addBase :: Int -> d -> d

instance contextDeltaDeltaUsers :: DeltaUsers ContextDelta where
  users (ContextDelta{users:u}) = u
  addToTransaction d (Transaction tr@{contextDeltas}) = Transaction tr {contextDeltas = union [d] contextDeltas}
  transactionCloneWithDelta d (Transaction tr) = Transaction tr {contextDeltas = [d], roleDeltas = [], propertyDeltas = [], universeContextDeltas = [], universeRoleDeltas = []}
  addBase i (ContextDelta r@{sequenceNumber}) = ContextDelta r {sequenceNumber = sequenceNumber + i}

instance roleBindingDeltaDeltaUsers :: DeltaUsers RoleBindingDelta where
  users (RoleBindingDelta{users:u}) = u
  addToTransaction d (Transaction tr@{roleDeltas}) = Transaction tr {roleDeltas = union [d] roleDeltas}
  transactionCloneWithDelta d (Transaction tr) = Transaction tr {roleDeltas = [d], contextDeltas = [], propertyDeltas = [], universeContextDeltas = [], universeRoleDeltas = []}
  addBase i (RoleBindingDelta r@{sequenceNumber}) = RoleBindingDelta r {sequenceNumber = sequenceNumber + i}

instance rolePropertyDeltaDeltaUsers :: DeltaUsers RolePropertyDelta where
  users (RolePropertyDelta{users:u}) = u
  addToTransaction d (Transaction tr@{propertyDeltas}) = Transaction tr {propertyDeltas = union [d] propertyDeltas}
  transactionCloneWithDelta d (Transaction tr) = Transaction tr {propertyDeltas = [d], roleDeltas = [], contextDeltas = [], universeContextDeltas = [], universeRoleDeltas = []}
  addBase i (RolePropertyDelta r@{sequenceNumber}) = RolePropertyDelta r {sequenceNumber = sequenceNumber + i}

instance deltaUsersUniverseContextDelta :: DeltaUsers UniverseContextDelta where
  users (UniverseContextDelta{users:u}) = u
  addToTransaction d (Transaction tr@{universeContextDeltas}) = Transaction tr {universeContextDeltas = union [d] universeContextDeltas}
  transactionCloneWithDelta d (Transaction tr) = Transaction tr {universeContextDeltas = [d], roleDeltas = [], contextDeltas = [], propertyDeltas = [], universeRoleDeltas = []}
  addBase i (UniverseContextDelta r@{sequenceNumber}) = UniverseContextDelta r {sequenceNumber = sequenceNumber + i}

instance deltaUsersUniverseRoleDelta :: DeltaUsers UniverseRoleDelta where
  users (UniverseRoleDelta{users:u}) = u
  addToTransaction d (Transaction tr@{universeRoleDeltas}) = Transaction tr {universeRoleDeltas = union [d] universeRoleDeltas}
  transactionCloneWithDelta d (Transaction tr) = Transaction tr {universeRoleDeltas = [d], roleDeltas = [], contextDeltas = [], propertyDeltas = [], universeContextDeltas = []}
  addBase i (UniverseRoleDelta r@{sequenceNumber}) = UniverseRoleDelta r {sequenceNumber = sequenceNumber + i}
