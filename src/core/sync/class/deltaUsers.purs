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

import Data.Array (cons)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (ContextDelta(..), RoleBindingDelta(..), RolePropertyDelta(..))

class DeltaUsers d where
  users :: d -> Array RoleInstance
  addToTransaction :: d -> Transaction -> Transaction
  transactionCloneWithDelta :: d -> Transaction -> Transaction

instance contextDeltaDeltaUsers :: DeltaUsers ContextDelta where
  users (ContextDelta{users:u}) = u
  addToTransaction d (Transaction tr@{contextDeltas}) = Transaction tr {contextDeltas = cons d contextDeltas}
  transactionCloneWithDelta d (Transaction tr) = Transaction tr {contextDeltas = [d], roleDeltas = [], propertyDeltas = []}

instance roleBindingDeltaDeltaUsers :: DeltaUsers RoleBindingDelta where
  users (RoleBindingDelta{users:u}) = u
  addToTransaction d (Transaction tr@{roleDeltas}) = Transaction tr {roleDeltas = cons d roleDeltas}
  transactionCloneWithDelta d (Transaction tr) = Transaction tr {roleDeltas = [d], contextDeltas = [], propertyDeltas = []}

instance rolePropertyDeltaDeltaUsers :: DeltaUsers RolePropertyDelta where
  users (RolePropertyDelta{users:u}) = u
  addToTransaction d (Transaction tr@{propertyDeltas}) = Transaction tr {propertyDeltas = cons d propertyDeltas}
  transactionCloneWithDelta d (Transaction tr) = Transaction tr {propertyDeltas = [d], roleDeltas = [], contextDeltas = []}
