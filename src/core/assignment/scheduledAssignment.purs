-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2022 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.ScheduledAssignment where

-- | The execution model of Perspectives requires that destructive operations be performed
-- | only after all monotonic operations have been executed.
-- | As modellers can write automatic actions that do not order these types of operations according
-- | to these rules, we have to catch and postpone the destructive operations.
-- | Type ScheduledAssignment represents such operations.

import Prelude

import Data.Array (filter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (RoleType)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Utilities (class PrettyPrint)

data ScheduledAssignment =
  -- The RoleType is the AuthorizedRole: a role of kind ContextRole that the modifying user is authorized for.
  ContextRemoval ContextInstance (Maybe RoleType)
  | RoleRemoval RoleInstance
  -- The first RoleInstance has its binding modified; the second RoleInstance, if present, is the new binding.
  | RoleUnbinding RoleInstance (Maybe RoleInstance) (Maybe SignedDelta)

derive instance genericScheduledAssignment :: Generic ScheduledAssignment _

instance showScheduledAssignment :: Show ScheduledAssignment where show = genericShow

instance prettyPrintScheduledAssignment :: PrettyPrint ScheduledAssignment where
  prettyPrint' t sa = show sa

instance eqScheduledAssignment :: Eq ScheduledAssignment where
  eq = genericEq

contextsToBeRemoved :: Array ScheduledAssignment -> Array ContextInstance
contextsToBeRemoved assignments = unsafePartial getContext <$> filter isContextRemoval assignments
  where
    getContext :: Partial => ScheduledAssignment -> ContextInstance
    getContext (ContextRemoval cid _) = cid

    isContextRemoval :: ScheduledAssignment -> Boolean
    isContextRemoval (ContextRemoval _ _) = true
    isContextRemoval _ = false
