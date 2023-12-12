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

module Perspectives.Sync.DeltaInTransaction where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Simple.JSON (class ReadForeign, class WriteForeign)

-- | `users` will not always be model:System$PerspectivesSystem$User instances.
newtype DeltaInTransaction = DeltaInTransaction {users :: Array RoleInstance, delta :: SignedDelta}

derive instance genericRepDeltaInTransaction :: Generic DeltaInTransaction _

instance showDeltaInTransaction :: Show DeltaInTransaction where
  show = genericShow

derive newtype instance WriteForeign DeltaInTransaction
derive newtype instance ReadForeign DeltaInTransaction

instance eqDeltaInTransaction :: Eq DeltaInTransaction where
  eq = genericEq

derive instance newtypeDeltaInTransaction :: Newtype DeltaInTransaction _

instance prettyPrintDeltaInTransaction :: PrettyPrint DeltaInTransaction where
  prettyPrint' t (DeltaInTransaction r) = "DeltaInTransaction " <> prettyPrint' (t <> "  ") r
