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

-- | This module defines External Core functions for model:Couchdb.

module Perspectives.Extern.Utilities where

import Control.Monad.Trans.Class (lift)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (MonadPerspectivesQuery)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Prelude (pure, ($))
import Unsafe.Coerce (unsafeCoerce)

-- TODO: verander naar echte gegenereerde identifiers.
genSym :: RoleInstance -> MonadPerspectivesQuery String
genSym _ = pure "geheim"

roleIdentifier :: RoleInstance -> MonadPerspectivesQuery String
roleIdentifier (RoleInstance id) = pure id

systemIdentifier :: RoleInstance -> MonadPerspectivesQuery String
systemIdentifier _ = lift $ lift getSystemIdentifier

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:Utilities$GenSym" {func: unsafeCoerce genSym, nArgs: 0}
  , Tuple "model:Utilities$RoleIdentifier" {func: unsafeCoerce roleIdentifier, nArgs: 0}
  , Tuple "model:Utilities$SystemIdentifier" {func: unsafeCoerce systemIdentifier, nArgs: 0}
  ]
