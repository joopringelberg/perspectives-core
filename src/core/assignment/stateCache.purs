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

module Perspectives.Assignment.StateCache where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (Updater, type (~~>))
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, Value)
import Perspectives.Representation.TypeIdentifiers (RoleType, StateIdentifier)
import Prelude (const, unit)

type StateCache = GLStrMap CompiledState

-- | A global store of SupportedEffect-s
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
stateCache :: StateCache
stateCache = new unit

cacheCompiledState :: StateIdentifier -> CompiledState -> CompiledState
cacheCompiledState a u = const u (poke stateCache (unwrap a) u)

retrieveCompiledState :: StateIdentifier -> (Maybe CompiledState)
retrieveCompiledState a = peek stateCache (unwrap a)

type CompiledState =
  { query :: (ContextInstance ~~> Value)
  , automaticOnEntry :: Map RoleType (Updater ContextInstance)
  , automaticOnExit :: Map RoleType (Updater ContextInstance)
  }
