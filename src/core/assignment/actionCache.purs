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

module Perspectives.Assignment.ActionCache where

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (Updater)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType)
import Prelude (unit)

type ActionCache = GLStrMap (Updater ContextInstance)

-- | A global store of SupportedEffect-s
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
actionCache :: ActionCache
actionCache = new unit

cacheAction :: ActionType -> (Updater ContextInstance) -> ActionCache
cacheAction a u = poke actionCache (unwrap a) u

retrieveAction :: ActionType -> (Maybe (Updater ContextInstance))
retrieveAction a = peek actionCache (unwrap a)
