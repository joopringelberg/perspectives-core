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

module Perspectives.GlobalState where

import Prelude (Unit, pure, bind, unit)

import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DependencyTracking.Dependency (ActiveSupportedEffects, activeSupportedEffects)
import Perspectives.GlobalUnsafeStrMap (clear)

-----------------------------------------------------------
-- | CLEARING STATE OUTSIDE PERSPECTIVESSTATE
-- | Some state is kept outside PerspectivesState, to preventy cyclic type dependencies.
-- | Even though this state is defined in other modules, we have a function here to clear it.
-- |
-- | The other caches are:
-- | * [ActionCache](Perspectives.Assignment.ActionCache.html#t:ActionCache), that stores compiled Action rules;
-- | * [ActiveSupportedEffects](Perspectives.DependencyTracking.Dependency.html#t:ActiveSupportedEffects), that stores effectful functions to implement the FRP pattern for clients of the Perspectives Distributed Runtime
-----------------------------------------------------------
clearEffectulFunctionCaches :: MonadPerspectives Unit
clearEffectulFunctionCaches = do
  _ <- pure clearActiveSupportedEffects
  pure unit
  where
    clearActiveSupportedEffects :: ActiveSupportedEffects
    clearActiveSupportedEffects = clear activeSupportedEffects
