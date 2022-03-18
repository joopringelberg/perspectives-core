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
import Perspectives.Assignment.SentenceCompiler (CompiledSentence)
import Perspectives.CoreTypes (Updater, type (~~>))
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new, peek, poke, filterKeys)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType, StateIdentifier, DomeinFileId(..))
import Prelude (Unit, const, unit)

type ContextStateCache = GLStrMap CompiledContextState

type CompiledContextState =
  { query :: (ContextInstance ~~> Value)
  , objectGetter :: Maybe (ContextInstance ~~> RoleInstance)
  , automaticOnEntry :: Map RoleType (Updater ContextInstance)
  , automaticOnExit :: Map RoleType (Updater ContextInstance)
  , notifyOnEntry :: Map RoleType (CompiledSentence ContextInstance)
  , notifyOnExit :: Map RoleType (CompiledSentence ContextInstance)
  , perspectivesOnEntry :: Map RoleType { properties :: Array PropertyType, selfOnly :: Boolean, isSelfPerspective :: Boolean}
  }

-- | A global store of SupportedEffect-s
-- | This index cannot be part of the PerspectivesState. The compiler loops on it.
contextStateCache :: ContextStateCache
contextStateCache = new unit

cacheCompiledContextState :: StateIdentifier -> CompiledContextState -> CompiledContextState
cacheCompiledContextState a u = const u (poke contextStateCache (unwrap a) u)

retrieveCompiledContextState :: StateIdentifier -> (Maybe CompiledContextState)
retrieveCompiledContextState a = peek contextStateCache (unwrap a)

-- | Remove, from both caches, all compiled states from a particular domain.
clearModelStates :: DomeinFileId -> Unit
clearModelStates (DomeinFileId s) = let
  _ = filterKeys isQualifiedWithDomein contextStateCache
  _ = filterKeys isQualifiedWithDomein roleStateCache
  in unit

type RoleStateCache = GLStrMap CompiledRoleState

type CompiledRoleState =
  { query :: (RoleInstance ~~> Value)
  , objectGetter :: Maybe (RoleInstance ~~> RoleInstance)
  , automaticOnEntry :: Map RoleType CompiledAutomaticAction
  , automaticOnExit :: Map RoleType CompiledAutomaticAction
  , notifyOnEntry :: Map RoleType CompiledNotification
  , notifyOnExit :: Map RoleType CompiledNotification
  , perspectivesOnEntry :: Map RoleType CompiledStateDependentPerspective
  }

roleStateCache :: RoleStateCache
roleStateCache = new unit

cacheCompiledRoleState :: StateIdentifier -> CompiledRoleState -> CompiledRoleState
cacheCompiledRoleState a u = const u (poke roleStateCache (unwrap a) u)

retrieveCompiledRoleState :: StateIdentifier -> (Maybe CompiledRoleState)
retrieveCompiledRoleState a = peek roleStateCache (unwrap a)

type CompiledAutomaticAction = {updater :: Updater RoleInstance, contextGetter :: RoleInstance ~~> ContextInstance}
type CompiledNotification = {compiledSentence :: (CompiledSentence RoleInstance), contextGetter :: RoleInstance ~~> ContextInstance}
type CompiledStateDependentPerspective =
  { contextGetter :: RoleInstance ~~> ContextInstance
  , properties :: Array PropertyType
  , selfOnly :: Boolean
  , isSelfPerspective :: Boolean
}
