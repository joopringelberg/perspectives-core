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

-- | The functions in this module build contexts and roles from a description.
-- | The resulting contexts and roles are fully integrated in the sense of the five
-- | basic responsibilities of the PDR:
-- | PERSISTENCE
-- | SYNCHRONISATION
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER

module Perspectives.Instances.CreateRole where


import Control.Monad.Writer (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.Assignment.Update (getSubject)
import Perspectives.Authenticate (signDelta)
import Perspectives.ContextAndRole (defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (###=))
import Perspectives.Deltas (addCreatedRoleToTransaction)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.ObjectGetters (contextType_)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType, cacheEntity)
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.SerializableNonEmptyArray (singleton) as SNEA
import Perspectives.StrippedDelta (stripResourceSchemes)
import Perspectives.Types.ObjectGetters (roleAspectsClosure)
import Perspectives.TypesForDeltas (UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (bind, discard, pure, void, ($))
import Simple.JSON (writeJSON)

-- | `localName` should be the local name of the roleType.
-- | The role instance is cached.
constructEmptyRole ::
  ContextInstance ->
  EnumeratedRoleType ->
  Int ->
  RoleInstance ->
  MonadPerspectivesTransaction PerspectRol
constructEmptyRole contextInstance roleType i rolInstanceId = do
  subject <- getSubject
  allTypes <- lift (roleType ###= roleAspectsClosure)
  contextType <- lift $ contextType_ contextInstance
  delta <- signDelta 
    (writeJSON $ stripResourceSchemes $ UniverseRoleDelta
      { id: contextInstance
      , contextType
      , roleInstances: (SNEA.singleton rolInstanceId)
      , roleType
      , authorizedRole: Nothing
      , deltaType: ConstructEmptyRole
      , subject })
  role <- pure (PerspectRol defaultRolRecord
    { _id = takeGuid $ unwrap rolInstanceId
    , id = rolInstanceId
    , pspType = roleType
    , allTypes = allTypes
    , context = contextInstance
    , occurrence = i
    , universeRoleDelta = delta
    , states = [StateIdentifier $ unwrap roleType]
    })
  void $ lift $ cacheEntity rolInstanceId role
  addCreatedRoleToTransaction rolInstanceId
  pure role
