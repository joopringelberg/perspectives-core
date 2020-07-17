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

module Perspectives.Assignment.SerialiseAsDeltas where

import Data.Array.NonEmpty (fromArray)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Foreign.Object (lookup)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (###>>), (##>>))
import Perspectives.Deltas (addDelta)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (bottom, context, roleType, roleType_)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, externalRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), externalRoleType)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Types.ObjectGetters (propertyIsInPerspectiveOf, roleIsInPerspectiveOf)
import Prelude (Unit, bind, discard, pure, unit, when, ($))

serialisedAsDeltasFor :: ContextInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor cid userId = do
  userType <- lift2 $ roleType_ userId
  systemUser <- lift2 (userId ##>> bottom)
  serialisedAsDeltasFor_ cid systemUser (ENR userType)

serialisedAsDeltasFor_:: ContextInstance -> RoleInstance -> RoleType -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor_ cid userId userType = do
  PerspectContext{universeContextDelta, pspType, rolInContext} <- lift2 $ getPerspectContext cid
  addDelta $ DeltaInTransaction {users: [userId], delta: universeContextDelta}
  -- Serialise the external role.
  serialiseRoleInstance cid (externalRoleType pspType) (externalRole cid)
  -- Now for each role, decide if the user may see it.
  -- If so, add a UniverseRoleDelta and a ContextDelta.
  forWithIndex_ rolInContext \roleTypeId roleInstances' -> do
    allowed <- lift2 (userType ###>> roleIsInPerspectiveOf (ENR $ EnumeratedRoleType roleTypeId))
    if allowed
      then do
        case fromArray roleInstances' of
          Nothing -> pure unit
          Just roleInstances -> do
            for_ roleInstances (serialiseRoleInstance cid (EnumeratedRoleType roleTypeId))
      else pure unit

  where
  serialiseRoleInstance :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction Unit
  serialiseRoleInstance cid' roleTypeId roleInstance = do
    PerspectRol{binding, properties, universeRoleDelta, contextDelta, bindingDelta, propertyDeltas} <- lift2 $ getPerspectRol roleInstance
    addDelta $ DeltaInTransaction { users: [userId], delta: universeRoleDelta}
    addDelta  $ DeltaInTransaction { users: [userId], delta: contextDelta }
    case binding of
      Nothing -> pure unit
      Just b -> do
        c <- lift2 (b ##>> context)
        typeOfBinding <- lift2 (b ##>> roleType)
        shouldBeSent <- lift2 (userType ###>> roleIsInPerspectiveOf (ENR typeOfBinding))
        -- On adding the delta, we check whether it has been serialised before in this transaction.
        when shouldBeSent do
          -- TODO. Serialiseer de context niet als de ander er al een rol bij speelt!
          serialisedAsDeltasFor_ c userId userType
          case bindingDelta of
            Nothing -> pure unit
            Just bd -> addDelta $ DeltaInTransaction {users: [userId], delta: bd }
    -- For each set of Property Values, add a RolePropertyDelta if the user may see it.
    forWithIndex_ properties \propertyTypeId values -> do
      propAllowed <- lift2 (userType ###>> propertyIsInPerspectiveOf (ENP (EnumeratedPropertyType propertyTypeId)))
      if propAllowed
        then case lookup propertyTypeId propertyDeltas of
          Nothing -> pure unit
          Just deltas -> for_ deltas \pd -> addDelta $ DeltaInTransaction {users: [userId], delta: pd}
        else pure unit
