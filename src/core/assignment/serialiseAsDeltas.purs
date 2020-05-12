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

import Data.Array.NonEmpty (fromArray, singleton)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (###>>), (##>>))
import Perspectives.Deltas (addContextDelta, addPropertyDelta, addRoleDelta, addUniverseContextDelta, addUniverseRoleDelta)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (bottom, context, roleType, roleType_)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, externalRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), externalRoleType)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.Types.ObjectGetters (propertyIsInPerspectiveOf, roleIsInPerspectiveOf)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), RolePropertyDeltaType(..), RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, pure, unit, when, ($))

serialisedAsDeltasFor :: ContextInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor cid userId = do
  userType <- lift2 $ roleType_ userId
  systemUser <- lift2 (userId ##>> bottom)
  serialisedAsDeltasFor_ cid systemUser (ENR userType)

serialisedAsDeltasFor_:: ContextInstance -> RoleInstance -> RoleType -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor_ cid userId userType = do
  (PerspectContext{pspType, rolInContext}) <- lift2 $ getPerspectContext cid
  addUniverseContextDelta $ UniverseContextDelta
    { id: cid
    , contextType: pspType
    , deltaType: ConstructEmptyContext
    , users: [userId]
    , sequenceNumber: 0
    }
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
            addContextDelta $ ContextDelta
              { id : cid
              , roleType: (EnumeratedRoleType roleTypeId)
              , deltaType: AddRoleInstancesToContext
              , roleInstances: SerializableNonEmptyArray roleInstances
              , users: [userId]
              , sequenceNumber: 0
              , destinationContext: Nothing
              }
      else pure unit

  where
  serialiseRoleInstance :: ContextInstance -> EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction Unit
  serialiseRoleInstance cid' roleTypeId roleInstance = do
    addUniverseRoleDelta $ UniverseRoleDelta
      { id: cid'
      , roleInstances: (SerializableNonEmptyArray $ singleton roleInstance)
      , roleType: roleTypeId
      , deltaType: ConstructEmptyRole
      , users: [userId]
      , sequenceNumber: 0
      }
    (PerspectRol{binding, properties}) <- lift2 $ getPerspectRol roleInstance
    case binding of
      Nothing -> pure unit
      Just b -> do
        c <- lift2 (b ##>> context)
        typeOfBinding <- lift2 (b ##>> roleType)
        shouldBeSent <- lift2 (userType ###>> roleIsInPerspectiveOf (ENR typeOfBinding))
        -- On adding the delta, we check whether it has been serialised before in this transaction.
        when shouldBeSent do
          serialisedAsDeltasFor_ c userId userType
          addRoleDelta (RoleBindingDelta
            { id : roleInstance
            , binding: binding
            , oldBinding: Nothing
            , deltaType: SetBinding
            , roleWillBeRemoved: false
            , users: [userId]
            , sequenceNumber: 0
            })
    -- For each set of Property Values, add a RolePropertyDelta if the user may see it.
    forWithIndex_ properties \propertyTypeId values -> do
      propAllowed <- lift2 (userType ###>> propertyIsInPerspectiveOf (ENP (EnumeratedPropertyType propertyTypeId)))
      if propAllowed
        then addPropertyDelta $ RolePropertyDelta
          { id : roleInstance
          , property: (EnumeratedPropertyType propertyTypeId)
          , deltaType: SetProperty
          , values: values
          , users: [userId]
          , sequenceNumber: 0
          }
        else pure unit
