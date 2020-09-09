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

module Perspectives.Sync.HandleTransaction where

import Control.Monad.Except (runExcept, runExceptT)
import Data.Array (length)
import Data.Array.NonEmpty (head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Newtype (unwrap)
import Effect.Class.Console (log)
import Foreign.Generic (decodeJSON)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..))
import Perspectives.Assignment.Update (addProperty, addRoleInstancesToContext, deleteProperty, moveRoleInstancesToAnotherContext, removeBinding, removeProperty, setBinding)
import Perspectives.Authenticate (authenticate)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectives, (##=))
import Perspectives.Identifiers (unsafeDeconstructModelName)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Builders (constructEmptyContext)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.Persistent (saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', loadModelIfMissing)
import Perspectives.SaveUserData (removeContextInstance, removeRoleInstance)
import Perspectives.SerializableNonEmptyArray (toNonEmptyArray)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, pure, show, unit, void, when, ($), (+), (<<<), (<>), (>>=))

executeContextDelta :: ContextDelta -> MonadPerspectivesTransaction Unit
executeContextDelta (ContextDelta{deltaType, id: contextId, roleType, roleInstances, destinationContext} ) = do
  log (show deltaType <> " to/from " <> show contextId <> " and " <> show roleInstances)
  case deltaType of
    -- The subject must be allowed to change the role: they must have a perspective on it that includes the verb Change.
    AddRoleInstancesToContext -> addRoleInstancesToContext contextId roleType (unwrap roleInstances)
    MoveRoleInstancesToAnotherContext -> moveRoleInstancesToAnotherContext contextId (unsafePartial $ fromJust destinationContext) roleType (unwrap roleInstances)

executeRoleBindingDelta :: RoleBindingDelta -> MonadPerspectivesTransaction Unit
executeRoleBindingDelta (RoleBindingDelta{id: roleId, binding, deltaType, roleWillBeRemoved}) = do
  log (show deltaType <> " of " <> show roleId <> " (to) " <> show binding)
  case deltaType of
    SetBinding -> void $ setBinding roleId (unsafePartial $ fromJust binding)
    RemoveBinding -> void $ removeBinding roleWillBeRemoved roleId

executeRolePropertyDelta :: RolePropertyDelta -> MonadPerspectivesTransaction Unit
executeRolePropertyDelta (RolePropertyDelta{id, deltaType, values, property}) = do
  log (show deltaType <> " for " <> show id <> " and property " <> show property)
  case deltaType of
    AddProperty -> addProperty [id] property values
    RemoveProperty -> removeProperty [id] property values
    DeleteProperty -> deleteProperty [id] property

-- | Retrieves from the repository the model that holds the ContextType, if necessary.
executeUniverseContextDelta :: UniverseContextDelta -> MonadPerspectivesTransaction Unit
executeUniverseContextDelta (UniverseContextDelta{id, contextType, deltaType}) = do
  log (show deltaType <> " with id " <> show id <> " and with type " <> show contextType)
  case deltaType of
    ConstructEmptyContext -> do
      (exists :: Maybe PerspectContext) <- lift2 $ tryGetPerspectEntiteit id
      when (isNothing exists)
        do
          loadModelIfMissing (unsafeDeconstructModelName $ unwrap contextType)
          void $ runExceptT $ constructEmptyContext id (unwrap contextType) "" (PropertySerialization empty)
    RemoveContextInstance -> removeContextInstance id

-- | Retrieves from the repository the model that holds the RoleType, if necessary.
executeUniverseRoleDelta :: UniverseRoleDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseRoleDelta (UniverseRoleDelta{id, roleType, roleInstances, deltaType}) s = do
  log (show deltaType <> " for/from " <> show id <> " with ids " <> show roleInstances <> " with type " <> show roleType)
  case deltaType of
    ConstructEmptyRole -> do
      loadModelIfMissing (unsafeDeconstructModelName $ unwrap roleType)
      -- find the number of roleinstances in the context.
      offset <- lift2 ((id ##= getEnumeratedRoleInstances roleType) >>= pure <<< length)
      forWithIndex_ (toNonEmptyArray roleInstances) \i roleInstance -> do
        (exists :: Maybe PerspectRol) <- lift2 $ tryGetPerspectEntiteit roleInstance
        when (isNothing exists)
          (void $ constructEmptyRole_ id (offset + i) roleInstance)
    ConstructExternalRole -> do
      externalRole <- pure (head $ toNonEmptyArray roleInstances)
      log ("ConstructExternalRole in " <> show id)
      (exists :: Maybe PerspectRol) <- lift2 $ tryGetPerspectEntiteit externalRole
      when (isNothing exists)
        do
          void $ constructEmptyRole_ id 0 externalRole
          lift2 $ void $ saveEntiteit externalRole
    RemoveRoleInstance -> for_ (toNonEmptyArray roleInstances) removeRoleInstance
    where
      constructEmptyRole_ :: ContextInstance -> Int -> RoleInstance -> MonadPerspectivesTransaction RoleInstance
      constructEmptyRole_ contextInstance i rolInstanceId = do
        role <- pure (PerspectRol defaultRolRecord
              { _id = rolInstanceId
              , pspType = roleType
              , context = contextInstance
              , occurrence = i
              , universeRoleDelta = s
              })
        void $ lift2 $ cacheEntity rolInstanceId role
        pure rolInstanceId

-- | Execute all Deltas in a run that does not distrubute.
executeTransaction :: TransactionForPeer -> MonadPerspectives Unit
executeTransaction t@(TransactionForPeer{deltas}) = void $ (runMonadPerspectivesTransaction'
    false
    (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
    (for_ deltas f))
  where
    f :: SignedDelta -> MonadPerspectivesTransaction Unit
    f s@(SignedDelta{author, encryptedDelta}) = executeDelta $ authenticate (RoleInstance author) encryptedDelta
      where
        executeDelta :: Maybe String -> MonadPerspectivesTransaction Unit
        -- For now, we fail silently on deltas that cannot be authenticated.
        executeDelta Nothing = pure unit
        executeDelta (Just stringifiedDelta) = case runExcept $ decodeJSON stringifiedDelta of
          Right d1 -> executeRolePropertyDelta d1
          Left _ -> case runExcept $ decodeJSON stringifiedDelta of
            Right d2 -> executeRoleBindingDelta d2
            Left _ -> case runExcept $ decodeJSON stringifiedDelta of
              Right d3 -> executeContextDelta d3
              Left _ -> case runExcept $ decodeJSON stringifiedDelta of
                Right d4 -> executeUniverseRoleDelta d4 s
                Left _ -> case runExcept $ decodeJSON stringifiedDelta of
                  Right d5 -> executeUniverseContextDelta d5
                  Left _ -> log ("Failing to parse and execute: " <> stringifiedDelta)
