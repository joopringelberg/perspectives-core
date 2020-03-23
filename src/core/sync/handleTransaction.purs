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

import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Array (sort, length)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Identity (Identity)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Effect.Class.Console (log)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..))
import Perspectives.Assignment.Update (addProperty, addRoleInstancesToContext, deleteProperty, moveRoleInstancesToAnotherContext, removeBinding, removeProperty, setBinding, setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, OrderedDelta(..), MonadPerspectives, (##=))
import Perspectives.Instances.Builders (constructEmptyContext, constructEmptyRole_)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.SaveUserData (removeContextInstance, removeRoleInstance)
import Perspectives.SerializableNonEmptyArray (toNonEmptyArray)
import Perspectives.Sync.Class.DeltaClass (getSequenceNumber)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, discard, pure, unit, void, ($), bind, (>>=), (<<<), (+), (<>), show)

executeContextDelta :: ContextDelta -> MonadPerspectivesTransaction Unit
executeContextDelta (ContextDelta{deltaType, id: contextId, roleType, roleInstances, destinationContext} ) = case deltaType of
  AddRoleInstancesToContext -> do
    log ("addRoleInstancesToContext " <> show contextId <> " and " <> show roleInstances)
    addRoleInstancesToContext contextId roleType (unwrap roleInstances)
  MoveRoleInstancesToAnotherContext -> moveRoleInstancesToAnotherContext contextId (unsafePartial $ fromJust destinationContext) roleType (unwrap roleInstances)
  NoOp -> pure unit

executeRoleBindingDelta :: RoleBindingDelta -> MonadPerspectivesTransaction Unit
executeRoleBindingDelta (RoleBindingDelta{id: roleId, binding, deltaType, roleWillBeRemoved}) = case deltaType of
  SetBinding -> do
    log ("setBinding of " <> show roleId <> " to " <> show (unsafePartial $ fromJust binding))
    void $ setBinding roleId (unsafePartial $ fromJust binding)
  RemoveBinding -> void $ removeBinding roleWillBeRemoved roleId

executeRolePropertyDelta :: RolePropertyDelta -> MonadPerspectivesTransaction Unit
executeRolePropertyDelta (RolePropertyDelta{id, deltaType, values, property}) = case deltaType of
  AddProperty -> addProperty [id] property values
  RemoveProperty -> removeProperty [id] property values
  DeleteProperty -> deleteProperty [id] property
  SetProperty -> do
    log ("setProperty " <> show property <> " of " <> show id <> " to " <> show values)
    setProperty [id] property values

executeUniverseContextDelta :: UniverseContextDelta -> MonadPerspectivesTransaction Unit
executeUniverseContextDelta (UniverseContextDelta{id, contextType, deltaType}) = case deltaType of
  ConstructEmptyContext -> do
    log ("constructEmptyContext " <> show id <> " with type " <> show contextType)
    void $ constructEmptyContext id (unwrap contextType) "" (PropertySerialization empty)
  RemoveContextInstance -> removeContextInstance id

executeUniverseRoleDelta :: UniverseRoleDelta -> MonadPerspectivesTransaction Unit
executeUniverseRoleDelta (UniverseRoleDelta{id, roleType, roleInstances, deltaType}) = case deltaType of
  ConstructEmptyRole -> do
    -- find the number of roleinstances in the context.
    offset <- lift2 ((id ##= getRole roleType) >>= pure <<< length)
    forWithIndex_ (toNonEmptyArray roleInstances) \i roleInstance -> do
      log ("constructEmptyRole_ in " <> show id <> " with id " <> show roleInstance)
      void $ constructEmptyRole_ roleType id (offset + i) roleInstance
  RemoveRoleInstance -> for_ (toNonEmptyArray roleInstances) removeRoleInstance

collectDeltas :: Transaction -> Array OrderedDelta
collectDeltas t = sort $ unwrap $ execWriterT (collectDeltas_ t)

collectDeltas_ :: Transaction -> WriterT (Array OrderedDelta) Identity Unit
collectDeltas_ (Transaction{contextDeltas, roleDeltas, propertyDeltas, universeContextDeltas, universeRoleDeltas}) = do
  for_ contextDeltas \d -> tell [
    (OrderedDelta
      ((\_ -> executeContextDelta d) :: Unit -> MonadPerspectivesTransaction Unit)
      (getSequenceNumber d))]
  for_ roleDeltas \d -> tell [
    (OrderedDelta
      ((\_ -> executeRoleBindingDelta d) :: Unit -> MonadPerspectivesTransaction Unit)
      (getSequenceNumber d))]
  for_ propertyDeltas \d -> tell [
    (OrderedDelta
      ((\_ -> executeRolePropertyDelta d) :: Unit -> MonadPerspectivesTransaction Unit)
      (getSequenceNumber d))]
  for_ universeContextDeltas \d -> tell [
    (OrderedDelta
      ((\_ -> executeUniverseContextDelta d) :: Unit -> MonadPerspectivesTransaction Unit)
      (getSequenceNumber d))]
  for_ universeRoleDeltas \d -> tell [
    (OrderedDelta
      ((\_ -> executeUniverseRoleDelta d) :: Unit -> MonadPerspectivesTransaction Unit)
      (getSequenceNumber d))]

-- | Execute all Deltas in a run that does not distrubute.
executeTransaction :: Transaction -> MonadPerspectives Unit
executeTransaction t = void $ runMonadPerspectivesTransaction' false (for_ (collectDeltas t) \(OrderedDelta d _) -> d unit)
