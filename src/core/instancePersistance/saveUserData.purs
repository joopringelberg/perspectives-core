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

module Perspectives.SaveUserData
  ( saveContextInstance
  , saveRoleInstance
  , removeContextInstance
  , removeRoleInstance)

  where

import Control.Monad.State (lift)
import Data.Array (nub)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Foreign.Object (values)
import Perspectives.Assignment.Update (addRol)
import Perspectives.Assignment.Update (saveEntiteit) as Update
import Perspectives.ContextAndRole (context_buitenRol, context_iedereRolInContext, removeRol_gevuldeRollen, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectives, Updater, MonadPerspectivesTransaction)
import Perspectives.Deltas (addContextToTransactie, addRolToTransactie, deleteContextFromTransactie, deleteRolFromTransactie)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Persistent (getPerspectEntiteit, removeEntiteit, saveEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Prelude (bind, discard, join, pure, unit, void, ($))

-- | These functions are Updaters, too. They do not push deltas like the Updaters that change PerspectEntities, but
-- | they modify other members of Transaction (createdContexts, deletedContexts, createdRoles, deletedRoles).

-- This function saves a previously cached ContextInstance and all its Roles and adds them all to the Transaction
saveContextInstance :: Updater ContextInstance
saveContextInstance id = do
  (ctxt :: PerspectContext) <- lift $ lift $ saveEntiteit id
  addContextToTransactie ctxt
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> do
    lift $ lift $ void $ saveEntiteit rol :: MonadPerspectives PerspectRol
  (_ :: PerspectRol) <- lift $ lift $ saveEntiteit (context_buitenRol ctxt)
  pure unit

-- | Saves a previously cached Role instance and adds it to the Transaction.
-- | Adds the Role instance to its Context instance and adds a ContextDelta to the Transaction.
saveRoleInstance :: Updater RoleInstance
saveRoleInstance id = do
  (role@(PerspectRol{_id, context, pspType}) :: PerspectRol) <- lift $ lift $ saveEntiteit id
  void $ addRol context pspType [_id]
  addRolToTransactie role

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- | Remove the ContextInstance both from the cache and from the database and adds it to the Transaction.
-- | We do not add the removed role instances to the Transaction, as the receiving PDR's will recompute
-- | them themselves.
removeContextInstance :: Updater ContextInstance
removeContextInstance id = do
  deleteContextFromTransactie id
  (ctxt :: PerspectContext) <- lift $ lift $ getPerspectEntiteit id
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> removeRoleInstance_ rol
  void $ removeRoleInstance_ (context_buitenRol ctxt)
  (_ :: PerspectContext) <- lift $ lift $ removeEntiteit id
  pure unit


-- Removes the rol from the cache and from the database.
-- Removes the rol from the inverse administration of its binding.
-- Removes the rol as binding from all its binders.
removeRoleInstance_ :: RoleInstance -> MonadPerspectivesTransaction PerspectRol
removeRoleInstance_ roleId = do
  -- Remove from couchdb, remove from the cache.
  originalRole@(PerspectRol{context, gevuldeRollen, binding, pspType}) <- lift $ lift $ removeEntiteit roleId :: MonadPerspectives PerspectRol
  case binding of
    Nothing -> pure unit
    (Just oldBindingId) -> do
      -- Remove this roleinstance as a binding role from its binding.
      (oldBinding :: PerspectRol) <- lift $ lift $ getPerspectEntiteit oldBindingId
      Update.saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)

  -- Now handle all Roles that have this Role as binding.
  forWithIndex_ gevuldeRollen \rol filledRollen ->
    -- for each filledRol instance...
    for_ filledRollen \filledRolId -> do
      -- ... remove the removed Role from that instance.
      (filledRol :: PerspectRol) <- lift $ lift $ getPerspectEntiteit filledRolId
      Update.saveEntiteit filledRolId (removeRol_gevuldeRollen filledRol (rol_pspType originalRole) roleId)
  pure originalRole

-- | Removes the role instance from the cache and from the database.
-- | Removes the role instance from the inverse administration of its binding.
-- | Removes the role instance as binding from all its binders.
-- | Does NOT remove the role instance from its context. Use removeRol for that.
-- | Adds the Role to the Transaction in deletedRoles. We do not push a ContextDelta, as the
-- | receiving PDR's will recompute the effect of destroying this instance on their context
-- | instances themselves.
removeRoleInstance :: Updater RoleInstance
removeRoleInstance pr = do
  r@(PerspectRol{pspType, context}) <- removeRoleInstance_ pr
  deleteRolFromTransactie pr

-- | Remove all instances of EnumeratedRoleType from the context instance.
-- | Removes all instances from cache, from the database and adds then to deletedRoles in the Transaction.
-- | Does NOT remove the role instances from their context. Use deleteRol for that.
-- | ContextDelta's are not necessary (see removeRoleInstance).
-- | TODO: Implementeer removeRole.
removeAllRoleInstances :: EnumeratedRoleType -> Updater ContextInstance
removeAllRoleInstances et cid = pure unit
