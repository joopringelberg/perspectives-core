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
  , saveAndConnectRoleInstance
  , removeContextInstance
  , removeRoleInstance
  , removeAllRoleInstances)

  where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (lift)
import Data.Array (nub)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect.Exception (error)
import Foreign.Object (values)
import Perspectives.Assignment.Update (addRol, removeBinding)
import Perspectives.Assignment.Update (saveEntiteit) as Update
import Perspectives.ContextAndRole (context_buitenRol, context_iedereRolInContext, removeRol_gevuldeRollen, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, Updater, assumption)
import Perspectives.Deltas (addContextToTransactie, addCorrelationIdentifiersToTransactie, addRolToTransactie, deleteContextFromTransactie, deleteRolFromTransactie)
import Perspectives.DependencyTracking.Dependency (findBindingRequests, findBinderRequests, findDependencies)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, getPerspectRol, removeEntiteit, saveEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Prelude (bind, discard, join, pure, show, unit, void, ($), (>>=))

-- | These functions are Updaters, too. They do not push deltas like the Updaters that change PerspectEntities, but
-- | they modify other members of Transaction (createdContexts, deletedContexts, createdRoles, deletedRoles).

-- This function saves a previously cached ContextInstance and all its Roles and adds them all to the Transaction
saveContextInstance :: Updater ContextInstance
saveContextInstance id = do
  (ctxt :: PerspectContext) <- lift $ lift $ saveEntiteit id
  addContextToTransactie ctxt
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> saveRoleInstance rol
  (_ :: PerspectRol) <- lift $ lift $ saveEntiteit (context_buitenRol ctxt)
  pure unit

-- | Saves a previously cached Role instance and adds it to the Transaction.
saveRoleInstance :: Updater RoleInstance
saveRoleInstance id = do
  (role@(PerspectRol{_id, context, pspType}) :: PerspectRol) <- lift $ lift $ saveEntiteit id
  addRolToTransactie role

-- | Saves a previously cached Role instance and adds it to the Transaction.
-- | Adds the Role instance to its Context instance and adds a ContextDelta to the Transaction.
saveAndConnectRoleInstance :: Updater RoleInstance
saveAndConnectRoleInstance id = do
  (role@(PerspectRol{_id, context, pspType}) :: PerspectRol) <- lift $ lift $ saveEntiteit id
  void $ addRol context pspType [_id]
  addRolToTransactie role

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- | Removes the ContextInstance both from the cache and from the database and adds it to the Transaction.
-- | We do not add the removed role instances to the Transaction, as the receiving PDR's will recompute
-- | them themselves.
removeContextInstance :: Updater ContextInstance
removeContextInstance id = do
  deleteContextFromTransactie id
  (ctxt :: PerspectContext) <- lift $ lift $ getPerspectContext id
  forWithIndex_ (context_iedereRolInContext ctxt) \rolTypeId instances -> let rolType = (EnumeratedRoleType rolTypeId) in
    for_ instances \rol -> do
      (lift $ lift $ findBinderRequests rol rolType) >>= addCorrelationIdentifiersToTransactie
      (lift $ lift $ findBindingRequests rol) >>= addCorrelationIdentifiersToTransactie
      removeRoleInstance_ rol
  void $ removeRoleInstance_ (context_buitenRol ctxt)
  (_ :: PerspectContext) <- lift $ lift $ removeEntiteit id
  pure unit


-- Removes the rol from the cache and from the database.
-- Removes the rol from the inverse administration of its binding.
-- Removes the rol as binding from all its binders.
removeRoleInstance_ :: RoleInstance -> MonadPerspectivesTransaction PerspectRol
removeRoleInstance_ roleId = do
  originalRole@(PerspectRol{context, gevuldeRollen, binding, pspType}) <- lift $ lift $ (getPerspectRol roleId)

  -- Remove the role instance from all roles that have it as their binding. This will push Deltas.
  forWithIndex_ gevuldeRollen \_ filledRollen ->
    for_ filledRollen \filledRolId -> removeBinding filledRolId

  -- If the removed role instance has a binding, remove the inverse administration from it.
  case binding of
    Nothing -> pure unit
    (Just oldBindingId) -> do -- TODO: push a Delta for the change on removeRol_gevuldeRollen?
      (oldBinding :: PerspectRol) <- lift $ lift $ getPerspectEntiteit oldBindingId
      -- Only then remove the removed role instance as a binding role from the binding.
      Update.saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)

  -- Remove from couchdb, remove from the cache.
  lift $ lift $ (removeEntiteit roleId :: MonadPerspectives PerspectRol)

-- | Removes the role instance from the cache and from the database.
-- | Removes the role instance from the inverse administration of its binding.
-- | Removes the role instance as binding from all its binders.
-- | Does NOT remove the role instance from its context. Use removeRolFromContext for that.
-- | Adds the Role to the Transaction in deletedRoles. We do not push a ContextDelta, as the
-- | receiving PDR's will recompute the effect of destroying this instance on their context
-- | instances themselves.
removeRoleInstance :: Updater RoleInstance
removeRoleInstance pr = do
  r@(PerspectRol{pspType, context, binding}) <- removeRoleInstance_ pr
  deleteRolFromTransactie pr
  -- Find all queries that should be re-run.
  -- All requests for the pspType binder of the binding
  case binding of
    Nothing -> pure unit
    Just bnd -> (lift $ lift $ findBinderRequests bnd pspType) >>= addCorrelationIdentifiersToTransactie
  -- All requests for instances of the roletype of pr for its context.
  mrr <- lift $ lift $ findDependencies (assumption (unwrap context)(unwrap pr))
  case mrr of
    Nothing -> pure unit
    Just rr -> addCorrelationIdentifiersToTransactie rr

-- | Remove all instances of EnumeratedRoleType from the context instance.
-- | Removes all instances from cache, from the database and adds then to deletedRoles in the Transaction.
-- | Does NOT remove the role instances from their context. Use deleteRol for that.
-- | ContextDelta's are not necessary (see removeRoleInstance).
-- | TODO: Implementeer removeAllRoleInstances.
removeAllRoleInstances :: EnumeratedRoleType -> Updater ContextInstance
removeAllRoleInstances et cid = throwError (error $ show (Custom "Implement removeAllRoleInstances!"))
