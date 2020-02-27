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

-- | The functions in this module save and remove contexts and roles.
-- | These updates fully take care of the basic responsibilities of the PDR:
-- | PERSISTENCE
-- | SYNCHRONISATION
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER
-- | except for saveContextInstance (no CURRENTUSER, nor SYNCHRONISATION)

module Perspectives.SaveUserData
  ( saveContextInstance
  , removeContextInstance
  , removeRoleInstance
  , removeAllRoleInstances)

  where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (nub)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (values)
import Perspectives.Assignment.Update (removeBinding, removeBinding_, removeRoleInstancesFromContext_)
import Perspectives.CollectAffectedContexts (aisInContextDelta, aisInRoleDelta, lift2)
import Perspectives.ContextAndRole (context_buitenRol, context_iedereRolInContext, context_pspType)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, Updater)
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addUniverseContextDelta)
import Perspectives.DependencyTracking.Dependency (findBinderRequests, findBindingRequests)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectContext, getPerspectRol, removeEntiteit, saveEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Perspectives.TypesForDeltas (ContextDelta(..), DeltaType(..), RoleBindingDelta(..), UniverseContextDelta(..))
import Prelude (Unit, bind, discard, join, pure, show, unit, void, ($), (>>=))

-- | This function takes care of
-- | PERSISTENCE
-- | QUERY UPDATES
-- | RULE TRIGGERING
-- | but nothing else.
saveContextInstance :: Updater ContextInstance
saveContextInstance id = do
  (ctxt :: PerspectContext) <- lift2 $ saveEntiteit id
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> do
    (PerspectRol{_id, binding, pspType, gevuldeRollen}) <- lift2 $ saveEntiteit rol
    case binding of
      Nothing -> pure unit
      Just b -> (lift2 $ findBinderRequests b pspType) >>= addCorrelationIdentifiersToTransactie
    forWithIndex_ gevuldeRollen \_ instances -> for_ instances \binder ->
      (lift2 $ findBindingRequests binder) >>= addCorrelationIdentifiersToTransactie
    -- Users from other contexts:
    void $ aisInRoleDelta $ RoleBindingDelta
      { id: rol
      , binding: binding
      , oldBinding: Nothing
      , deltaType: Add
      , users: []}
    -- Users from this context (let's trigger all rules).
    void $ aisInContextDelta $ ContextDelta
      { id
      , roleType: pspType
      , roleInstance: _id
      , deltaType: Add
      , users: []}

  (_ :: PerspectRol) <- lift2 $ saveEntiteit (context_buitenRol ctxt)
  -- For roles with a binding equal to R: detect the binder <RoleType> requests for R
  -- For roles that are bound by role R: detect the binding requests for R.
  pure unit

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- | Removes the ContextInstance both from the cache and from the database and adds it to the Transaction.
-- | This function is complete w.r.t. the five responsibilities.
removeContextInstance :: Updater ContextInstance
removeContextInstance id = do
  (ctxt :: PerspectContext) <- lift $ lift $ getPerspectContext id
  (Tuple _ users) <- runWriterT $ do
     for_ (iedereRolInContext ctxt) (removeRoleInstance_ true)
     removeRoleInstance_ true (context_buitenRol ctxt)
  (_ :: PerspectContext) <- lift $ lift $ removeEntiteit id
  addUniverseContextDelta $ UniverseContextDelta
    { id
    , contextType: context_pspType ctxt
    , deltaType: Remove
    , users}
  pure unit

-- | Collects the union of the user role instances that occurr in the bindings.
removeRoleInstance_ :: Boolean -> RoleInstance -> WriterT (Array RoleInstance) MonadPerspectivesTransaction Unit
removeRoleInstance_ contextWillBeRemoved roleId = do
  originalRole@(PerspectRol{context, gevuldeRollen, binding, pspType}) <- lift $ lift2 $ (getPerspectRol roleId)
  lift $ removeRoleInstancesFromContext_ contextWillBeRemoved  context pspType [roleId]

  -- Remove the role instance from all roles that have it as their binding. This will push Deltas.
  forWithIndex_ gevuldeRollen \_ filledRollen ->
    for_ filledRollen \filledRolId -> do
      (lift $ removeBinding filledRolId) >>= tell
  -- Remove the binding.
  (lift $ removeBinding_ true roleId) >>= tell
  -- Remove from couchdb, remove from the cache.
  void $ lift $ lift2 $ (removeEntiteit roleId :: MonadPerspectives PerspectRol)

-- | Calls removeBinding for the role instance prior to removing it.
-- | Calls removeBinding on all role instances that have this role as their binding.
-- | Calls removeRoleInstancesFromContext.
-- | This function is complete w.r.t. the five responsibilities.
-- | The opposite of this function creates a role instance first and then adds it to a context: [createAndAddRoleInstance](Perspectives.Instances.Builders.html#t:createAndAddRoleInstance).
removeRoleInstance :: RoleInstance -> MonadPerspectivesTransaction Unit
removeRoleInstance roleId = void $ runWriterT $ removeRoleInstance_ false roleId

-- | Remove all instances of EnumeratedRoleType from the context instance.
-- | Removes all instances from cache, from the database and adds then to deletedRoles in the Transaction.
-- | Does NOT remove the role instances from their context. Use deleteRoleFromContextInstance for that.
-- | ContextDelta's are not necessary (see removeRoleInstance).
-- | TODO: Implementeer removeAllRoleInstances.
removeAllRoleInstances :: EnumeratedRoleType -> Updater ContextInstance
removeAllRoleInstances et cid = throwError (error $ show (Custom "Implement removeAllRoleInstances!"))
