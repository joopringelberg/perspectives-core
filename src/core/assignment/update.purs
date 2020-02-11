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

-- | The functions in this module modify contexts and roles. All these functions
-- |  * cache the results
-- |  * save the results
-- |  * add Delta's to the current Transaction
-- | They fall in three categories:
-- |  * modification of a Context, by changing its roles.
-- |  * modification of a Role by changing its binding
-- |  * modification of a Role by changing its property values
-- | The two binding-changing functions recompute the special `isMe` property (a Boolean value indicating whether the role represents the user).
-- | The context-changing functions recompute the special `me` role (it is the role instance that represents the user).
-- | IMPORTANT: the functions that change the context never save nor cache the role instances that are involved in the
-- | change.

module Perspectives.Assignment.Update where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (difference, find, union)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign.Generic.Class (class GenericEncode)
import Perspectives.CollectAffectedContexts (aisInRoleDelta, aisInContextDelta, aisInPropertyDelta)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, addRol_property, changeContext_me, changeRol_binding, changeRol_isMe, context_me, context_rolInContext, deleteContext_rolInContext, deleteRol_property, modifyContext_rolInContext, removeRol_binding, removeRol_gevuldeRollen, removeRol_property, rol_binding, rol_context, rol_id, rol_isMe, rol_pspType, setContext_rolInContext, setRol_property)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, Updater)
import Perspectives.Deltas (addContextDelta, addCorrelationIdentifiersToTransactie, addPropertyDelta, addRoleDelta)
import Perspectives.DependencyTracking.Dependency (findBinderRequests)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Persistent (class Persistent, getPerspectEntiteit, getPerspectRol, getPerspectContext)
import Perspectives.Persistent (saveEntiteit) as Instances
import Perspectives.Representation.Class.Cacheable (EnumeratedPropertyType, EnumeratedRoleType, cacheOverwritingRevision)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.TypesForDeltas (ContextDelta(..), DeltaType(..), RolePropertyDelta(..), RoleBindingDelta(..))

{-
Om een door de gebruiker aangebrachte wijziging door te voeren, moet je:
  - een Delta maken;
  - die versturen aan alle betrokkenen.
    - wat is het type van de context?
    - wie zijn de betrokkenen?
    - welke betrokkenen hebben een Actie met als lijdend voorwerp de entiteit?
    - heeft die Actie een view met de betreffende property?
  - de wijziging doorvoeren op de interne representatie;
  - de consequenties doorvoeren in de triple administratie;
  - de gewijzigde context opslaan;

Om een door een andere gebruiker aangebrachte wijziging door te voeren, moet je:
  - controleren of de author wel gerechtigd is tot de wijziging;
    - in welke rol is de author betrokken bij de context (van de rol)?
    - heeft die rol een actie die de betreffende delta oplevert?
      - past het werkwoord bij de DeltaType?
      - is het lijdend voorwerp de betreffende rol of context?
      - heeft de view op het lijdend voorwerp de relevante property (indien het gaat om een property delta)?
  - de wijziging doorvoeren op de interne representatie;
  - de consequenties doorvoeren in de triple administratie;
  - de gewijzigde context opslaan;
-}
-- | Create update functions on PerspectContext or PerspectRol.
-- | The result is an ObjectsGetter that always returns the (ID of the) Persistent.
-- | Sets up the Bot actions for a Context.

-----------------------------------------------------------
-- UPDATE A BINDING
-----------------------------------------------------------
-- | Changes the binding, and the inverse binding.
-- | The first argument represents the role instance that receives the new binding.
-- | The second argument represents the new binding.
-- | Modifies the Role instance.
-- | Caches and saves the Role instance.
-- | Adds a RoleBindingDelta to the transaction.
-- | Sets isMe on the Role instance if the binding represents the user.
-- | Sets me on the context of the Role instance if the binding represents the user.
setBinding :: RoleInstance -> (Updater RoleInstance)
setBinding roleId (newBindingId :: RoleInstance) = do
  (originalRole :: PerspectRol) <- lift $ lift $ getPerspectEntiteit roleId
  saveEntiteit roleId (changeRol_binding newBindingId originalRole)

  newBinding@(PerspectRol{isMe}) <- lift $ lift $ getPerspectEntiteit newBindingId
  if isMe
    then do
      modifiedRole <- lift $ lift $ getPerspectEntiteit roleId
      saveEntiteit roleId (changeRol_isMe modifiedRole isMe)
      -- set roleId to be the value of Me of its context.
      setMe (rol_context modifiedRole) (Just roleId)
    else pure unit

  -- Handle inverse binding.
  case rol_binding originalRole of
    Nothing -> pure unit
    (Just (oldBindingId :: RoleInstance)) -> do
      -- TODO. It is no longer possible to move from the old binding to this roleInstance.
      -- That means that some query results might be affected.
      -- That is not yet captured in aisInRoleDelta, because it does not know of the old binding.

      -- Remove this roleinstance as a binding role from the old binding.
      (oldBinding@(PerspectRol{isMe:oldIsMe}) :: PerspectRol) <- lift $ lift $ getPerspectEntiteit oldBindingId
      saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)
      -- if the oldBinding has isMe and the new binding has not, than remove me from the context.
      if oldIsMe && not isMe
        then do
          modifiedRole <- lift $ lift $ getPerspectEntiteit roleId
          setMe (rol_context modifiedRole) Nothing
        else pure unit

  -- Add this roleinstance as a binding role for the new binding.
  saveEntiteit newBindingId (addRol_gevuldeRollen newBinding (rol_pspType originalRole) roleId)

  delta <- pure $ RoleBindingDelta
                { id : roleId
                , binding: Just newBindingId
                , deltaType: Change
                , users: []
                }
  aisInRoleDelta delta >>= addRoleDelta

-- | Removes the binding R of the rol, if any.
-- | Removes the rol as value of 'gevuldeRollen' for psp:Rol$binding from the binding R.
-- | Modifies the Role instance.
-- | Caches and saves the Role instance.
-- | Adds a RoleBindingDelta to the transaction.
-- | Sets isMe to Nothing on the Role instance (regardless of whether the binding represents the user).
-- | Removes me from the context of the Role instance if the binding we removed represents the user.
-- TODO. As soon as we introduce multiple values for a binding, we have to adapt this so a binding argument
-- is taken into account, too (we then only remove that binding, not all of them).
-- REFACTOR. Roep om te beginnen aisInRoleDelta aan op de RoleBindingDelta. Dat voegt de geraakte contexten toe.
-- Verwijder pas daarna de binding.
removeBinding :: (Updater RoleInstance)
removeBinding roleId = do
  (originalRole :: PerspectRol) <- lift $ lift $ getPerspectEntiteit roleId
  delta <- pure $ RoleBindingDelta
              { id : roleId
              , binding: (rol_binding originalRole)
              , deltaType: Remove
              , users: []
              }
  case rol_binding originalRole of
    Nothing -> pure unit
    Just oldBindingId -> (lift $ lift $ findBinderRequests oldBindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
  delta' <- aisInRoleDelta delta
  saveEntiteit roleId (changeRol_isMe (removeRol_binding originalRole) false)
  ctxt <- lift $ lift $ getPerspectContext $ rol_context originalRole
  if (context_me ctxt == (Just roleId))
    then setMe (rol_context originalRole) Nothing
    else pure unit
  addRoleDelta delta'

  -- Handle inverse binding.
  case rol_binding originalRole of
    Nothing -> pure unit
    (Just oldBindingId) -> do
      -- Remove this roleinstance as a binding role from the old binding.
      (oldBinding :: PerspectRol) <- lift $ lift $ getPerspectEntiteit oldBindingId
      saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)

-----------------------------------------------------------
-- UPDATE A ROLE
-----------------------------------------------------------
type RoleUpdater = ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))

-- | Modifies the context instance.
-- | Recomputes the `me` role.
-- | Adds Context deltas to the Transaction.
-- | Caches and saves the context instance.
-- | Notice that this function does neither cache nor save the rolInstances themselves.
addRol :: ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))
addRol contextId rolName rolInstances = do
  (pe :: PerspectContext) <- lift $ lift $ getPerspectContext contextId
  changedContext <- pure (modifyContext_rolInContext pe rolName (flip union rolInstances))
  roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
  case find rol_isMe roles of
    Nothing -> saveEntiteit contextId changedContext
    Just me -> saveEntiteit contextId (changeContext_me changedContext (Just (rol_id me)))
  for_ rolInstances \rolInstance -> do
    delta <- pure $ ContextDelta
                { id : contextId
                , roleType: rolName
                , deltaType: Add
                , roleInstance: Just rolInstance
                , users: []
                }
    aisInContextDelta delta >>= addContextDelta

-- | Modifies the context instance.
-- | Recomputes the `me` role.
-- | Adds Context deltas to the Transaction.
-- | Caches and saves the context instance.
-- | Notice that this function does neither uncache nor unsave the rolInstances themselves.
-- | Instead, use removeRoleInstance.
removeRolFromContext :: ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))
removeRolFromContext contextId rolName rolInstances = do
  for_ rolInstances \rolInstance -> do
    delta <- pure $ ContextDelta
                { id : contextId
                , roleType: rolName
                , deltaType: Remove
                , roleInstance: Just rolInstance
                , users: []
                }
    aisInContextDelta delta >>= addContextDelta
  (pe :: PerspectContext) <- lift $ lift $ getPerspectContext contextId
  changedContext <- pure (modifyContext_rolInContext pe rolName (flip difference rolInstances))
  roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
  case find rol_isMe roles of
    Nothing -> saveEntiteit contextId changedContext
    Just me -> saveEntiteit contextId (changeContext_me changedContext Nothing)

-- | Modifies the context instance.
-- | Recomputes the `me` role.
-- | Adds Context deltas to the Transaction.
-- | Caches and saves the context instance.
-- | Notice that this function does not remove the rolInstances themselves, nor
-- | add them to the Transaction.
-- | Instead, use removeAllRoleInstances for that.
deleteRol :: ContextInstance -> EnumeratedRoleType -> MonadPerspectivesTransaction Unit
deleteRol contextId rolName = do
  delta <- pure $ ContextDelta
              { id : contextId
              , roleType: rolName
              , deltaType: Delete
              , roleInstance: Nothing
              , users: []
              }
  aisInContextDelta delta >>= addContextDelta
  (pe :: PerspectContext) <- lift $ lift $ getPerspectContext contextId
  changedContext <- pure (deleteContext_rolInContext pe rolName)
  roles <- traverse (lift <<< lift <<< getPerspectRol) (context_rolInContext pe rolName)
  case find rol_isMe roles of
    Nothing -> saveEntiteit contextId changedContext
    Just me -> saveEntiteit contextId (changeContext_me changedContext Nothing)

-- | Modifies the context instance.
-- | Recomputes the `me` role.
-- | Adds Context deltas to the Transaction.
-- | Caches and saves the context instance.
-- | Notice that this function does not remove the rolInstances themselves, nor
-- | add them to the Transaction.
-- | Instead, use removeAllRoleInstances for that.
setRol :: ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))
setRol contextId rolName rolInstances = do
  (pe :: PerspectContext) <- lift $ lift $ getPerspectContext contextId
  for_ (context_rolInContext pe rolName) \rolInstance -> do
    delta <- pure $ ContextDelta
                { id : contextId
                , roleType: rolName
                , deltaType: Remove
                , roleInstance: Just rolInstance
                , users: []
                }
    aisInContextDelta delta >>= addContextDelta
  roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
  me <- pure $ rol_id <$> find rol_isMe roles
  saveEntiteit contextId (changeContext_me (setContext_rolInContext pe rolName (rolInstances :: Array RoleInstance)) me)
  for_ rolInstances \rolInstance -> do
    delta <- pure $ ContextDelta
                { id : contextId
                , roleType: rolName
                , deltaType: Add
                , roleInstance: Just rolInstance
                , users: []
                }
    aisInContextDelta delta >>= addContextDelta

-- | Detach the role instances from their current context and attach them to the new context.
-- | Modifies both context instances.
-- | Recomputes the `me` role on both.
-- | Adds Context deltas to the Transaction.
-- | Caches and saves the context instances.
-- | Nothing else is changed.
-- TODO: verzamel de affected contexts.
moveRoles :: ContextInstance -> ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))
moveRoles originContextId destinationContextId rolName rolInstances = do
  roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
  me <- pure $ rol_id <$> find rol_isMe roles
  -- me <- pure $ Just $ RoleInstance ""
  origin <- lift $ lift $ getPerspectContext originContextId
  destination <- lift $ lift $ getPerspectContext destinationContextId
  case me of
    Nothing -> do
      saveEntiteit destinationContextId (modifyContext_rolInContext destination rolName (append rolInstances))
      saveEntiteit originContextId (modifyContext_rolInContext origin rolName (flip difference rolInstances))
    Just m -> do
      saveEntiteit destinationContextId (changeContext_me (modifyContext_rolInContext destination rolName (append rolInstances)) me)
      saveEntiteit originContextId (changeContext_me (modifyContext_rolInContext origin rolName (flip difference rolInstances)) Nothing)
  for_ rolInstances \rolInstance -> do
    (aisInContextDelta $ ContextDelta
                { id : originContextId
                , roleType: rolName
                , deltaType: Delete
                , roleInstance: Just rolInstance
                , users: []
                }) >>= addContextDelta
    (aisInContextDelta $ ContextDelta
                { id : destinationContextId
                , roleType: rolName
                , deltaType: Add
                , roleInstance: Just rolInstance
                , users: []
                }) >>= addContextDelta

-----------------------------------------------------------
-- UPDATE A PROPERTY
-----------------------------------------------------------
type PropertyUpdater = Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))

-- | Modify the role instance with the new property values.
-- | Adds PropertyDeltas to the Transaction.
-- | Caches and saves the modified Role instance.
addProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
addProperty rids propertyName values = for_ rids \rid -> do
  (pe :: PerspectRol) <- lift $ lift $ getPerspectEntiteit rid
  saveEntiteit rid (addRol_property pe propertyName values)
  for_ values \val ->
    aisInPropertyDelta (RolePropertyDelta
                { id : rid
                , property: propertyName
                , deltaType: Add
                , value: Just val
                , users: []
                }) >>= addPropertyDelta

-- | Modify the role instance with the new property values.
-- | Adds PropertyDeltas to the Transaction.
-- | Caches and saves the modified Role instance.
removeProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
removeProperty rids propertyName values = for_ rids \rid -> do
  (pe :: PerspectRol) <- lift $ lift $ getPerspectEntiteit rid
  saveEntiteit rid (removeRol_property pe propertyName values)
  for_ values \val -> aisInPropertyDelta (RolePropertyDelta
              { id : rid
              , property: propertyName
              , deltaType: Remove
              , value: Just val
              , users: []
              }) >>= addPropertyDelta

-- | Delete all property values from the role for the EnumeratedPropertyType.
-- | Adds PropertyDeltas to the Transaction.
-- | Caches and saves the modified Role instance.
deleteProperty :: Array RoleInstance -> EnumeratedPropertyType -> MonadPerspectivesTransaction Unit
deleteProperty rids propertyName = for_ rids \rid -> do
  (pe :: PerspectRol) <- lift $ lift $ getPerspectEntiteit rid
  saveEntiteit rid (deleteRol_property pe propertyName)
  aisInPropertyDelta (RolePropertyDelta
              { id : rid
              , property: propertyName
              , deltaType: Delete
              , value: Nothing
              , users: []
              }) >>= addPropertyDelta

-- | Modify the role instance with the new property values.
-- | Adds PropertyDeltas to the Transaction.
-- | Caches and saves the modified Role instance.
-- TODO. Dit kan efficienter, als je alle waarden ineens zet.
setProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
setProperty rids propertyName values = for_ rids \rid -> do
  (pe :: PerspectRol) <- lift $ lift $ getPerspectEntiteit rid
  saveEntiteit rid (setRol_property pe propertyName values)
  for_ values \value -> do
    delta <- pure $ RolePropertyDelta
                { id : rid
                , property: propertyName
                , deltaType: Change
                , value: Just value
                , users: []
                }
    aisInPropertyDelta delta >>= addPropertyDelta

-----------------------------------------------------------
-- LOCAL SAVENTITEIT
-----------------------------------------------------------
-- Save the entity in cache and in couchdb.
saveEntiteit :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectivesTransaction Unit
saveEntiteit rid rol = do
  -- We can use cacheOverwritingRevision instead of cachePreservingRevision because a) we know there is
  -- a cached entiteit, in this context of updating, so b) we do not accidentally overwrite
  -- the version number (because we don't create entities in this file).
  lift $ lift $ void $ cacheOverwritingRevision rid rol
  lift $ lift $ void $ Instances.saveEntiteit rid

-----------------------------------------------------------
-- SET ME
-----------------------------------------------------------
-- | Even though we modify the ContextInstance, we do not push a Delta.
-- | This is because the value of Me is indexed and never communicated with other users.
setMe :: ContextInstance -> Maybe RoleInstance -> MonadPerspectivesTransaction Unit
setMe cid me = do
  ctxt <- lift $ lift $ getPerspectContext cid
  saveEntiteit cid (changeContext_me ctxt me)
