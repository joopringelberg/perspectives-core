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
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.CollectAffectedContexts (aisInContextDelta, aisInPropertyDelta, aisInRoleDelta, lift2)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, addRol_property, changeContext_me, changeRol_binding, changeRol_isMe, context_me, context_rolInContext, deleteContext_rolInContext, deleteRol_property, modifyContext_rolInContext, removeRol_binding, removeRol_gevuldeRollen, removeRol_property, rol_binding, rol_context, rol_id, rol_isMe, rol_pspType, setContext_rolInContext, setRol_property)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, Updater)
import Perspectives.Deltas (addContextDelta, addCorrelationIdentifiersToTransactie, addPropertyDelta, addRoleDelta, addUniverseRoleDelta)
import Perspectives.DependencyTracking.Dependency (findBinderRequests, findBindingRequests, findPropertyRequests, findRoleRequests)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Persistent (class Persistent, getPerspectEntiteit, getPerspectRol, getPerspectContext)
import Perspectives.Persistent (saveEntiteit) as Instances
import Perspectives.Representation.Class.Cacheable (EnumeratedPropertyType, EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Perspectives.TypesForDeltas (ContextDelta(..), DeltaType(..), RoleBindingDelta(..), RolePropertyDelta(..), UniverseRoleDelta(..))

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
-- UPDATE A ROLE (ADD OR REMOVE A BINDING)
-- All mutations on a binding should handle RULE TRIGGERING and QUERY UPDATES
-- for both ways to traverse a binding.
-- They should also take care of PERSISTENCE of the binding role and
-- the bound role. SYNCHRONISATION should be taken care of by a RoleBindingDelta.
-----------------------------------------------------------
-- | The first argument represents the role instance that receives the new binding.
-- | The second argument represents the new binding.
-- | PERSISTENCE of binding role, old binding and new binding.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for both the new and the old binding.
-- | QUERY UPDATES for `binding <roleId`, `binder <TypeOfRoleId>` for both the old binding and the new binding.
-- | CURRENTUSER for roleId and its context.
setBinding :: RoleInstance -> (RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance))
setBinding roleId (newBindingId :: RoleInstance) = do
  (originalRole :: PerspectRol) <- lift2 $ getPerspectEntiteit roleId
  saveEntiteit roleId (changeRol_binding newBindingId originalRole)

  (lift2 $ findBinderRequests newBindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
  (lift2 $ findBindingRequests roleId) >>= addCorrelationIdentifiersToTransactie

  newBinding@(PerspectRol{isMe}) <- lift2 $ getPerspectEntiteit newBindingId
  -- Handle isMe (on the binding role) and me (on its context).
  if isMe
    then do
      modifiedRole <- lift2 $ getPerspectEntiteit roleId
      saveEntiteit roleId (changeRol_isMe modifiedRole isMe)
      -- set roleId to be the value of Me of its context.
      setMe (rol_context modifiedRole) (Just roleId)
    else pure unit

  -- Handle inverse binding.
  case rol_binding originalRole of
    Nothing -> pure unit
    (Just (oldBindingId :: RoleInstance)) -> do
      (lift2 $ findBinderRequests oldBindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
      -- Remove this roleinstance as a binding role from the old binding.
      (oldBinding@(PerspectRol{isMe:oldIsMe}) :: PerspectRol) <- lift2 $ getPerspectEntiteit oldBindingId
      saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)
      -- if the oldBinding has isMe and the new binding has not, than remove me from the context.
      if oldIsMe && not isMe
        then do
          modifiedRole <- lift2 $ getPerspectEntiteit roleId
          setMe (rol_context modifiedRole) Nothing
        else pure unit

  -- Add this roleinstance as a binding role for the new binding.
  saveEntiteit newBindingId (addRol_gevuldeRollen newBinding (rol_pspType originalRole) roleId)

  delta@(RoleBindingDelta{users}) <- aisInRoleDelta $ RoleBindingDelta
                { id : roleId
                , binding: Just newBindingId
                , oldBinding: rol_binding originalRole
                , deltaType: Change
                , users: []
                , sequenceNumber: 0
                }
  addRoleDelta delta

  -- If the type of the role has kind UserRole, we add a new user to the context. This user should have access to
  -- this context. We will generate Deltas so his PDR can build it from scratch, according to his perspective.
  (EnumeratedRole{kindOfRole}) <- lift2 $ getEnumeratedRole (rol_pspType originalRole)
  if kindOfRole == UserRole && not isMe
    then (rol_context originalRole) `serialisedAsDeltasFor` roleId
    else pure unit
  pure users

-- | Removes the binding R of the rol, if any.
-- | Removes the rol as value of 'gevuldeRollen' for psp:Rol$binding from the binding R.
-- | Modifies the Role instance.
-- | PERSISTENCE of binding role and old binding.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for the old binding.
-- | QUERY UPDATES for `binding <roleId>` and `binder <TypeOfRoleId>`.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | CURRENTUSER for roleId and its context.
removeBinding :: (RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance))
removeBinding = removeBinding_ false

removeBinding_ :: Boolean -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
removeBinding_ roleWillBeRemoved roleId = do
  (originalRole :: PerspectRol) <- lift2 $ getPerspectEntiteit roleId
  users' <- case rol_binding originalRole of
    Nothing -> pure []
    Just oldBindingId -> do
      (lift2 $ findBinderRequests oldBindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
      delta@(RoleBindingDelta{users}) <- aisInRoleDelta $ RoleBindingDelta
                  { id : roleId
                  , binding: (rol_binding originalRole)
                  , oldBinding: (rol_binding originalRole)
                  , deltaType: Remove
                  , users: []
                  , sequenceNumber: 0
                  }
      when (not roleWillBeRemoved)
        (do
          (lift2 $ findBindingRequests roleId) >>= addCorrelationIdentifiersToTransactie
          saveEntiteit roleId (changeRol_isMe (removeRol_binding originalRole) false))
      ctxt <- lift2 $ getPerspectContext $ rol_context originalRole
      if (context_me ctxt == (Just roleId))
        then setMe (rol_context originalRole) Nothing
        else pure unit
      addRoleDelta delta
      pure users

  -- Handle inverse binding.
  case rol_binding originalRole of
    Nothing -> pure unit
    (Just oldBindingId) -> do
      -- Remove this roleinstance as a binding role from the old binding.
      (oldBinding :: PerspectRol) <- lift2 $ getPerspectEntiteit oldBindingId
      saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)
  pure users'

-----------------------------------------------------------
-- UPDATE A CONTEXT (ADDING OR REMOVING ROLE INSTANCES)
-----------------------------------------------------------
type RoleUpdater = ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))

-- | Modifies the context instance by adding the given role instances.
-- | Notice that this function does neither cache nor save the rolInstances themselves.
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
addRoleInstancesToContext :: ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))
addRoleInstancesToContext contextId rolName rolInstances = do
  (pe :: PerspectContext) <- lift2 $ getPerspectContext contextId
  changedContext <- pure (modifyContext_rolInContext pe rolName (flip union rolInstances))
  roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
  case find rol_isMe roles of
    Nothing -> saveEntiteit contextId changedContext
    Just me -> do
      (lift2 $ findRoleRequests contextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
      saveEntiteit contextId (changeContext_me changedContext (Just (rol_id me)))
  for_ rolInstances \roleInstance -> do
    delta@(ContextDelta{users}) <- aisInContextDelta $ ContextDelta
                { id : contextId
                , roleType: rolName
                , deltaType: Add
                , roleInstance
                , users: []
                , sequenceNumber: 0
                }
    addContextDelta delta
    addUniverseRoleDelta $ UniverseRoleDelta
        { id: roleInstance
        , roleType: rolName
        , deltaType: Add
        , users
        , sequenceNumber: 0
      }
  (lift2 $ findRoleRequests contextId rolName) >>= addCorrelationIdentifiersToTransactie

-- | Modifies the context instance by detaching the given role instances.
-- | Notice that this function does neither uncache nor unsave the rolInstances
-- | themselves. Instead, use removeRoleInstance.
-- | Does not touch the binding of any of the role instances.
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
removeRoleInstancesFromContext :: ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))
removeRoleInstancesFromContext = removeRoleInstancesFromContext_ false

removeRoleInstancesFromContext_ :: Boolean -> ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))
removeRoleInstancesFromContext_ contextWillBeRemoved contextId rolName rolInstances = do
  for_ rolInstances \roleInstance -> do
    -- TODO. Waarschijnlijk is de volgorde van de deltas hier verkeerd.
    delta@(ContextDelta{users}) <- aisInContextDelta $ ContextDelta
      { id : contextId
      , roleType: rolName
      , deltaType: Remove
      , roleInstance
      , users: []
      , sequenceNumber: 0
      }
    -- TODO. Ik denk dat de ContextDelta overbodig is als de context zelf weggegooid wordt.
    addContextDelta delta
    addUniverseRoleDelta $ UniverseRoleDelta
        { id: roleInstance
        , roleType: rolName
        , deltaType: Remove
        , users
        , sequenceNumber: 0
      }
  (pe :: PerspectContext) <- lift2 $ getPerspectContext contextId
  (lift2 $ findRoleRequests contextId rolName) >>= addCorrelationIdentifiersToTransactie
  roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
  when (not contextWillBeRemoved)
    (do
      changedContext <- pure (modifyContext_rolInContext pe rolName (flip difference rolInstances))
      case find rol_isMe roles of
        Nothing -> saveEntiteit contextId changedContext
        Just me -> do
          (lift2 $ findRoleRequests contextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
          saveEntiteit contextId (changeContext_me changedContext Nothing))

-- | Modifies the context instance by removing all instances of the role.
-- | Notice that this function does NOT handle PERSISTENCE or SYNCHRONISATION for the
-- | rolInstances themselves
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
-- | NOTE: deleteRoleFromContextInstance could be written in terms of removeRoleInstancesFromContext
deleteRoleFromContextInstance :: ContextInstance -> EnumeratedRoleType -> MonadPerspectivesTransaction Unit
deleteRoleFromContextInstance contextId rolName = do
  (pe :: PerspectContext) <- lift2 $ getPerspectContext contextId
  for_ (context_rolInContext pe rolName) \roleInstance -> do
    delta@(ContextDelta{users}) <- aisInContextDelta $ ContextDelta
                { id : contextId
                , roleType: rolName
                , deltaType: Remove
                , roleInstance
                , users: []
                , sequenceNumber: 0
                }
    addContextDelta delta
    addUniverseRoleDelta $ UniverseRoleDelta
        { id: roleInstance
        , roleType: rolName
        , deltaType: Remove
        , users
        , sequenceNumber: 0
      }

  (lift2 $ findRoleRequests contextId rolName) >>= addCorrelationIdentifiersToTransactie
  changedContext <- pure (deleteContext_rolInContext pe rolName)
  roles <- traverse (lift <<< lift <<< getPerspectRol) (context_rolInContext pe rolName)
  case find rol_isMe roles of
    Nothing -> saveEntiteit contextId changedContext
    Just me -> do
      (lift2 $ findRoleRequests contextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
      saveEntiteit contextId (changeContext_me changedContext Nothing)

-- | Modifies the context instance by replacing the role's instances by the given instances.
-- | Notice that this function does not remove the rolInstances themselves, nor
-- | add them to the Transaction. Instead, use removeAllRoleInstances for that.
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
setRoleInstancesInContext :: ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))
setRoleInstancesInContext contextId rolName rolInstances = do
  (pe :: PerspectContext) <- lift2 $ getPerspectContext contextId
  for_ (context_rolInContext pe rolName) \roleInstance -> do
    delta@(ContextDelta{users}) <- aisInContextDelta $ ContextDelta
                { id : contextId
                , roleType: rolName
                , deltaType: Remove
                , roleInstance
                , users: []
                , sequenceNumber: 0
                }
    addContextDelta delta
    addUniverseRoleDelta $ UniverseRoleDelta
        { id: roleInstance
        , roleType: rolName
        , deltaType: Remove
        , users
        , sequenceNumber: 0
      }
  (lift2 $ findRoleRequests contextId rolName) >>= addCorrelationIdentifiersToTransactie
  roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
  me <- pure $ rol_id <$> find rol_isMe roles
  saveEntiteit contextId (changeContext_me (setContext_rolInContext pe rolName (rolInstances :: Array RoleInstance)) me)
  (lift2 $ findRoleRequests contextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie

  for_ rolInstances \roleInstance -> do
    delta@(ContextDelta{users}) <- aisInContextDelta $ ContextDelta
                { id : contextId
                , roleType: rolName
                , deltaType: Add
                , roleInstance
                , users: []
                , sequenceNumber: 0
                }
    addContextDelta delta
    addUniverseRoleDelta $ UniverseRoleDelta
        { id: roleInstance
        , roleType: rolName
        , deltaType: Add
        , users
        , sequenceNumber: 0
      }

-- | Detach the role instances from their current context and attach them to the new context.
-- | This is not just a convenience function. The combination of removeRoleInstancesFromContext and addRoleInstancesToContext would add UniverseRoleDeltas, which we don't need here.
-- | PERSISTENCE of both context instances.
-- | SYNCHRONISATION by two ContextDeltas (no UniverseRoleDeltas needed!).
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
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
      (lift2 $ findRoleRequests destinationContextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
      saveEntiteit originContextId (changeContext_me (modifyContext_rolInContext origin rolName (flip difference rolInstances)) Nothing)
      (lift2 $ findRoleRequests originContextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
  for_ rolInstances \rolInstance -> do
    (aisInContextDelta $ ContextDelta
                { id : originContextId
                , roleType: rolName
                , deltaType: Delete
                , roleInstance: rolInstance
                , users: []
                , sequenceNumber: 0
                }) >>= addContextDelta
    (aisInContextDelta $ ContextDelta
                { id : destinationContextId
                , roleType: rolName
                , deltaType: Add
                , roleInstance: rolInstance
                , users: []
                , sequenceNumber: 0
                }) >>= addContextDelta
  (lift2 $ findRoleRequests destinationContextId rolName) >>= addCorrelationIdentifiersToTransactie
  (lift2 $ findRoleRequests originContextId rolName) >>= addCorrelationIdentifiersToTransactie

-----------------------------------------------------------
-- UPDATE A ROLE (ADD OR REMOVE PROPERTY VALUES)
-----------------------------------------------------------
type PropertyUpdater = Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))

-- | Modify the role instance with the new property values.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
addProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
addProperty rids propertyName values = for_ rids \rid -> do
  (pe :: PerspectRol) <- lift2 $ getPerspectEntiteit rid
  saveEntiteit rid (addRol_property pe propertyName values)
  for_ values \val ->
    aisInPropertyDelta (RolePropertyDelta
                { id : rid
                , property: propertyName
                , deltaType: Add
                , value: Just val
                , users: []
                , sequenceNumber: 0
                }) >>= addPropertyDelta
  (lift2 $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie

-- | Modify the role instance with the new property values.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
removeProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
removeProperty rids propertyName values = for_ rids \rid -> do
  (pe :: PerspectRol) <- lift2 $ getPerspectEntiteit rid
  saveEntiteit rid (removeRol_property pe propertyName values)
  for_ values \val -> aisInPropertyDelta (RolePropertyDelta
              { id : rid
              , property: propertyName
              , deltaType: Remove
              , value: Just val
              , users: []
              , sequenceNumber: 0
              }) >>= addPropertyDelta
  (lift2 $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie

-- | Delete all property values from the role for the EnumeratedPropertyType.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by PropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
deleteProperty :: Array RoleInstance -> EnumeratedPropertyType -> MonadPerspectivesTransaction Unit
deleteProperty rids propertyName = for_ rids \rid -> do
  (pe :: PerspectRol) <- lift2 $ getPerspectEntiteit rid
  saveEntiteit rid (deleteRol_property pe propertyName)
  aisInPropertyDelta (RolePropertyDelta
              { id : rid
              , property: propertyName
              , deltaType: Delete
              , value: Nothing
              , users: []
              , sequenceNumber: 0
              }) >>= addPropertyDelta
  (lift2 $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie

-- | Modify the role instance with the new property values.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
-- TODO. Dit kan efficienter, als je alle waarden ineens zet.
setProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
setProperty rids propertyName values = for_ rids \rid -> do
  (pe :: PerspectRol) <- lift2 $ getPerspectEntiteit rid
  saveEntiteit rid (setRol_property pe propertyName values)
  for_ values \value -> do
    delta <- pure $ RolePropertyDelta
                { id : rid
                , property: propertyName
                , deltaType: Change
                , value: Just value
                , users: []
                , sequenceNumber: 0
                }
    aisInPropertyDelta delta >>= addPropertyDelta
  (lift2 $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie

-----------------------------------------------------------
-- LOCAL SAVENTITEIT
-----------------------------------------------------------
-- Save the entity in cache and in couchdb.
saveEntiteit :: forall a i r. GenericEncode r => Generic a r => Persistent a i => i -> a -> MonadPerspectivesTransaction Unit
saveEntiteit rid rol = do
  -- We can use cacheEntity instead of cacheEntity because a) we know there is
  -- a cached entiteit, in this context of updating, so b) we do not accidentally overwrite
  -- the version number (because we don't create entities in this file).
  lift2 $ void $ cacheEntity rid rol
  lift2 $ void $ Instances.saveEntiteit rid

-----------------------------------------------------------
-- SET ME
-----------------------------------------------------------
-- | Even though we modify the ContextInstance, we do not push a Delta.
-- | This is because the value of Me is indexed and never communicated with other users.
setMe :: ContextInstance -> Maybe RoleInstance -> MonadPerspectivesTransaction Unit
setMe cid me = do
  ctxt <- lift2 $ getPerspectContext cid
  saveEntiteit cid (changeContext_me ctxt me)
  (lift2 $ findRoleRequests cid (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
