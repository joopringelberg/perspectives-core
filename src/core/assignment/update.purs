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

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (difference, elemIndex, find, null, union)
import Data.Array (head) as ARR
import Data.Array.NonEmpty (NonEmptyArray, toArray, head)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Generic (encodeJSON)
import Foreign.Generic.Class (class GenericEncode)
import Foreign.Object (Object, filterKeys, fromFoldable, insert, lookup, empty)
import Foreign.Object (union) as OBJ
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Authenticate (sign)
import Perspectives.CollectAffectedContexts (aisInPropertyDelta, aisInRoleDelta, lift2, usersWithPerspectiveOnRoleInstance)
import Perspectives.ContextAndRole (addRol_gevuldeRollen, addRol_property, changeContext_me, changeRol_binding, changeRol_isMe, context_me, context_rolInContext, deleteRol_property, modifyContext_rolInContext, removeRol_binding, removeRol_gevuldeRollen, removeRol_property, rol_binding, rol_context, rol_id, rol_isMe, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, Updater, (##>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addDelta)
import Perspectives.DependencyTracking.Dependency (findBinderRequests, findBindingRequests, findPropertyRequests, findRoleRequests)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.Extern.Couchdb (addModelToLocalStore)
import Perspectives.Identifiers (deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getProperty, isMe)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Persistent (class Persistent, getPerspectEntiteit, getPerspectRol, getPerspectContext)
import Perspectives.Persistent (saveEntiteit) as Instances
import Perspectives.Representation.Class.Cacheable (EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..), singleton)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), SubjectOfAction(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))

-----------------------------------------------------------
-- UPDATE A ROLE (ADD OR REMOVE A BINDING)
-- All mutations on a binding should handle RULE TRIGGERING and QUERY UPDATES
-- for both ways to traverse a binding.
-- They should also take care of PERSISTENCE of the binding role and
-- the bound role. SYNCHRONISATION should be taken care of by a RoleBindingDelta.
-----------------------------------------------------------
-- | The first argument represents the role instance that receives the new binding.
-- | The second argument represents the new binding.
-- | If the new binding is in fact bound to the role instance before the operation, this is a no-op without effect.
-- | function, because of createAndAddRoleInstance.
-- | PERSISTENCE of binding role, old binding and new binding.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for both the new and the old binding.
-- | QUERY UPDATES for `binding <roleId`, `binder <TypeOfRoleId>` for both the old binding and the new binding.
-- | CURRENTUSER for roleId and its context.
setBinding :: RoleInstance -> (RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance))
setBinding roleId (newBindingId :: RoleInstance) = do
  (originalRole :: PerspectRol) <- lift2 $ getPerspectEntiteit roleId
  if (rol_binding originalRole == Just newBindingId)
    then pure []
    else setBinding_ roleId newBindingId

-- | The first argument represents the role instance that receives the new binding.
-- | The second argument represents the new binding.
-- | If the new binding is in fact bound to the role instance before the operation, we still have to execute the
-- | function, because of createAndAddRoleInstance.
-- | PERSISTENCE of binding role, old binding and new binding.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for both the new and the old binding.
-- | QUERY UPDATES for `binding <roleId`, `binder <TypeOfRoleId>` for both the old binding and the new binding.
-- | CURRENTUSER for roleId and its context.
setBinding_ :: RoleInstance -> (RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance))
setBinding_ roleId (newBindingId :: RoleInstance) = do
  (originalRole :: PerspectRol) <- lift2 $ getPerspectEntiteit roleId
  saveEntiteit roleId (changeRol_binding newBindingId originalRole)

  (lift2 $ findBinderRequests newBindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
  (lift2 $ findBindingRequests roleId) >>= addCorrelationIdentifiersToTransactie

  newBinding@(PerspectRol{isMe, pspType}) <- lift2 $ getPerspectEntiteit newBindingId
  -- If the type of the new binding is unknown, load the model. There is only one
  -- circumstance that we can have a RoleInstance of an unknown type and that is when
  -- it is the external role of a sys:Model context. This role has a property Url that
  -- we can fetch the model from.
  mDomeinFile <- lift2 $ traverse tryRetrieveDomeinFile (deconstructModelName $ unwrap pspType)
  when (isNothing mDomeinFile)
    (do
      murl <- lift2 (newBindingId ##> getProperty (EnumeratedPropertyType "model:System$Model$External$Url"))
      case murl of
        Nothing -> throwError (error $ "System error: no url found to load model for unknown type " <> (unwrap pspType))
        Just (Value url) -> addModelToLocalStore [url] newBindingId)
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

  subject <- authoringRole
  delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
                { id : roleId
                , binding: Just newBindingId
                , oldBinding: rol_binding originalRole
                , deltaType: SetBinding
                , roleWillBeRemoved: false
                , subject
                }
  users <- aisInRoleDelta delta
  author <- lift2 getUserIdentifier
  signedDelta <- pure $ SignedDelta
    { author
    , encryptedDelta: sign $ encodeJSON $ delta}
  addDelta (DeltaInTransaction { users, delta: signedDelta })
  -- Save the SignedDelta as the bindingDelta in the role.
  (modifiedRole :: PerspectRol) <- lift2 $ getPerspectEntiteit roleId
  saveEntiteit roleId (over PerspectRol (\rl -> rl {bindingDelta = Just signedDelta}) modifiedRole)

  pure users

-- | If the type of the role has kind UserRole and is not the `me` role for its context,
-- | we add a new user to the context. This user should have access to
-- | this context. We will generate Deltas so his PDR can build it from scratch,
-- | according to his perspective.
-- | Notice that in order to establish whether this role represents `usr:Me`,
-- | it needs a binding!
handleNewPeer :: RoleInstance -> MonadPerspectivesTransaction Unit
handleNewPeer roleInstance = do
  PerspectRol{context, pspType} <- lift2 $ getPerspectRol roleInstance
  (EnumeratedRole{kindOfRole}) <- lift2 $ getEnumeratedRole pspType
  me <- lift2 $ isMe roleInstance
  if kindOfRole == UserRole && not me
    then context `serialisedAsDeltasFor` roleInstance
    else pure unit

handleNewPeer_ :: Boolean -> RoleInstance -> MonadPerspectivesTransaction Unit
handleNewPeer_ me roleInstance = do
  PerspectRol{context, pspType} <- lift2 $ getPerspectRol roleInstance
  (EnumeratedRole{kindOfRole}) <- lift2 $ getEnumeratedRole pspType
  if kindOfRole == UserRole && not me
    then context `serialisedAsDeltasFor` roleInstance
    else pure unit

-- | Removes the binding R of the rol, if any.
-- | Removes the rol as value of 'gevuldeRollen' for psp:Rol$binding from the binding R.
-- | Modifies the Role instance.
-- | If the the role instance has in fact no binding before the operation, this is a no-op without effect.
-- | PERSISTENCE of binding role and old binding.
-- | RULE TRIGGERING for `binding <roleId`, `binder <TypeOfRoleId>` for the old binding.
-- | QUERY UPDATES for `binding <roleId>` and `binder <TypeOfRoleId>`.
-- | SYNCHRONISATION by RoleBindingDelta.
-- | CURRENTUSER for roleId and its context.
removeBinding :: Boolean -> RoleInstance -> MonadPerspectivesTransaction (Array RoleInstance)
removeBinding roleWillBeRemoved roleId = do
  (originalRole :: PerspectRol) <- lift2 $ getPerspectEntiteit roleId
  if (isJust $ rol_binding originalRole)
    then do
      users' <- case rol_binding originalRole of
        Nothing -> pure []
        Just oldBindingId -> do
          (lift2 $ findBinderRequests oldBindingId (rol_pspType originalRole)) >>= addCorrelationIdentifiersToTransactie
          subject <- authoringRole
          delta@(RoleBindingDelta r) <- pure $ RoleBindingDelta
                        { id : roleId
                        , binding: (rol_binding originalRole)
                        , oldBinding: (rol_binding originalRole)
                        , deltaType: RemoveBinding
                        , roleWillBeRemoved
                        , subject
                        }
          users <- aisInRoleDelta delta
          author <- lift2 getUserIdentifier
          signedDelta <- pure $ SignedDelta
            { author
            , encryptedDelta: sign $ encodeJSON $ delta}
          addDelta (DeltaInTransaction { users, delta: signedDelta})

          when (not roleWillBeRemoved)
            do
              (lift2 $ findBindingRequests roleId) >>= addCorrelationIdentifiersToTransactie
              saveEntiteit roleId (over PerspectRol (\rl -> rl {bindingDelta = Nothing})
                (changeRol_isMe (removeRol_binding originalRole) false))
          ctxt <- lift2 $ getPerspectContext $ rol_context originalRole
          if (context_me ctxt == (Just roleId))
            then setMe (rol_context originalRole) Nothing
            else pure unit
          pure users

      -- Handle inverse binding.
      case rol_binding originalRole of
        Nothing -> pure unit
        (Just oldBindingId) -> do
          -- Remove this roleinstance as a binding role from the old binding.
          (oldBinding :: PerspectRol) <- lift2 $ getPerspectEntiteit oldBindingId
          saveEntiteit oldBindingId (removeRol_gevuldeRollen oldBinding (rol_pspType originalRole) roleId)
      pure users'
    else pure []

-----------------------------------------------------------
-- UPDATE A CONTEXT (ADDING OR REMOVING ROLE INSTANCES)
-----------------------------------------------------------
type RoleUpdater = ContextInstance -> EnumeratedRoleType -> (Updater (Array RoleInstance))

-- | Modifies the context instance by adding the given role instances.
-- | Notice that this function does neither cache nor save the rolInstances themselves.
-- | If all rolInstances are part of the context before the operation, this is a no-op without any effects.
-- | PERSISTENCE of the context instance.
-- | SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
addRoleInstancesToContext :: ContextInstance -> EnumeratedRoleType -> (Updater (NonEmptyArray RoleInstance))
addRoleInstancesToContext contextId rolName rolInstances = do
  (pe :: PerspectContext) <- lift2 $ getPerspectContext contextId
  when (not $ null (toArray rolInstances `difference` context_rolInContext pe rolName))
    do
      changedContext <- lift2 (modifyContext_rolInContext pe rolName (flip union (toArray rolInstances)))
      roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
      -- PERSISTENCE
      case find rol_isMe roles of
        Nothing -> saveEntiteit contextId changedContext
        Just me -> do
          (lift2 $ findRoleRequests contextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
          -- CURRENTUSER
          saveEntiteit contextId (changeContext_me changedContext (Just (rol_id me)))

      -- Guarantees RULE TRIGGERING because contexts with a vantage point are added to
      -- the transaction, too.
      users <- usersWithPerspectiveOnRoleInstance contextId rolName (head rolInstances)
      -- SYNCHRONISATION
      subject <- authoringRole
      for_ roles \(PerspectRol r@{_id, universeRoleDelta}) -> do
        pure unit
        addDelta (DeltaInTransaction { users, delta: universeRoleDelta})
        delta <- pure $ SignedDelta
          { author: _.author $ unwrap universeRoleDelta
          , encryptedDelta: sign $ encodeJSON $ ContextDelta
            { id : contextId
            , roleType: rolName
            , deltaType: AddRoleInstancesToContext
            , roleInstances: (singleton _id)
            , destinationContext: Nothing
            , subject
            } }
        addDelta $ DeltaInTransaction {users, delta}
        saveEntiteit _id $ PerspectRol r { contextDelta = delta }
      -- QUERY UPDATES
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
removeRoleInstancesFromContext :: ContextInstance -> EnumeratedRoleType -> (Updater (NonEmptyArray RoleInstance))
removeRoleInstancesFromContext contextId rolName rolInstances = do
  -- Guarantees RULE TRIGGERING because contexts with a vantage point are added to
  -- the transaction, too.
  users <- usersWithPerspectiveOnRoleInstance contextId rolName (head rolInstances)
  subject <- authoringRole
-- SYNCHRONISATION
  author <- lift2 getUserIdentifier
  addDelta $ DeltaInTransaction
    { users
    , delta: SignedDelta
      { author
      , encryptedDelta: sign $ encodeJSON $ UniverseRoleDelta
        { id: contextId
        , roleInstances: (SerializableNonEmptyArray rolInstances)
        , roleType: rolName
        , deltaType: RemoveRoleInstance
        , subject } }}

  -- QUERY UPDATES.
  (lift2 $ findRoleRequests contextId rolName) >>= addCorrelationIdentifiersToTransactie
  roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
  (pe :: PerspectContext) <- lift2 $ getPerspectContext contextId
  changedContext <- lift2 (modifyContext_rolInContext pe rolName (flip difference (toArray rolInstances)))
  -- PERSISTENCE.
  case find rol_isMe roles of
    Nothing -> saveEntiteit contextId changedContext
    Just me -> do
      (lift2 $ findRoleRequests contextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
      -- CURRENTUSER.
      saveEntiteit contextId (changeContext_me changedContext Nothing)

-- | Detach the role instances from their current context and attach them to the new context.
-- | This is not just a convenience function. The combination of removeRoleInstancesFromContext and addRoleInstancesToContext would add UniverseRoleDeltas, which we don't need here.
-- | If all rolInstances are part of the destination context before the operation, this is a no-op without any effects.
-- | PERSISTENCE of both context instances.
-- | SYNCHRONISATION by two ContextDeltas (no UniverseRoleDeltas needed!).
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER for contextId and one of rolInstances.
-- TODO. De enige manier om deze functie aan te passen lijkt een nieuwe ContextDelta te maken en mee te nemen in de ContextDelta met MoveRoleInstancesToAnotherContext. Aan de ontvangende kant moet die nieuwe ContextDelta dan in de verplaatste rol worden gezet op de plek van 'contextDelta'.
moveRoleInstancesToAnotherContext :: ContextInstance -> ContextInstance -> EnumeratedRoleType -> (Updater (NonEmptyArray RoleInstance))
moveRoleInstancesToAnotherContext originContextId destinationContextId rolName rolInstances = pure unit
-- moveRoleInstancesToAnotherContext originContextId destinationContextId rolName rolInstances = do
--   roles <- traverse (lift <<< lift <<< getPerspectRol) rolInstances
--   me <- pure $ rol_id <$> find rol_isMe roles
--   -- me <- pure $ Just $ RoleInstance ""
--   origin <- lift $ lift $ getPerspectContext originContextId
--   destination <- lift $ lift $ getPerspectContext destinationContextId
--   subject <- subjectForRoleInstance (head rolInstances)
--   when (not null (toArray rolInstances `difference` context_rolInContext destination rolName))
--     do
--       case me of
--         Nothing -> do
--           (lift2 $ modifyContext_rolInContext destination rolName (append (toArray rolInstances))) >>= saveEntiteit destinationContextId
--           (lift2 $ modifyContext_rolInContext origin rolName (flip difference (toArray rolInstances))) >>= saveEntiteit originContextId
--         Just m -> do
--           destination' <- lift2 (modifyContext_rolInContext destination rolName (append (toArray rolInstances)))
--           saveEntiteit destinationContextId (changeContext_me destination' me)
--           (lift2 $ findRoleRequests destinationContextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
--           origin' <- lift2 (modifyContext_rolInContext origin rolName (flip difference (toArray rolInstances)))
--           saveEntiteit originContextId (changeContext_me origin' Nothing)
--           (lift2 $ findRoleRequests originContextId (EnumeratedRoleType "model:System$Context$Me")) >>= addCorrelationIdentifiersToTransactie
--       -- Guarantees RULE TRIGGERING because contexts with a vantage point are added to
--       -- the transaction, too.
--       users <- usersWithPerspectiveOnRoleInstance originContextId rolName (head rolInstances)
--       -- SYNCHRONISATION
--       addContextDelta $ ContextDelta
--             { id : originContextId
--             , roleType: rolName
--             , deltaType: MoveRoleInstancesToAnotherContext
--             , roleInstances: SerializableNonEmptyArray rolInstances
--             , users
--             , sequenceNumber: 0
--             , destinationContext: Just destinationContextId
--             , subject
--             }
--       -- QUERY UPDATES
--       (lift2 $ findRoleRequests destinationContextId rolName) >>= addCorrelationIdentifiersToTransactie
--       (lift2 $ findRoleRequests originContextId rolName) >>= addCorrelationIdentifiersToTransactie

-----------------------------------------------------------
-- UPDATE A ROLE (ADD OR REMOVE PROPERTY VALUES)
-----------------------------------------------------------
type PropertyUpdater = Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))

-- | Modify the role instance with the new property values.
-- | When all values are already in the list of values of the property for the role instance, this is a no-op.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
addProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
addProperty rids propertyName values = case ARR.head rids of
  Nothing -> pure unit
  Just roleId -> do
    subject <- authoringRole
    author <- lift2 getUserIdentifier
    for_ rids \rid -> do
      (pe :: PerspectRol) <- lift2 $ getPerspectEntiteit rid
      -- Compute the users for this role (the value has no effect). As a side effect, contexts are added to the transaction.
      users <- aisInPropertyDelta rid propertyName
      deltas <- for values \value -> do
        -- Create a delta for each value.
        delta <- pure $ RolePropertyDelta
          { id : rid
          , property: propertyName
          , deltaType: AddProperty
          , values: [value]
          , subject
          }
        signedDelta <- pure $ SignedDelta
          { author
          , encryptedDelta: sign $ encodeJSON $ delta}
        addDelta (DeltaInTransaction { users, delta: signedDelta})
        pure (Tuple (unwrap value) signedDelta)
      (lift2 $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie
      -- Apply all changes to the role and then save it:
      --  - change the property values in one go
      --  - add all propertyDeltas.
      saveEntiteit roleId (over PerspectRol (\r@{propertyDeltas} -> r {propertyDeltas = setDeltasForProperty propertyName (OBJ.union (fromFoldable deltas)) propertyDeltas}) (addRol_property pe propertyName values))

setDeltasForProperty :: EnumeratedPropertyType -> (Object SignedDelta -> Object SignedDelta) -> (Object (Object SignedDelta)) -> (Object (Object SignedDelta))
setDeltasForProperty propertyName modifier allDeltas = case lookup (unwrap propertyName) allDeltas of
  Nothing -> insert (unwrap propertyName) (modifier empty) allDeltas
  Just oldDeltas -> insert (unwrap propertyName) (modifier oldDeltas) allDeltas

-- | Remove the values from the property's values for the role instance.
-- | When none of the values are in the list of values of the property for the role instance, this is a no-op.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
removeProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
removeProperty rids propertyName values = case ARR.head rids of
  Nothing -> pure unit
  Just roleId -> do
    subject <- authoringRole
    for_ rids \rid -> do
      (pe :: PerspectRol) <- lift2 $ getPerspectEntiteit rid
      users <- aisInPropertyDelta rid propertyName
      -- Create a delta for all values at once.
      delta <- pure $ RolePropertyDelta
        { id : rid
        , property: propertyName
        , deltaType: RemoveProperty
        , values: values
        , subject
        }
      author <- lift2 getUserIdentifier
      signedDelta <- pure $ SignedDelta
        { author
        , encryptedDelta: sign $ encodeJSON $ delta}
      addDelta (DeltaInTransaction { users, delta: signedDelta})
      (lift2 $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie
      -- Apply all changes to the role and then save it:
      --  - change the property values in one go
      --  - remove all propertyDeltas.
      saveEntiteit roleId (over PerspectRol (\r@{propertyDeltas} -> r {propertyDeltas = setDeltasForProperty propertyName (filterKeys (\key -> isJust $ elemIndex (Value key) values)) propertyDeltas}) (removeRol_property pe propertyName values))

-- | Delete all property values from the role for the EnumeratedPropertyType.
-- | If there are no values for the property on the role instance, this is a no-op.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by PropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
deleteProperty :: Array RoleInstance -> EnumeratedPropertyType -> MonadPerspectivesTransaction Unit
deleteProperty rids propertyName = case ARR.head rids of
  Nothing -> pure unit
  Just roleId -> do
    subject <- authoringRole
    for_ rids \rid -> do
      (pe :: PerspectRol) <- lift2 $ getPerspectEntiteit rid
      users <- aisInPropertyDelta rid propertyName
      -- Create a delta for all values.
      delta <- pure $ RolePropertyDelta
        { id : rid
        , property: propertyName
        , deltaType: DeleteProperty
        , values: []
        , subject
        }
      author <- lift2 getUserIdentifier
      signedDelta <- pure $ SignedDelta
        { author
        , encryptedDelta: sign $ encodeJSON $ delta}
      addDelta (DeltaInTransaction { users, delta: signedDelta})
      (lift2 $ findPropertyRequests rid propertyName) >>= addCorrelationIdentifiersToTransactie
      -- Apply all changes to the role and then save it:
      --  - change the property values in one go
      --  - remove all propertyDeltas for this property.
      saveEntiteit roleId (over PerspectRol (\r@{propertyDeltas} -> r {propertyDeltas = setDeltasForProperty propertyName (const empty) propertyDeltas}) (deleteRol_property pe propertyName))

-- | Modify the role instance with the new property values.
-- | When all new values are in fact already in the set of values for the property of the role instance, this is
-- | a no-op.
-- | PERSISTENCE of the role instance.
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER: there can be no change to the current user.
setProperty :: Array RoleInstance -> EnumeratedPropertyType -> (Updater (Array Value))
setProperty rids propertyName values = do
  deleteProperty rids propertyName
  addProperty rids propertyName values

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

authoringRole :: MonadPerspectivesTransaction SubjectOfAction
authoringRole = lift $ UserType <$> gets (_.authoringRole <<< unwrap)
