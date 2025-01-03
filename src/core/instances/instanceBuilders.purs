-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | The functions in this module build contexts and roles from a description.
-- | The resulting contexts and roles are fully integrated in the sense of the five
-- | basic responsibilities of the PDR:
-- | PERSISTENCE
-- | SYNCHRONISATION
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER

module Perspectives.Instances.Builders
  ( constructContext
  , createAndAddRoleInstance
  , createAndAddRoleInstance_
  , lookupOrCreateRoleInstance
  , lookupOrCreateContextInstance
  , module Perspectives.Instances.CreateContext
  )
  where

import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Array (catMaybes, elemIndex, length)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign.Object (empty, insert)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Assignment.Update (addRoleInstanceToContext, setProperty)
import Perspectives.ContextAndRole (changeRol_isMe, getNextRolIndex)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=), (###=), IndexedResource(..))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addCreatedContextToTransaction, deltaIndex, insertDelta)
import Perspectives.DependencyTracking.Dependency (findIndexedContextNamesRequests)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.CreateContext (constructEmptyContext)
import Perspectives.Instances.CreateRole (constructEmptyRole)
import Perspectives.Instances.Me (isMe)
import Perspectives.Names (expandDefaultNamespaces, getMySystem, lookupIndexedContext, lookupIndexedRole)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Persistent (getPerspectRol, saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.PerspectivesState (getIndexedResourceToCreate)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (kindOfRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ResourceType(..), RoleKind(..), RoleType(..), roletype2string)
import Perspectives.ResourceIdentifiers (createResourceIdentifier, createResourceIdentifier', guid, isInPublicScheme)
import Perspectives.SaveUserData (setFirstBinding)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (indexedContextName, indexedRoleName, publicUserRole)
import Prelude (Unit, bind, discard, eq, pure, unit, void, ($), (*>), (+), (<$>), (<<<), (<>), (>>=), (&&), (-))

-- | Construct a context from the serialization. If a context with the given id exists, returns a PerspectivesError.
-- | Calls setFirstBinding on each role.
-- | Calls setProperty for each property value.
-- | calls addRoleInstanceToContext on the role instances.
-- | This function is complete w.r.t. the five responsibilities, for the context and its roles.
-- | Retrieves from the repository the model that holds the ContextType, if necessary.
constructContext :: Maybe RoleType -> ContextSerialization -> ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
constructContext mbindingRoleType c@(ContextSerialization{id, ctype, rollen, externeProperties}) = lookupOrCreateContextInstance  (ContextType ctype) id
  do
    contextInstanceId <- case id of
      Nothing -> ContextInstance <$> (lift $ createResourceIdentifier (CType $ ContextType ctype))
      Just cname -> ContextInstance <$> (lift $ createResourceIdentifier' (CType $ ContextType ctype) cname)
    localName <- lift $ lift $ guid (unwrap contextInstanceId)
    (mc :: Maybe PerspectContext) <- lift $ lift $ tryGetPerspectEntiteit contextInstanceId
    case mc of
      Just _ -> pure contextInstanceId
      Nothing -> do
        -- RULE TRIGGERING
        PerspectContext{universeContextDelta, buitenRol} <- constructEmptyContext contextInstanceId ctype localName externeProperties mbindingRoleType
        -- Get the number of deltas in the transaction. Insert the
        -- UniverseContextDelta and the UniverseRoleDelta after that point.
        i <- lift deltaIndex
        -- Add instances for each role type to the new empty context.
        (Tuple rolInstances users) <- runWriterT $ forWithIndex rollen \rolTypeId rolDescriptions -> do
          -- Construct all instances of this type without binding first, then add them to the context.
          (rolInstances' :: NonEmptyArray (Tuple RolSerialization RoleInstance)) <- forWithIndex
            (unwrap rolDescriptions)
            \ind ser@(RolSerialization{id:mRoleId}) -> (do
              rid <- lift $ lift $ lookupOrCreateRoleInstance 
                (EnumeratedRoleType rolTypeId) 
                mRoleId
                (constructSingleRoleInstance contextInstanceId (EnumeratedRoleType rolTypeId) ind ser)
              -- Add the Role instances to the context.
              -- SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
              lift $ lift $ addRoleInstanceToContext
                contextInstanceId
                (EnumeratedRoleType rolTypeId)
                (Tuple rid Nothing)
              pure rid)
              >>= pure <<< Tuple ser
          -- SYNCHRONISATION: through setFirstBinding adds a RoleBindingDelta, also sets isMe.
          -- CURRENTUSER
          for_ rolInstances' addBindingToRoleInstance
          pure $ toArray (snd <$> rolInstances')

        lift $ lift $ void $ saveEntiteit contextInstanceId
        -- If the context type has a public role, create an instance of its proxy; but only when the contextId is not a public identifier.
        -- Proxies are not created for public contexts, as they are not needed.
        publicRoleInstances <- if isInPublicScheme (unwrap contextInstanceId)
          then pure []
          else do
            publicRoles <- lift $ lift ((ContextType ctype) ###= publicUserRole)
            catMaybes <$> for (EnumeratedRoleType <<< roletype2string <$> publicRoles)
              \t -> lift $ createAndAddRoleInstance 
                t 
                (unwrap contextInstanceId) 
                (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})

        -- Add a UniverseRoleDelta to the Transaction for the external role.
        -- As we've just constructed the context and its external role, no need to
        -- catch errors rising from not being able to exchange the identifier for the
        -- resources.
        PerspectRol{universeRoleDelta, contextDelta} <- lift $ lift $ getPerspectRol buitenRol
        lift $ insertDelta (DeltaInTransaction{ users: users <> publicRoleInstances, delta: universeRoleDelta}) (i)
        -- Add a UniverseContextDelta to the Transaction with the union of the users of the RoleBindingDeltas.
        lift $ insertDelta (DeltaInTransaction{ users: users <> publicRoleInstances, delta: universeContextDelta}) (i + 1)
        -- Add the ContextDelta for the external role to the transaction.
        lift $ insertDelta (DeltaInTransaction{ users: users <> publicRoleInstances, delta: contextDelta}) (i + 2)
        -- Add the context as a createdContext to the transaction
        lift $ addCreatedContextToTransaction contextInstanceId
        -- As the proxy of the public role is just another user, we have to make sure it will receive all deltas necessary
        -- according to its perspectives.
        for_ publicRoleInstances \proxy -> lift (contextInstanceId `serialisedAsDeltasFor` proxy)
        pure contextInstanceId 
  where


    -- Constructed with a UniverseRoleDelta but no RoleBindingDelta.
    constructSingleRoleInstance ::
      ContextInstance ->
      EnumeratedRoleType ->
      Int ->
      RolSerialization ->
      MonadPerspectivesTransaction RoleInstance
    constructSingleRoleInstance contextInstanceId roleType i s@(RolSerialization{id:mid, properties, binding}) = do
      localName <- lift $ guid (unwrap contextInstanceId)
      binding' <-  lift (traverse expandDefaultNamespaces binding)
      roleInstanceId <- case mid of
        Nothing -> RoleInstance <$> (createResourceIdentifier (RType roleType))
        Just rid -> RoleInstance <$> (createResourceIdentifier' (RType roleType) rid)
      void $ constructEmptyRole contextInstanceId roleType i roleInstanceId
      pure $ roleInstanceId

    -- SYNCHRONISATION: through setFirstBinding adds a RoleBindingDelta.
    addBindingToRoleInstance :: (Tuple RolSerialization RoleInstance) -> (WriterT (Array RoleInstance) (ExceptT PerspectivesError MonadPerspectivesTransaction)) Unit
    addBindingToRoleInstance (Tuple (RolSerialization{binding, properties}) roleInstance) = do
      users <- case binding of
        -- the roleInstance is only in cache, save it.
        Nothing -> lift $ lift $ lift $ saveEntiteit roleInstance *> pure []
        Just bnd -> do
          expandedBinding <- RoleInstance <$> (lift $ lift $ lift $ expandDefaultNamespaces bnd)
          -- setFirstBinding saves, too.
          lift $ lift $ withRoleInsertionPoint (setFirstBinding roleInstance expandedBinding Nothing)
      case properties of
        (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
          lift $ lift $ setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) Nothing (Value <$> values)
      tell users

-- | Construct a Role instance for an existing Context instance.
-- | This function is complete w.r.t. the five responsibilities.
-- | Notice that roleType must be a well-formed (and expanded) identifier!
-- | The contextId may be prefixed with a default namespace: it will be expanded.
-- | Retrieves from the repository the model that holds the RoleType, if necessary.
createAndAddRoleInstance :: EnumeratedRoleType -> String -> RolSerialization -> MonadPerspectivesTransaction (Maybe RoleInstance)
createAndAddRoleInstance roleType@(EnumeratedRoleType rtype) contextId r@(RolSerialization{binding}) =
  case binding of
      Nothing -> Just <$> (createAndAddRoleInstance_ roleType contextId r false)
      Just b -> do 
        me <- lift (isMe $ RoleInstance b)
        isUserRole <- lift (getEnumeratedRole roleType >>= \rl -> pure $ isJust $ elemIndex (kindOfRole rl) [UserRole, Public, PublicProxy])
        Just <$> (createAndAddRoleInstance_ roleType contextId r (me && isUserRole))

createAndAddRoleInstance_ :: EnumeratedRoleType -> String -> RolSerialization -> Boolean -> MonadPerspectivesTransaction RoleInstance
createAndAddRoleInstance_ roleType@(EnumeratedRoleType rtype) contextId (RolSerialization{id: mRoleId, properties, binding}) isMe = lookupOrCreateRoleInstance roleType
  mRoleId
  do
    contextInstanceId <- ContextInstance <$> (lift $ expandDefaultNamespaces contextId) 
    rolInstances <- lift (contextInstanceId ##= getRoleInstances (ENR roleType))
    -- SYNCHRONISATION by UniverseRoleDelta
    r@(PerspectRol {id:roleInstance}) <- case mRoleId of
      Nothing -> do
        rolInstanceId <- createResourceIdentifier (RType roleType)
        constructEmptyRole contextInstanceId roleType (getNextRolIndex rolInstances) (RoleInstance rolInstanceId)
      Just roleId -> constructEmptyRole contextInstanceId roleType (getNextRolIndex rolInstances) (RoleInstance roleId)
    
    if isMe
      then void $ lift $ cacheEntity roleInstance (changeRol_isMe r true)
      else pure unit
    
    -- Then add the new Role instance to the context. Takes care of SYNCHRONISATION by constructing and
    -- adding a ContextDelta. Also adds the UniverseRoleDelta constructed by constructEmptyRole.
    -- We postpone adding the UniverseRoleDelta because we cannot compute the users to distribute it to yet.
    -- This is done in addRoleInstanceToContext.
    addRoleInstanceToContext contextInstanceId roleType (Tuple roleInstance Nothing)
    -- Then add the binding
    case binding of
      Nothing -> pure unit
      Just bnd -> do
        expandedBinding <- RoleInstance <$> (lift $ expandDefaultNamespaces bnd)
        void $ withRoleInsertionPoint (setFirstBinding roleInstance expandedBinding Nothing)
    -- Then add the properties
    case properties of
      (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
        setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) Nothing (Value <$> values)
    pure roleInstance

scheduleIndexedResourceCreation :: IndexedResource -> MonadPerspectivesTransaction Unit
scheduleIndexedResourceCreation ir = do
  -- Fetch the AVar that holds the IndexedResource to create
  av <- lift $ getIndexedResourceToCreate
  -- Put the resource into it. Notice that this blocks; however, there is a fiber continuously waiting 
  -- and it will take the IndexedResourece out of the AVar as soon as possible.
  liftAff $ put ir av

-- | If made before, returns the indexed role.
-- | Iff the role type is indexed, create an entry for IndexedRoles in System and fill it with this role.
-- | Also sets the Name property.
-- | The role and name will be used to re-create IndexedRoles at system startup.
-- | DOES NOT check whether an instance of the indexed role has been made before.
lookupOrCreateRoleInstance :: EnumeratedRoleType -> Maybe String -> MonadPerspectivesTransaction RoleInstance -> MonadPerspectivesTransaction RoleInstance
lookupOrCreateRoleInstance rtype id roleConstructor = do
  mindexedName <- lift $ indexedRoleName rtype
  case mindexedName of
    Nothing -> roleConstructor
    Just (RoleInstance indexedName) -> do
      mrole <- lift $ lookupIndexedRole indexedName
      case mrole of 
        -- Now construct the instance and add an instance of IndexedRoles to register it is, in fact, an indexed instance!
        Nothing -> executeConstructor indexedName
        Just ind@(RoleInstance r) -> if maybe true (eq r) id
          -- We have an indexed instance that is equal to the instance that we want to make.
          -- Just return it.
          then pure ind
          -- Yes, this is an indexed role type and we do already have an instance of IndexedRoles to register
          -- that that instance is our indexed instance. Nevertheless, we want to make another instance - but 
          -- it will not be our indexed instance!
          else roleConstructor
  where
    executeConstructor :: String -> MonadPerspectivesTransaction RoleInstance
    executeConstructor indexedName = do
      rid <- roleConstructor
      liftAff $ log $ "Adding indexed role " <> indexedName
      lift $ modify \ps -> ps {indexedRoles = insert indexedName rid ps.indexedRoles}
      scheduleIndexedResourceCreation (IndexedRole rid indexedName)
      pure rid

-- | If made before, returns the indexed context.
-- | Otherwise, iff the role type is indexed, create an entry for IndexedRoles in System and fill it with this role.
-- | Also sets the Name property.
-- | The role and name will be used to re-create IndexedContexts at system startup.
lookupOrCreateContextInstance :: ContextType -> 
  Maybe String ->
  ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance -> 
  ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
lookupOrCreateContextInstance ctype id contextConstructor = do
  mindexedName <- lift $ lift $ indexedContextName ctype 
  case mindexedName of 
    Nothing -> contextConstructor
    Just (ContextInstance indexedName) -> do
      minst <- lift $ lift $ lookupIndexedContext indexedName
      case minst of
        Nothing -> executeConstructor indexedName
        -- Even though we may have an indexed instance, it may be ANOTHER instance than the one we want to create.
        -- Only one instance of this role type in this context may be THE indexed instance in this installation,
        -- but we may have other instances (e.g. received from other users) that we do store but not index!
        -- E.g. think of PerspectivesSystem. We have our own indexed instance, but receive instances from peers, too.
        Just ind@(ContextInstance i) -> if maybe true (eq i) id
          -- We have an indexed instance that is equal to the instance that we want to make.
          -- Just return it.
          then pure ind 
          -- Yes, this is an indexed context type and we do already have an instance of IndexedContexts to register
          -- that that instance is our indexed instance. Nevertheless, we want to make another instance - but 
          -- it will not be our indexed instance!
          else contextConstructor
  where
    executeConstructor :: String -> ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
    executeConstructor indexedName = do
      cid <- contextConstructor
      liftAff $ log $ "Adding indexed context " <> indexedName
      lift $ lift $ modify \ps -> ps {indexedContexts = insert indexedName cid ps.indexedContexts}
      lift $ scheduleIndexedResourceCreation (IndexedContext cid indexedName)
      -- QUERY UPDATES
      mysystem <- lift $ lift $ getMySystem
      lift ((lift $ findIndexedContextNamesRequests (ContextInstance mysystem)) >>= addCorrelationIdentifiersToTransactie)
      pure cid

-- | Computes the value in MonadPerspectivesTransaction with an insertion point such that all deltas
-- | added during the computation will end up __before__ the last two deltas added before the computation.
-- | This accommodates the case that we add a role to a context that turns out to be a peer.
-- | The serialisation of the context necessary for the peer will be in the transaction (right) before the 
-- | serialisation of the peer itself. Without this, the peer will receive a transaction that starts with 
-- | adding himself to a context he does not yet have constructed locally.
withRoleInsertionPoint :: forall a. MonadPerspectivesTransaction a -> MonadPerspectivesTransaction a
withRoleInsertionPoint x = do 
  modify \(Transaction tr@{deltas}) -> Transaction $ tr { insertionPoint = Just $ (length deltas) - 2}
  result <- x
  modify \(Transaction tr) -> Transaction $ tr { insertionPoint = Nothing}
  pure result