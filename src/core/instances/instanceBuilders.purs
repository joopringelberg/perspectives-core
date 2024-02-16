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
  , module Perspectives.Instances.CreateContext
  )
  where

import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (liftAff)
import Foreign.Object (empty)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Assignment.Update (addRoleInstanceToContext, setProperty)
import Perspectives.ContextAndRole (changeRol_isMe, getNextRolIndex)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=), (###=), IndexedResource(..))
import Perspectives.Deltas (addCreatedContextToTransaction, deltaIndex, insertDelta)
import Perspectives.Error.Boundaries (handlePerspectRolError')
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.CreateContext (constructEmptyContext)
import Perspectives.Instances.CreateRole (constructEmptyRole)
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Persistent (getPerspectEntiteit, getPerspectRol, saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.PerspectivesState (getIndexedResourceToCreate)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ResourceType(..), RoleType(..), roletype2string)
import Perspectives.ResourceIdentifiers (createResourceIdentifier, createResourceIdentifier', guid)
import Perspectives.SaveUserData (setFirstBinding)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Types.ObjectGetters (indexedContextName, indexedRoleName, publicUserRole)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (*>), (+), (<$>), (<<<), (>>=))

-- | Construct a context from the serialization. If a context with the given id exists, returns a PerspectivesError.
-- | Calls setFirstBinding on each role.
-- | Calls setProperty for each property value.
-- | calls addRoleInstanceToContext on the role instances.
-- | This function is complete w.r.t. the five responsibilities, for the context and its roles.
-- | Retrieves from the repository the model that holds the ContextType, if necessary.
constructContext :: Maybe RoleType -> ContextSerialization -> ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
constructContext mbindingRoleType c@(ContextSerialization{id, ctype, rollen, externeProperties}) = do
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
          (constructSingleRoleInstance contextInstanceId (EnumeratedRoleType rolTypeId))
        -- SYNCHRONISATION: through setFirstBinding adds a RoleBindingDelta, also sets isMe.
        -- CURRENTUSER
        for_ rolInstances' addBindingToRoleInstance
        -- Add the completed Role instances to the context.
        -- SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
        for_ rolInstances'
          \rolInstance -> lift $ lift $ addRoleInstanceToContext
            contextInstanceId
            (EnumeratedRoleType rolTypeId)
            (Tuple (snd rolInstance) Nothing)
        pure $ toArray (snd <$> rolInstances')

      lift $ lift $ void $ saveEntiteit contextInstanceId
      -- Add a UniverseRoleDelta to the Transaction for the external role.
      -- As we've just constructed the context and its external role, no need to
      -- catch errors rising from not being able to exchange the identifier for the
      -- resources.
      PerspectRol{universeRoleDelta, contextDelta} <- lift $ lift $ getPerspectRol buitenRol
      lift $ insertDelta (DeltaInTransaction{ users, delta: universeRoleDelta}) (i)
      -- Add a UniverseContextDelta to the Transaction with the union of the users of the RoleBindingDeltas.
      lift $ insertDelta (DeltaInTransaction{ users, delta: universeContextDelta}) (i + 1)
      -- Add the ContextDelta for the external role to the transaction.
      lift $ insertDelta (DeltaInTransaction{ users, delta: contextDelta}) (i + 2)
      -- Add the context as a createdContext to the transaction
      lift $ addCreatedContextToTransaction contextInstanceId
      -- If the context type has a public role, create an instance of its proxy.
      publicRoles <- lift $ lift ((ContextType ctype) ###= publicUserRole)
      for_ (EnumeratedRoleType <<< roletype2string <$> publicRoles) 
        \t -> (do 
          mproxy <- lift $ createAndAddRoleInstance 
            t 
            (unwrap contextInstanceId) 
            (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})
          case mproxy of 
            -- As the proxy of the public role is just another user, we have to make sure it will receive all deltas necessary
            -- according to its perspectives.
            Just proxy -> lift (contextInstanceId `serialisedAsDeltasFor` proxy)
            -- This will never happen.
            Nothing -> pure unit
            )
      lift $ createIndexedContext (ContextType ctype) contextInstanceId
      pure contextInstanceId 
  where


    -- Constructed with a UniverseRoleDelta but no RoleBindingDelta.
    constructSingleRoleInstance ::
      ContextInstance ->
      EnumeratedRoleType ->
      Int ->
      RolSerialization ->
      (WriterT (Array RoleInstance) (ExceptT PerspectivesError MonadPerspectivesTransaction)) (Tuple RolSerialization RoleInstance)
    constructSingleRoleInstance contextInstanceId roleType i s@(RolSerialization{id:mid, properties, binding}) = do
      localName <- lift $ lift $ lift $ guid (unwrap contextInstanceId)
      binding' <- lift  $ lift $ lift (traverse expandDefaultNamespaces binding)
      roleInstanceId <- case mid of
        Nothing -> RoleInstance <$> (lift $ lift $ createResourceIdentifier (RType roleType))
        Just rid -> RoleInstance <$> (lift $ lift $ createResourceIdentifier' (RType roleType) rid)
      void $ lift $ lift $ constructEmptyRole contextInstanceId roleType i roleInstanceId
      lift $ lift $ createIndexedRole roleType roleInstanceId
      pure $ Tuple s roleInstanceId

    -- SYNCHRONISATION: through setFirstBinding adds a RoleBindingDelta.
    addBindingToRoleInstance :: (Tuple RolSerialization RoleInstance) -> (WriterT (Array RoleInstance) (ExceptT PerspectivesError MonadPerspectivesTransaction)) Unit
    addBindingToRoleInstance (Tuple (RolSerialization{binding, properties}) roleInstance) = do
      users <- case binding of
        -- the roleInstance is only in cache, save it.
        Nothing -> lift $ lift $ lift $ saveEntiteit roleInstance *> pure []
        Just bnd -> do
          expandedBinding <- RoleInstance <$> (lift $ lift $ lift $ expandDefaultNamespaces bnd)
          -- setFirstBinding saves, too.
          lift $ lift $ setFirstBinding roleInstance expandedBinding Nothing
      case properties of
        (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
          lift $ lift $ setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
      tell users

-- | Construct a Role instance for an existing Context instance.
-- | This function is complete w.r.t. the five responsibilities.
-- | Notice that roleType must be a well-formed (and expanded) identifier!
-- | The contextId may be prefixed with a default namespace: it will be expanded.
-- | Retrieves from the repository the model that holds the RoleType, if necessary.
createAndAddRoleInstance :: EnumeratedRoleType -> String -> RolSerialization -> MonadPerspectivesTransaction (Maybe RoleInstance)
createAndAddRoleInstance roleType@(EnumeratedRoleType rtype) contextId r@(RolSerialization{binding}) = case binding of
  Nothing -> createAndAddRoleInstance_ roleType contextId r false
  Just b -> (lift $ try $ getPerspectEntiteit (RoleInstance b)) >>=
    handlePerspectRolError' "createAndAddRoleInstance" Nothing
      \(PerspectRol{isMe}) -> createAndAddRoleInstance_ roleType contextId r isMe

createAndAddRoleInstance_ :: EnumeratedRoleType -> String -> RolSerialization -> Boolean -> MonadPerspectivesTransaction (Maybe RoleInstance)
createAndAddRoleInstance_ roleType@(EnumeratedRoleType rtype) contextId (RolSerialization{id: mRoleId, properties, binding}) isMe = do
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
        void $ setFirstBinding roleInstance expandedBinding Nothing
    -- Then add the properties
    case properties of
      (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
        setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)

    createIndexedRole roleType roleInstance
    pure $ Just roleInstance

-- | Iff the role type is indexed, create an entry for IndexedRoles in System and fill it with this role.
-- | Also sets the Name property.
-- | The role and name will be used to re-create IndexedRoles at system startup.
-- | DOES NOT check whether an instance of the indexed role has been made before.
createIndexedRole :: EnumeratedRoleType -> RoleInstance -> MonadPerspectivesTransaction Unit
createIndexedRole rtype rid = do
  mindexedName <- lift $ indexedRoleName rtype
  case mindexedName of 
    Nothing -> pure unit
    -- Create an instance of IndexedRole in sys:MySystem
    -- Fill it with rid
    -- Set its property Name to indexedName.
    -- Run the transaction in a separate thread.
    Just (RoleInstance iName) -> scheduleIndexedResourceCreation (IndexedRole rid iName)

-- | Iff the role type is indexed, create an entry for IndexedRoles in System and fill it with this role.
-- | Also sets the Name property.
-- | The role and name will be used to re-create IndexedContexts at system startup.
-- | DOES NOT check whether an instance of the indexed context has been made before.
createIndexedContext :: ContextType -> ContextInstance -> MonadPerspectivesTransaction Unit
createIndexedContext ctype cid = do
  mindexedName <- lift $ indexedContextName ctype
  case mindexedName of 
    Nothing -> pure unit
    -- Create an instance of IndexedContext in sys:MySystem
    -- Fill it with rid
    -- Set its property Name to indexedName.
    -- Run the transaction in a separate thread.
    Just (ContextInstance iName) -> scheduleIndexedResourceCreation (IndexedContext cid iName)

scheduleIndexedResourceCreation :: IndexedResource -> MonadPerspectivesTransaction Unit
scheduleIndexedResourceCreation ir = do
  -- Fetch the AVar that holds the IndexedResource to create
  av <- lift $ getIndexedResourceToCreate
  -- Put the resource into it. Notice that this blocks; however, there is a fiber continuously waiting 
  -- and it will take the IndexedResourece out of the AVar as soon as possible.
  liftAff $ put ir av
