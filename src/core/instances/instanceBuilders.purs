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
  , module Perspectives.Instances.CreateContext
  )

where

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), snd)
import Foreign.Generic (encodeJSON)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (addRoleInstanceToContext, getAuthor, getSubject, setProperty)
import Perspectives.Authenticate (sign)
import Perspectives.ContextAndRole (defaultRolRecord, getNextRolIndex, rol_padOccurrence)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=), (###=))
import Perspectives.Deltas (addCreatedContextToTransaction, addCreatedRoleToTransaction, deltaIndex, insertDelta)
import Perspectives.Error.Boundaries (handlePerspectRolError')
import Perspectives.Identifiers (deconstructLocalName, deconstructModelName, isQualifiedWithDomein, isUrl)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.CreateContext (constructEmptyContext)
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Parsing.Arc.IndentParser (upperLeft)
import Perspectives.Parsing.Arc.PhaseTwo (addNamespace)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectEntiteit, getPerspectRol, saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.Persistent.PublicStore (mapPublicStore)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..), getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.SaveUserData (setFirstBinding)
import Perspectives.SerializableNonEmptyArray (singleton) as SNEA
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Types.ObjectGetters (getPublicStore_, roleAspectsClosure)
import Perspectives.TypesForDeltas (UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, pure, unit, void, ($), (*>), (+), (<$>), (<>), (>>=))

-- | Construct a context from the serialization. If a context with the given id exists, returns a PerspectivesError.
-- | Calls setFirstBinding on each role.
-- | Calls setProperty for each property value.
-- | calls addRoleInstanceToContext on the role instances.
-- | This function is complete w.r.t. the five responsibilities, for the context and its roles.
-- | Retrieves from the repository the model that holds the ContextType, if necessary.
constructContext :: Maybe RoleType -> ContextSerialization -> ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
constructContext mbindingRoleType c@(ContextSerialization{id, ctype, rollen, externeProperties}) = do
  contextInstanceId <- constructContextIdentifier
  case (deconstructLocalName $ unwrap contextInstanceId) of
    Nothing -> throwError (NotWellFormedName upperLeft (unwrap contextInstanceId))
    Just localName -> do
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
          -- As we've just constructed the context and its external rol, no need to
          -- catch errors rising from not being able to exchange the identifier for the
          -- resources.
          PerspectRol{universeRoleDelta} <- lift $ lift $ getPerspectRol buitenRol
          lift $ insertDelta (DeltaInTransaction{ users, delta: universeRoleDelta}) (i)
          -- Add a UniverseContextDelta to the Transaction with the union of the users of the RoleBindingDeltas.
          lift $ insertDelta (DeltaInTransaction{ users, delta: universeContextDelta}) (i + 1)
          -- Add the context as a createdContext to the transaction
          lift $ addCreatedContextToTransaction contextInstanceId
          pure contextInstanceId
  where

    constructContextIdentifier :: ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
    constructContextIdentifier = do 
      expanded <- lift $ lift $ expandDefaultNamespaces id 
      if isQualifiedWithDomein expanded
        then pure $ ContextInstance expanded
        else if isUrl id
          then pure $ ContextInstance id
          -- The case expression below is Partial, but the parser only returns constructors of PublicStore,
          -- hence it is safe to claim unsafePartial.
          else (lift $ lift $ getPublicStore_ (ContextType ctype)) >>= unsafePartial case _ of 
            -- There is no public store that we can use to construct an identifier.
            Nothing -> throwError (NotWellFormedName upperLeft id)
            Just pStore -> case deconstructModelName ctype of
                -- As ctype comes from a ContextSerialization that comes from the client, 
                -- anticipate incorrectly formed names.
                Nothing -> throwError (NotWellFormedName upperLeft ctype)
                Just modelName -> pure $ ContextInstance $ addNamespace (mapPublicStore pStore modelName) id

    -- Constructed with a UniverseRoleDelta but no RoleBindingDelta.
    constructSingleRoleInstance ::
      ContextInstance ->
      EnumeratedRoleType ->
      Int ->
      RolSerialization ->
      (WriterT (Array RoleInstance) (ExceptT PerspectivesError MonadPerspectivesTransaction)) (Tuple RolSerialization RoleInstance)
    constructSingleRoleInstance contextInstanceId roleType i s@(RolSerialization{id:mid, properties, binding}) = do
      case (deconstructLocalName $unwrap roleType) of
        Nothing -> throwError $ NotWellFormedName upperLeft "not a valid identifier"
        Just localRoleName -> do
          binding' <- lift  $ lift $ lift (traverse expandDefaultNamespaces binding)
          roleInstanceId <- case mid of
            Nothing -> pure $ RoleInstance (unwrap contextInstanceId <> "$" <> localRoleName <> "_" <> (rol_padOccurrence i))
            Just rid -> pure $ RoleInstance rid
          void $ lift $ lift $ constructEmptyRole contextInstanceId roleType i roleInstanceId
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
createAndAddRoleInstance roleType@(EnumeratedRoleType rtype) contextId (RolSerialization{id: mRoleId, properties, binding}) = case binding of
  Nothing -> go false
  Just b -> (lift $ try $ getPerspectEntiteit (RoleInstance b)) >>=
    handlePerspectRolError' "createAndAddRoleInstance" Nothing
      \(PerspectRol{isMe}) -> go isMe

  where
    go :: Boolean -> MonadPerspectivesTransaction (Maybe RoleInstance)
    go isMe = do
      contextInstanceId <- ContextInstance <$> (lift $ expandDefaultNamespaces contextId)
      rolInstances <- lift (contextInstanceId ##= getRoleInstances (ENR roleType))
      (EnumeratedRole{kindOfRole}) <- lift $ getEnumeratedRole roleType
      -- SYNCHRONISATION by UniverseRoleDelta
      (PerspectRol r@{_id:roleInstance}) <- case mRoleId of
        Nothing -> do
          rolInstanceId <- pure $ RoleInstance (unwrap contextInstanceId <> "$" <> (unsafePartial $ fromJust (deconstructLocalName $ unwrap roleType)) <> "_" <> (rol_padOccurrence (getNextRolIndex rolInstances)))
          constructEmptyRole contextInstanceId roleType (getNextRolIndex rolInstances) rolInstanceId
        Just roleId -> constructEmptyRole contextInstanceId roleType (getNextRolIndex rolInstances) (RoleInstance roleId)

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

      pure $ Just roleInstance


-- | `localName` should be the local name of the roleType.
-- | The role instance is cached.
constructEmptyRole ::
  ContextInstance ->
  EnumeratedRoleType ->
  Int ->
  RoleInstance ->
  MonadPerspectivesTransaction PerspectRol
constructEmptyRole contextInstance roleType i rolInstanceId = do
  author <- getAuthor
  subject <- getSubject
  allTypes <- lift (roleType ###= roleAspectsClosure)
  role <- pure (PerspectRol defaultRolRecord
    { _id = rolInstanceId
    , pspType = roleType
    , allTypes = allTypes
    , context = contextInstance
    , occurrence = i
    , universeRoleDelta =
        SignedDelta
          { author
          , encryptedDelta: sign $ encodeJSON $ UniverseRoleDelta
            { id: contextInstance
            , roleInstances: (SNEA.singleton rolInstanceId)
            , roleType
            , authorizedRole: Nothing
            , deltaType: ConstructEmptyRole
            , subject } }
    , states = [StateIdentifier $ unwrap roleType]
    })
  void $ lift $ cacheEntity rolInstanceId role
  addCreatedRoleToTransaction rolInstanceId
  pure role
