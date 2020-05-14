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
  , constructEmptyContext
  , constructEmptyRole
  , constructEmptyRole_
  )

where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Array.NonEmpty (NonEmptyArray, singleton)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (isEmpty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (addRoleInstancesToContext, handleNewPeer_, setBinding, setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex, rol_padOccurrence)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addUniverseContextDelta_, increaseDeltaIndex)
import Perspectives.DependencyTracking.Dependency (findRoleRequests)
import Perspectives.Identifiers (buitenRol, deconstructLocalName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances, isMe)
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Parsing.Arc.IndentParser (upperLeft)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.TypesForDeltas (UniverseContextDelta(..), UniverseContextDeltaType(..))
import Prelude (bind, discard, pure, unit, void, when, ($), (*>), (<$>), (<>), (>>=), (<<<))

-- | Construct a context from the serialization. If a context with the given id exists, returns a PerspectivesError.
-- | Calls setBinding on each role.
-- | Calls setProperty for each property value.
-- | calls addRoleInstancesToContext on the role instances.
-- | This function is complete w.r.t. the five responsibilities, for the context and its roles.
constructContext :: ContextSerialization -> ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
constructContext c@(ContextSerialization{id, ctype, rollen, externeProperties}) = do
  contextInstanceId <- ContextInstance <$> (lift $ lift2 $ expandDefaultNamespaces id)
  case (deconstructLocalName $ unwrap contextInstanceId) of
    Nothing -> throwError (NotWellFormedName upperLeft (unwrap contextInstanceId))
    Just localName -> do
      (mc :: Maybe PerspectContext) <- lift $ lift2 $ tryGetPerspectEntiteit contextInstanceId
      case mc of
        Just _ -> pure contextInstanceId
        Nothing -> do
          void $ constructEmptyContext contextInstanceId ctype localName externeProperties
          -- Bump the index in the transaction, reserve the current index for the
          -- UniverseContextDelta.
          i <- lift increaseDeltaIndex
          -- Add each role to the new empty context.
          (Tuple _ users) <- runWriterT $ forWithIndex_ rollen \rolTypeId rolDescriptions -> do
            -- Construct all instances of this type first, then add them to the context.
            (rolInstances :: NonEmptyArray RoleInstance) <- forWithIndex (unwrap rolDescriptions) (constructSingleRoleInstance contextInstanceId (EnumeratedRoleType rolTypeId))
            -- Add the completed Role instance to the context.
            lift $ lift $ addRoleInstancesToContext contextInstanceId (EnumeratedRoleType rolTypeId) rolInstances
          -- Add a UniverseContextDelta with the union of the users of the RoleBindingDeltas.
          contextType <- ContextType <$> (lift $ lift2 $ expandDefaultNamespaces ctype)
          lift $ addUniverseContextDelta_ $ UniverseContextDelta
              { id: contextInstanceId
              , contextType
              , deltaType: ConstructEmptyContext
              , users
              , sequenceNumber: i
            }
          pure contextInstanceId
  where
    constructSingleRoleInstance :: ContextInstance -> EnumeratedRoleType -> Int -> RolSerialization -> (WriterT (Array RoleInstance)  (ExceptT PerspectivesError MonadPerspectivesTransaction)) RoleInstance
    constructSingleRoleInstance contextInstanceId roleType i (RolSerialization{id:mid, properties, binding}) = do
      -- create an empty role
      case (deconstructLocalName $unwrap roleType) of
        Nothing -> throwError $ NotWellFormedName upperLeft "not a valid identifier"
        Just localRoleName -> do
          roleInstance <- case mid of
            Nothing -> lift $ lift $ constructEmptyRole roleType contextInstanceId i localRoleName
            Just ident -> lift $ lift $ constructEmptyRole_ roleType contextInstanceId i (RoleInstance ident)
          -- then add the binding
          users <- case binding of
            -- the roleInstance is only in cache, save it.
            Nothing -> lift $ lift $ lift2 $ saveEntiteit roleInstance *> pure []
            Just bnd -> do
              expandedBinding <- RoleInstance <$> (lift $ lift $ lift2 $ expandDefaultNamespaces bnd)
              -- setBinding saves, too.
              lift $ lift $ setBinding roleInstance expandedBinding
          tell users
          -- then add the properties
          case properties of
            (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
              lift $ lift $ setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
          pure roleInstance

-- | Constructs an empty context, caches it.
-- | Whenever constructEmptyContext is applied, a UniverseContextDelta should be added
-- | to the Transaction. However, in order to be able to add all users with a perspective,
-- | it is better to construct that Delta in the calling function.
constructEmptyContext :: ContextInstance -> String -> String -> PropertySerialization -> ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
constructEmptyContext contextInstanceId ctype localName externeProperties = do
  externalRole <- pure $ RoleInstance $ buitenRol $ unwrap contextInstanceId
  pspType <- ContextType <$> (lift $ lift2 $ expandDefaultNamespaces ctype)
  _ <- lift $ lift2 $ cacheEntity contextInstanceId
    (PerspectContext defaultContextRecord
      { _id = contextInstanceId
      , displayName  = localName
      , pspType = pspType
      , buitenRol = externalRole
    })
  _ <- lift $ lift2 $ cacheEntity externalRole
    (PerspectRol defaultRolRecord
      { _id = externalRole
      , pspType = EnumeratedRoleType (unwrap pspType <> "$External")
      , context = contextInstanceId
      , binding = Nothing
      })
  (lift $ lift2 $ findRoleRequests (ContextInstance "AnyContext") (EnumeratedRoleType $ unwrap pspType <> "$External")) >>= lift <<< addCorrelationIdentifiersToTransactie
  case externeProperties of
    (PropertySerialization props) -> lift do
      forWithIndex_ props \propertyTypeId values ->
        setProperty [externalRole] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
      when (isEmpty props) (lift2 $ void $ saveEntiteit externalRole)
  pure contextInstanceId

-- | Constructs an empty role and caches it. `localName` should be the local name of the roleType.
constructEmptyRole :: EnumeratedRoleType -> ContextInstance -> Int -> String -> MonadPerspectivesTransaction RoleInstance
constructEmptyRole roleType contextInstance i localRoleName = do
  rolInstanceId <- pure $ RoleInstance (unwrap contextInstance <> "$" <> localRoleName <> "_" <> (rol_padOccurrence i))
  constructEmptyRole_ roleType contextInstance i rolInstanceId

constructEmptyRole_ :: EnumeratedRoleType -> ContextInstance -> Int -> RoleInstance -> MonadPerspectivesTransaction RoleInstance
constructEmptyRole_ roleType contextInstance i rolInstanceId = do
  role <- pure (PerspectRol defaultRolRecord
        { _id = rolInstanceId
        , pspType = roleType
        , context = contextInstance
        , occurrence = i
        })
  void $ lift2 $ cacheEntity rolInstanceId role
  pure rolInstanceId


-- | Construct a Role instance for an existing Context instance.
-- | This function is complete w.r.t. the five responsibilities.
-- | Notice that roleType must be a well-formed identifier!
createAndAddRoleInstance :: EnumeratedRoleType -> String -> RolSerialization -> MonadPerspectivesTransaction RoleInstance
createAndAddRoleInstance roleType id (RolSerialization{id: mRoleId, properties, binding}) = do
  contextInstanceId <- ContextInstance <$> (lift2 $ expandDefaultNamespaces id)
  rolInstances <- lift2 (contextInstanceId ##= getEnumeratedRoleInstances roleType)
  -- create an empty role
  roleInstance <- case mRoleId of
    Nothing -> constructEmptyRole roleType contextInstanceId (getNextRolIndex rolInstances) (unsafePartial $ fromJust (deconstructLocalName $ unwrap roleType))
    Just roleId -> constructEmptyRole_ roleType contextInstanceId (getNextRolIndex rolInstances) (RoleInstance roleId)
  -- Serialise as Deltas if we bind to a user that is not me.
  case binding of
    Nothing -> pure unit
    Just b -> do
      me <- lift2 $ isMe (RoleInstance b)
      -- If we have a user role and we bind to a user that is not me, serialise the context.
      -- We assume here that a user has just one role in the context (otherwise the serialisation is superfluous).
      handleNewPeer_ me roleInstance
  -- Then add the new Role instance to the context.
  addRoleInstancesToContext contextInstanceId roleType (singleton roleInstance)
  -- then add the binding
  case binding of
    Nothing -> pure unit
    Just bnd -> do
      expandedBinding <- RoleInstance <$> (lift2 $ expandDefaultNamespaces bnd)
      void $ setBinding roleInstance expandedBinding
  -- Finally add the properties
  case properties of
    (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
      setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
  pure roleInstance
