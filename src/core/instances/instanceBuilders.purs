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
  )

where

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (addRoleInstancesToContext, setBinding, setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex, rol_padOccurrence)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=))
import Perspectives.Deltas (addUniverseContextDelta)
import Perspectives.Identifiers (buitenRol, deconstructLocalName, expandDefaultNamespaces)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.Parsing.Arc.IndentParser (upperLeft)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheInitially)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.TypesForDeltas (DeltaType(..), UniverseContextDelta(..))
import Prelude (bind, discard, pure, show, unit, void, ($), (*>), (<$>), (<>))

-- | Construct a context from the serialization. If a context with the given id exists, returns a PerspectivesError.
-- | Calls setBinding on each role.
-- | Calls setProperty for each property value.
-- | calls addRoleInstancesToContext on the role instances.
-- | This function is complete w.r.t. the five responsibilities, for the context and its roles.
constructContext :: ContextSerialization -> MonadPerspectivesTransaction (Either (Array PerspectivesError) ContextInstance)
constructContext c@(ContextSerialization{id, ctype, rollen, externeProperties}) = do
  contextInstanceId <- pure $ ContextInstance $ expandDefaultNamespaces id
  case (deconstructLocalName $ unwrap contextInstanceId) of
    Nothing -> pure $ Left [(NotWellFormedName upperLeft (unwrap contextInstanceId))]
    Just localName -> do
      (mc :: Maybe PerspectContext) <- lift2 $ tryGetPerspectEntiteit contextInstanceId
      case mc of
        Just _ -> pure $ Right contextInstanceId
        Nothing -> do
          contextInstance <- Right <$> constructEmptyContext contextInstanceId ctype localName
          -- We now have an empty context. Add each role to it.
          r <- try $ runWriterT $ forWithIndex_ rollen \rolTypeId rolDescriptions -> do
            -- Construct the role instances first, then add to the context.
            rolInstances <- forWithIndex rolDescriptions (constructSingleRoleInstance contextInstanceId (EnumeratedRoleType rolTypeId))
            -- Add the completed Role instance to the context.
            lift $ addRoleInstancesToContext contextInstanceId (EnumeratedRoleType rolTypeId) rolInstances
          case r of
            Left e -> pure $ Left [Custom (show e)]
            Right (Tuple _ users) -> do
              -- Add a UniverseContextDelta with the union of the users of the RoleBindingDeltas.
              -- TODO: de volgorde van deltas is misschien niet goed.
              addUniverseContextDelta $ UniverseContextDelta
                  { id: contextInstanceId
                  , contextType: ContextType $ expandDefaultNamespaces ctype
                  , deltaType: Add
                  , users}
              pure $ Right contextInstanceId
  where
    constructSingleRoleInstance :: ContextInstance -> EnumeratedRoleType -> Int -> RolSerialization -> WriterT (Array RoleInstance)  MonadPerspectivesTransaction RoleInstance
    constructSingleRoleInstance contextInstanceId roleType i (RolSerialization{properties, binding}) = do
      -- create an empty role
      case (deconstructLocalName $unwrap roleType) of
        Nothing -> throwError $ error "not a valid identifier"
        Just localRoleName -> do
          roleInstance <- lift $ constructEmptyRole roleType contextInstanceId i localRoleName
          -- then add the binding
          users <- case binding of
            -- the roleInstance is only in cache, save it.
            Nothing -> lift $ lift2 $ saveEntiteit roleInstance *> pure []
            Just bnd -> do
              expandedBinding <- pure $ RoleInstance $ expandDefaultNamespaces bnd
              -- setBinding saves, too.
              lift $ setBinding roleInstance expandedBinding
          tell users
          -- then add the properties
          case properties of
            (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
              lift $ setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
          pure roleInstance

-- | Constructs an empty role, caches it.
constructEmptyContext :: ContextInstance -> String -> String -> MonadPerspectivesTransaction ContextInstance
constructEmptyContext contextInstanceId ctype localName = do
  externalRole <- pure $ RoleInstance $ buitenRol $ unwrap contextInstanceId
  pspType <- pure $ ContextType $ expandDefaultNamespaces ctype
  _ <- lift2 $ cacheInitially contextInstanceId
    (PerspectContext defaultContextRecord
      { _id = contextInstanceId
      , displayName  = localName
      , pspType = pspType
      , buitenRol = externalRole
    })
  _ <- lift2 $ cacheInitially externalRole
    (PerspectRol defaultRolRecord
      { _id = externalRole
      , pspType = EnumeratedRoleType $ expandDefaultNamespaces ctype <> "$External"
      , context = contextInstanceId
      , binding = Nothing
      })
  pure contextInstanceId

-- | Constructs an empty role and caches it. `localName` should be the local name of the roleType.
constructEmptyRole :: EnumeratedRoleType -> ContextInstance -> Int -> String -> MonadPerspectivesTransaction RoleInstance
constructEmptyRole roleType contextInstance i localRoleName = do
  rolInstanceId <- pure $ RoleInstance (unwrap contextInstance <> "$" <> localRoleName <> "_" <> (rol_padOccurrence i))
  role <- pure (PerspectRol defaultRolRecord
        { _id = rolInstanceId
        , pspType = roleType
        , context = contextInstance
        , occurrence = i
        })
  void $ lift2 $ cacheInitially rolInstanceId role
  pure rolInstanceId

-- | Construct a Role instance for an existing Context instance.
-- | This function is complete w.r.t. the five responsibilities.
-- | Notice that roleType must be a well-formed identifier!
createAndAddRoleInstance :: EnumeratedRoleType -> String -> RolSerialization -> MonadPerspectivesTransaction RoleInstance
createAndAddRoleInstance roleType id (RolSerialization{properties, binding}) = do
  contextInstanceId <- pure $ ContextInstance $ expandDefaultNamespaces id
  rolInstances <- lift2 (contextInstanceId ##= getRole roleType)
  -- create an empty role
  roleInstance <- constructEmptyRole roleType contextInstanceId (getNextRolIndex rolInstances) (unsafePartial $ fromJust (deconstructLocalName $ unwrap roleType))
  -- then add the binding
  case binding of
    Nothing -> pure unit
    Just bnd -> do
      expandedBinding <- pure $ RoleInstance $ expandDefaultNamespaces bnd
      void $ setBinding roleInstance expandedBinding
  -- then add the properties
  case properties of
    (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
      setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
  -- Finally, add the completed Role instance to the context.
  addRoleInstancesToContext contextInstanceId roleType [roleInstance]
  pure roleInstance
