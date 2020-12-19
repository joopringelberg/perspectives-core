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
  )

where

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Array.NonEmpty (NonEmptyArray, singleton, toArray)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), snd)
import Foreign.Generic (encodeJSON)
import Foreign.Object (isEmpty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.SerialiseAsDeltas (serialisedAsDeltasFor)
import Perspectives.Assignment.Update (addRoleInstancesToContext, getAuthor, getSubject, setProperty)
import Perspectives.Authenticate (sign)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex, rol_padOccurrence)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addCreatedContextToTransaction, deltaIndex, insertDelta)
import Perspectives.DependencyTracking.Dependency (findRoleRequests)
import Perspectives.Error.Boundaries (handlePerspectRolError')
import Perspectives.Identifiers (buitenRol, deconstructLocalName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Parsing.Arc.IndentParser (upperLeft)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectEntiteit, getPerspectRol, saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..), RoleType)
import Perspectives.SaveUserData (setBinding)
import Perspectives.SerializableNonEmptyArray (singleton) as SNEA
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.TypesForDeltas (UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, flip, not, pure, unit, void, ($), (&&), (*>), (+), (<$>), (<<<), (<>), (==), (>>=))

-- | Construct a context from the serialization. If a context with the given id exists, returns a PerspectivesError.
-- | Calls setBinding on each role.
-- | Calls setProperty for each property value.
-- | calls addRoleInstancesToContext on the role instances.
-- | This function is complete w.r.t. the five responsibilities, for the context and its roles.
-- | Retrieves from the repository the model that holds the ContextType, if necessary.
constructContext :: Maybe RoleType -> ContextSerialization -> ExceptT PerspectivesError MonadPerspectivesTransaction ContextInstance
constructContext mbindingRoleType c@(ContextSerialization{id, ctype, rollen, externeProperties}) = do
  contextInstanceId <- ContextInstance <$> (lift $ lift2 $ expandDefaultNamespaces id)
  case (deconstructLocalName $ unwrap contextInstanceId) of
    Nothing -> throwError (NotWellFormedName upperLeft (unwrap contextInstanceId))
    Just localName -> do
      (mc :: Maybe PerspectContext) <- lift $ lift2 $ tryGetPerspectEntiteit contextInstanceId
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
            -- SYNCHRONISATION: through setBinding adds a RoleBindingDelta, also sets isMe.
            -- CURRENTUSER
            for_ rolInstances' addBindingToRoleInstance
            -- Add the completed Role instances to the context.
            -- SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
            lift $ lift $ addRoleInstancesToContext contextInstanceId (EnumeratedRoleType rolTypeId) (flip Tuple Nothing <$> (snd <$> rolInstances'))
            pure $ toArray (snd <$> rolInstances')

          lift $ lift2 $ void $ saveEntiteit contextInstanceId
          -- Add a UniverseContextDelta to the Transaction with the union of the users of the RoleBindingDeltas.
          lift $ insertDelta (DeltaInTransaction{ users, delta: universeContextDelta}) i
          -- TODO. Add the context as a createdContext to the transaction
          lift $ addCreatedContextToTransaction contextInstanceId
          -- Add a UniverseRoleDelta to the Transaction for the external role.
          -- As we've just constructed the context and its external rol, no need to
          -- catch errors rising from not being able to exchange the identifier for the
          -- resource.
          PerspectRol{universeRoleDelta} <- lift $ lift $ lift $ getPerspectRol buitenRol
          lift $ insertDelta (DeltaInTransaction{ users, delta: universeRoleDelta}) (i + 1)
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
      case (deconstructLocalName $unwrap roleType) of
        Nothing -> throwError $ NotWellFormedName upperLeft "not a valid identifier"
        Just localRoleName -> do
          binding' <- lift $ lift $ lift2 (traverse expandDefaultNamespaces binding)
          roleInstanceId <- case mid of
            Nothing -> pure $ RoleInstance (unwrap contextInstanceId <> "$" <> localRoleName <> "_" <> (rol_padOccurrence i))
            Just rid -> pure $ RoleInstance rid
          void $ lift $ lift $ constructEmptyRole contextInstanceId roleType i roleInstanceId
          pure $ Tuple s roleInstanceId

    -- SYNCHRONISATION: through setBinding adds a RoleBindingDelta.
    addBindingToRoleInstance :: (Tuple RolSerialization RoleInstance) -> (WriterT (Array RoleInstance) (ExceptT PerspectivesError MonadPerspectivesTransaction)) Unit
    addBindingToRoleInstance (Tuple (RolSerialization{binding, properties}) roleInstance) = do
      users <- case binding of
        -- the roleInstance is only in cache, save it.
        Nothing -> lift $ lift $ lift2 $ saveEntiteit roleInstance *> pure []
        Just bnd -> do
          expandedBinding <- RoleInstance <$> (lift $ lift $ lift2 $ expandDefaultNamespaces bnd)
          -- setBinding saves, too.
          lift $ lift $ setBinding roleInstance expandedBinding Nothing
      case properties of
        (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
          lift $ lift $ setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
      tell users

-- | Constructs an empty context, caches it.
-- | The context contains a UniverseContextDelta, the external role contains a UniverseRoleDelta.
-- | However, they have not yet been added to the Transaction. This is because we need to know the users
-- | we should sent these deltas to, and these are computed on constructing the roles of the context.
-- | So each caller of constructEmptyContext should add these two deltas to the Transaction.
-- | to the Transaction (and also a UniverseRoleDelta for the external role).
-- | QUERY UPDATES
-- | PERSISTENCE of the external role, but not of the context itself.
-- |
-- | For properties:
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
constructEmptyContext :: ContextInstance -> String -> String -> PropertySerialization -> Maybe RoleType -> ExceptT PerspectivesError MonadPerspectivesTransaction PerspectContext
constructEmptyContext contextInstanceId ctype localName externeProperties authorizedRole = do
  externalRole <- pure $ RoleInstance $ buitenRol $ unwrap contextInstanceId
  pspType <- ContextType <$> (lift $ lift2 $ expandDefaultNamespaces ctype)
  author <- lift $ getAuthor
  subject <- lift getSubject
  contextInstance <- pure
    (PerspectContext defaultContextRecord
      { _id = contextInstanceId
      , displayName  = localName
      , pspType = pspType
      , buitenRol = externalRole
      , universeContextDelta = SignedDelta
        { author
        , encryptedDelta: sign $ encodeJSON $ UniverseContextDelta
            { id: contextInstanceId
            , contextType: pspType
            , deltaType: ConstructEmptyContext
            , subject
          }}
      })
  lift $ lift2 $ void $ cacheEntity contextInstanceId contextInstance
  _ <- lift $ lift2 $ cacheEntity externalRole
    (PerspectRol defaultRolRecord
      { _id = externalRole
      , pspType = EnumeratedRoleType (unwrap pspType <> "$External")
      , context = contextInstanceId
      , binding = Nothing
      , universeRoleDelta =
          SignedDelta
            { author
            , encryptedDelta: sign $ encodeJSON $ UniverseRoleDelta
              { id: contextInstanceId
              , roleInstances: (SNEA.singleton externalRole)
              , roleType: EnumeratedRoleType (unwrap pspType <> "$External")
              , authorizedRole
              , deltaType: ConstructExternalRole
              , subject } }
      })
  -- QUERY UPDATES
  (lift $ lift2 $ findRoleRequests (ContextInstance "model:System$AnyContext") (EnumeratedRoleType $ unwrap pspType <> "$External")) >>= lift <<< addCorrelationIdentifiersToTransactie
  -- TODO. Op dit moment van constructie aangekomen is nog niet vastgelegd wie 'me' is in de context.
  case externeProperties of
    (PropertySerialization props) -> lift do
      forWithIndex_ props \propertyTypeId values ->
        -- PERSISTENCE of the role instance.
        -- CURRENTUSER: there can be no change to the current user.
        setProperty [externalRole] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
      if isEmpty props then lift2 $ void $ saveEntiteit externalRole else pure unit
  pure contextInstance

-- | Construct a Role instance for an existing Context instance.
-- | This function is complete w.r.t. the five responsibilities.
-- | Notice that roleType must be a well-formed identifier!
-- | Retrieves from the repository the model that holds the RoleType, if necessary.
createAndAddRoleInstance :: EnumeratedRoleType -> String -> RolSerialization -> MonadPerspectivesTransaction (Maybe RoleInstance)
createAndAddRoleInstance roleType@(EnumeratedRoleType rtype) id (RolSerialization{id: mRoleId, properties, binding}) = case binding of
  Nothing -> go false
  Just b -> (lift2 $ try $ getPerspectEntiteit (RoleInstance b)) >>=
    handlePerspectRolError' "createAndAddRoleInstance" Nothing
      \(PerspectRol{isMe}) -> go isMe

  where
    go :: Boolean -> MonadPerspectivesTransaction (Maybe RoleInstance)
    go isMe = do
      contextInstanceId <- ContextInstance <$> (lift2 $ expandDefaultNamespaces id)
      rolInstances <- lift2 (contextInstanceId ##= getEnumeratedRoleInstances roleType)
      (EnumeratedRole{kindOfRole}) <- lift2 $ getEnumeratedRole roleType
      (PerspectRol r@{_id:roleInstance}) <- case mRoleId of
        Nothing -> do
          rolInstanceId <- pure $ RoleInstance (unwrap contextInstanceId <> "$" <> (unsafePartial $ fromJust (deconstructLocalName $ unwrap roleType)) <> "_" <> (rol_padOccurrence (getNextRolIndex rolInstances)))
          constructEmptyRole contextInstanceId roleType (getNextRolIndex rolInstances) rolInstanceId
        Just roleId -> constructEmptyRole contextInstanceId roleType (getNextRolIndex rolInstances) (RoleInstance roleId)

      -- Serialise as Deltas if we bind to a user that is not me.
      if (isJust binding && kindOfRole == UserRole && not isMe)
          -- If the binding is a User that is not me,
          -- serialise the context for that User.
          -- We must do that before we generate Deltas that add a role to the context,
          -- otherwise, on reconstructing from the Deltas, the peer'll try to add a rol to a
          -- non-existing context!
          -- We assume here that a User has just one role in the context (otherwise the serialisation is superfluous).
          then contextInstanceId `serialisedAsDeltasFor` roleInstance
          else pure unit

      -- Then add the new Role instance to the context. Takes care of SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
      addRoleInstancesToContext contextInstanceId roleType (singleton (Tuple roleInstance Nothing))
      -- Then add the binding
      case binding of
        Nothing -> pure unit
        Just bnd -> do
          expandedBinding <- RoleInstance <$> (lift2 $ expandDefaultNamespaces bnd)
          void $ setBinding roleInstance expandedBinding Nothing
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
  role <- pure (PerspectRol defaultRolRecord
    { _id = rolInstanceId
    , pspType = roleType
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
    })
  void $ lift2 $ cacheEntity rolInstanceId role
  pure role
