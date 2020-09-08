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

import Control.Monad.Error.Class (throwError)
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
import Perspectives.Assignment.Update (addRoleInstancesToContext, getAuthor, getSubject, setBinding_, setProperty)
import Perspectives.Authenticate (sign)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex, rol_padOccurrence)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, deltaIndex, insertDelta)
import Perspectives.DependencyTracking.Dependency (findRoleRequests)
import Perspectives.Identifiers (buitenRol, deconstructLocalName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..), Binding)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Parsing.Arc.IndentParser (upperLeft)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectEntiteit, getPerspectRol, saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Perspectives.SerializableNonEmptyArray (singleton) as SNEA
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.TypesForDeltas (UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, not, pure, unit, void, when, ($), (&&), (*>), (<$>), (<<<), (<>), (==), (>>=), (+))

-- | Construct a context from the serialization. If a context with the given id exists, returns a PerspectivesError.
-- | Calls setBinding on each role.
-- | Calls setProperty for each property value.
-- | calls addRoleInstancesToContext on the role instances.
-- | This function is complete w.r.t. the five responsibilities, for the context and its roles.
-- | Retrieves from the repository the model that holds the ContextType, if necessary.
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
          -- RULE TRIGGERING
          PerspectContext{universeContextDelta, buitenRol} <- constructEmptyContext contextInstanceId ctype localName externeProperties
          -- Get the number of deltas in the transaction. Insert the
          -- UniverseContextDelta and the UniverseRoleDelta after that point.
          i <- lift deltaIndex
          -- Add instances for each role type to the new empty context.
          (Tuple rolInstances users) <- runWriterT $ forWithIndex rollen \rolTypeId rolDescriptions -> do
            -- Construct all instances of this type without binding first, then add them to the context.
            (rolInstances' :: NonEmptyArray (Tuple RolSerialization RoleInstance)) <- forWithIndex
              (unwrap rolDescriptions)
              (constructSingleRoleInstance contextInstanceId (EnumeratedRoleType rolTypeId))
            -- Add the completed Role instances to the context.
            -- SYNCHRONISATION by ContextDelta and UniverseRoleDelta.
            lift $ lift $ addRoleInstancesToContext contextInstanceId (EnumeratedRoleType rolTypeId) (snd <$> rolInstances')
            -- SYNCHRONISATION: through setBinding adds a RoleBindingDelta.
            -- CURRENTUSER
            for_ rolInstances' addBindingToRoleInstance
            pure $ toArray (snd <$> rolInstances')

          lift $ lift2 $ void $ saveEntiteit contextInstanceId
          -- Add a UniverseContextDelta to the Transaction with the union of the users of the RoleBindingDeltas.
          lift $ insertDelta (DeltaInTransaction{ users, delta: universeContextDelta}) i
          -- Add a UniverseRoleDelta to the Transaction for the external role.
          PerspectRol{universeRoleDelta} <- lift $ lift $ lift $ getPerspectRol buitenRol
          lift $ insertDelta (DeltaInTransaction{ users, delta: universeRoleDelta}) (i + 1)
          pure contextInstanceId
  where

    -- Constructed with a UniverseRoleDelta.
    constructSingleRoleInstance ::
      ContextInstance ->
      EnumeratedRoleType ->
      Int ->
      RolSerialization ->
      (WriterT (Array RoleInstance) (ExceptT PerspectivesError MonadPerspectivesTransaction)) (Tuple RolSerialization RoleInstance)
    constructSingleRoleInstance contextInstanceId roleType i s@(RolSerialization{id:mid, properties, binding}) = do
      -- create an empty role
      binding' <- lift $ lift $ lift2 (traverse expandDefaultNamespaces binding)
      case (deconstructLocalName $unwrap roleType) of
        Nothing -> throwError $ NotWellFormedName upperLeft "not a valid identifier"
        Just localRoleName -> do
          roleInstance <- case mid of
            Nothing -> lift $ lift $ constructEmptyRole contextInstanceId localRoleName (RoleInstance <$> binding')
            Just ident -> lift $ lift $ (_._id <<< unwrap) <$> constructRoleWithIsMe contextInstanceId roleType i (RoleInstance ident) (RoleInstance <$> binding')
          pure $ Tuple s roleInstance
      where
        -- | Constructs an empty role and caches it. `localName` should be the local name of the roleType.
        constructEmptyRole :: ContextInstance -> String -> Maybe RoleInstance -> MonadPerspectivesTransaction RoleInstance
        constructEmptyRole contextInstance localRoleName bnd = do
          rolInstanceId <- pure $ RoleInstance (unwrap contextInstance <> "$" <> localRoleName <> "_" <> (rol_padOccurrence i))
          void $ constructRoleWithIsMe contextInstance roleType i rolInstanceId bnd
          pure rolInstanceId

    -- SYNCHRONISATION: through setBinding adds a RoleBindingDelta.
    addBindingToRoleInstance :: (Tuple RolSerialization RoleInstance) -> (WriterT (Array RoleInstance) (ExceptT PerspectivesError MonadPerspectivesTransaction)) Unit
    addBindingToRoleInstance (Tuple (RolSerialization{binding, properties}) roleInstance) = do
      users <- case binding of
        -- the roleInstance is only in cache, save it.
        Nothing -> lift $ lift $ lift2 $ saveEntiteit roleInstance *> pure []
        Just bnd -> do
          expandedBinding <- RoleInstance <$> (lift $ lift $ lift2 $ expandDefaultNamespaces bnd)
          -- setBinding saves, too.
          lift $ lift $ setBinding_ roleInstance expandedBinding
      case properties of
        (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
          lift $ lift $ setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
      tell users

-- | Constructs an empty context, caches it.
-- | Whenever constructEmptyContext is applied, a UniverseContextDelta should be added
-- | to the Transaction (and also a UniverseRoleDelta for the external role).
-- | However, in order to be able to add all users with a perspective,
-- | it is better to construct that Delta in the calling function.
-- | RULE TRIGGERING for the context through the step `role <Type of External Role of the Context`.
constructEmptyContext :: ContextInstance -> String -> String -> PropertySerialization -> ExceptT PerspectivesError MonadPerspectivesTransaction PerspectContext
constructEmptyContext contextInstanceId ctype localName externeProperties = do
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
              , deltaType: ConstructEmptyRole
              , subject } }
      })
  (lift $ lift2 $ findRoleRequests (ContextInstance "model:System$AnyContext") (EnumeratedRoleType $ unwrap pspType <> "$External")) >>= lift <<< addCorrelationIdentifiersToTransactie
  -- TODO. Op dit moment van constructie aangekomen is nog niet vastgelegd wie 'me' is in de context.
  case externeProperties of
    (PropertySerialization props) -> lift do
      forWithIndex_ props \propertyTypeId values ->
        setProperty [externalRole] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
      when (isEmpty props) (lift2 $ void $ saveEntiteit externalRole)
  pure contextInstance

-- | Construct a Role instance for an existing Context instance.
-- | This function is complete w.r.t. the five responsibilities.
-- | Notice that roleType must be a well-formed identifier!
-- | Retrieves from the repository the model that holds the RoleType, if necessary.
createAndAddRoleInstance :: EnumeratedRoleType -> String -> RolSerialization -> MonadPerspectivesTransaction RoleInstance
createAndAddRoleInstance roleType@(EnumeratedRoleType rtype) id (RolSerialization{id: mRoleId, properties, binding}) = do
  contextInstanceId <- ContextInstance <$> (lift2 $ expandDefaultNamespaces id)
  rolInstances <- lift2 (contextInstanceId ##= getEnumeratedRoleInstances roleType)
  (EnumeratedRole{kindOfRole}) <- lift2 $ getEnumeratedRole roleType
  -- create a role with isMe set. We need that for the case when roleType is a User role.
  -- Without the binding, we cannot judge for whom the UniverseRoleDelta and ContextDelta would be.
  (PerspectRol r@{_id:roleInstance, isMe}) <- case mRoleId of
    Nothing -> do
      rolInstanceId <- pure $ RoleInstance (unwrap contextInstanceId <> "$" <> (unsafePartial $ fromJust (deconstructLocalName $ unwrap roleType)) <> "_" <> (rol_padOccurrence (getNextRolIndex rolInstances)))
      constructRoleWithIsMe contextInstanceId roleType (getNextRolIndex rolInstances) rolInstanceId (RoleInstance <$> binding)
    Just roleId -> constructRoleWithIsMe contextInstanceId roleType (getNextRolIndex rolInstances) (RoleInstance roleId) (RoleInstance <$> binding)

  -- Serialise as Deltas if we bind to a user that is not me.
  when (isJust binding && kindOfRole == UserRole && not isMe)
      -- If the binding is a User that is not me,
      -- serialise the context for that User.
      -- We must do that before we generate Deltas that add a role to the context,
      -- otherwise, on reconstructing from the Deltas, the peer'll try to add a rol to a
      -- non-existing context!
      -- We assume here that a User has just one role in the context (otherwise the serialisation is superfluous).
      (contextInstanceId `serialisedAsDeltasFor` roleInstance)
  -- Then add the new Role instance to the context.
  addRoleInstancesToContext contextInstanceId roleType (singleton roleInstance)
  -- Then add the binding
  case binding of
    Nothing -> pure unit
    Just bnd -> do
      expandedBinding <- RoleInstance <$> (lift2 $ expandDefaultNamespaces bnd)
      void $ setBinding_ roleInstance expandedBinding
  -- Then add the properties
  case properties of
    (PropertySerialization props) -> forWithIndex_ props \propertyTypeId values ->
      setProperty [roleInstance] (EnumeratedPropertyType propertyTypeId) (Value <$> values)
  pure roleInstance

-- | Constructs a role with a correct value for isMe and caches it. `localName` should be the local name of the roleType.
-- | The role instance is cached.
constructRoleWithIsMe ::
  ContextInstance ->
  EnumeratedRoleType ->
  Int ->
  RoleInstance ->
  Binding ->
  MonadPerspectivesTransaction PerspectRol
constructRoleWithIsMe contextInstance roleType i rolInstanceId binding' = do
  isMe <- getIsMe
  author <- getAuthor
  subject <- getSubject
  role <- pure (PerspectRol defaultRolRecord
    { _id = rolInstanceId
    , pspType = roleType
    , context = contextInstance
    , occurrence = i
    , isMe = isMe
    , binding = binding'
    , universeRoleDelta =
        SignedDelta
          { author
          , encryptedDelta: sign $ encodeJSON $ UniverseRoleDelta
            { id: contextInstance
            , roleInstances: (SNEA.singleton rolInstanceId)
            , roleType
            , deltaType: ConstructEmptyRole
            , subject } }
    })
  void $ lift2 $ cacheEntity rolInstanceId role
  pure role

  where
    getIsMe :: MonadPerspectivesTransaction Boolean
    getIsMe = do
      case binding' of
        Nothing -> pure false
        Just b -> do
          PerspectRol{isMe} <- lift2 $ getPerspectEntiteit b
          pure isMe
