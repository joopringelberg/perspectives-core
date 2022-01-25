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

module Perspectives.Sync.HandleTransaction where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (runExcept)
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Generic (decodeJSON)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.Update (addProperty, addRoleInstancesToContext, deleteProperty, moveRoleInstancesToAnotherContext, removeProperty)
import Perspectives.Authenticate (authenticate)
import Perspectives.Checking.Authorization (roleHasPerspectiveOnExternalRoleWithVerb, roleHasPerspectiveOnPropertyWithVerb, roleHasPerspectiveOnRoleWithVerb)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectives, (##=), (###>>), (##>>))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addCreatedContextToTransaction, addCreatedRoleToTransaction)
import Perspectives.DependencyTracking.Dependency (findRoleRequests)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (buitenRol, unsafeDeconstructModelName)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (roleType, typeOfSubjectOfAction)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (entityExists, saveEntiteit, tryGetPerspectEntiteit)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (RoleType(..), StateIdentifier(..), externalRoleType)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb(..)) as Verbs
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', loadModelIfMissing)
import Perspectives.SaveUserData (removeBinding, removeContextIfUnbound, removeRoleInstance, replaceBinding, setFirstBinding)
import Perspectives.SerializableNonEmptyArray (toArray, toNonEmptyArray)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.Types.ObjectGetters (hasAspect)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), SubjectOfAction(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (Unit, bind, discard, flip, pure, show, unit, void, ($), (+), (<<<), (<>), (>>=), (<$>), (==))

-- TODO. Each of the executing functions must catch errors that arise from unknown types.
-- Inspect the model version of an unknown type and establish whether the resident model is newer or older than
-- the arriving type. If newer, check whether the type is backwards compatible with the arriving older type.
-- If older, retrieve the newer version of the model and check whether the arriving type is backwards compatible with
-- the type in the resident model.
-- If the types are truly incompatible, either ignore the incoming delta (best solution if the resident model is newer)
-- or update to the newer model version (have the end user consent).

executeContextDelta :: ContextDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeContextDelta (ContextDelta{deltaType, id: contextId, roleType, roleInstances, destinationContext, subject} ) signedDelta = do
  log (show deltaType <> " to/from " <> show contextId <> " and " <> show roleInstances)
  case deltaType of
    -- The subject must be allowed to change the role: they must have a perspective on it that includes:
    --  * the verb CreateAndFill, in case a context role is created;
    --  * the verb Create, in case another role is created.
    AddRoleInstancesToContext -> (lift2 $ roleHasPerspectiveOnRoleWithVerb subject roleType [Verbs.Create, Verbs.CreateAndFill]) >>= case _ of
      Left e -> handleError e
      Right _ -> addRoleInstancesToContext contextId roleType ((flip Tuple (Just signedDelta)) <$> (unwrap roleInstances))
    -- NOTE: the perspective should always include the Delete verb.
    MoveRoleInstancesToAnotherContext -> (lift2 $ roleHasPerspectiveOnRoleWithVerb subject roleType [Verbs.Create, Verbs.CreateAndFill]) >>= case _ of
      Left e -> handleError e
      Right _ -> moveRoleInstancesToAnotherContext contextId (unsafePartial $ fromJust destinationContext) roleType (unwrap roleInstances)

executeRoleBindingDelta :: RoleBindingDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeRoleBindingDelta (RoleBindingDelta{id: roleId, binding, deltaType, subject}) signedDelta = do
  log (show deltaType <> " of " <> show roleId <> " (to) " <> show binding)
  roleType' <- lift2 (roleId ##>> roleType)
  (lift2 $ roleHasPerspectiveOnRoleWithVerb subject roleType' [Verbs.Fill, Verbs.CreateAndFill]) >>= case _ of
    Left e -> handleError e
    Right _ -> case deltaType of
      SetFirstBinding -> void $ setFirstBinding roleId (unsafePartial $ fromJust binding) (Just signedDelta)
      RemoveBinding -> void $ removeBinding roleId
      ReplaceBinding -> void $ replaceBinding roleId (unsafePartial $ fromJust binding) (Just signedDelta)

-- TODO. Wat met SetPropertyValue?
executeRolePropertyDelta :: RolePropertyDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeRolePropertyDelta (RolePropertyDelta{id, deltaType, values, property, subject}) signedDelta = do
  log (show deltaType <> " for " <> show id <> " and property " <> show property)
  case deltaType of
    AddProperty -> do
      -- we need not check whether the model is known if we assume valid transactions:
      -- a role instance creation delta must have preceded the property delta.
      (lift2 $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.AddPropertyValue) >>= case _ of
        Left e -> handleError e
        Right _ -> addProperty [id] property (flip Tuple (Just signedDelta) <$> values)
    RemoveProperty -> (lift2 $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.RemovePropertyValue) >>= case _ of
      Left e -> handleError e
      Right _ -> removeProperty [id] property values
    DeleteProperty -> (lift2 $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.DeleteProperty) >>= case _ of
      Left e -> handleError e
      Right _ -> deleteProperty [id] property

-- TODO. Wat we nodig hebben, is een secundair kanaal naar de client waarin we
-- fouten en waarschuwingen kunnen sturen.
-- Totdat we dat hebben, zetten we een waarschuwing op de console.
handleError :: PerspectivesError -> MonadPerspectivesTransaction Unit
handleError e = liftEffect $ log (show e)

-- | Retrieves from the repository the model that holds the ContextType, if necessary.
-- | The ConstructExternalRole always precedes the UniverseContextDelta for the context it is the external
-- | role for. Hence we only have to check whether the external role exists.
executeUniverseContextDelta :: UniverseContextDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseContextDelta (UniverseContextDelta{id, contextType, deltaType, subject}) signedDelta = do
  log (show deltaType <> " with id " <> show id <> " and with type " <> show contextType)
  externalRoleExists <- lift2 $ entityExists (RoleInstance $ buitenRol $ unwrap id)
  if externalRoleExists
    then case deltaType of
      ConstructEmptyContext -> do
        (exists :: Maybe PerspectContext) <- lift2 $ tryGetPerspectEntiteit id
        if isNothing exists
          then do
            contextInstance <- pure
              (PerspectContext defaultContextRecord
                { _id = id
                , displayName = unwrap id
                , pspType = contextType
                , buitenRol = RoleInstance $ buitenRol $ unwrap id
                , universeContextDelta = signedDelta
                , states = [StateIdentifier $ unwrap contextType]
                })
            lift2 $ void $ cacheEntity id contextInstance
            (lift2 $ findRoleRequests (ContextInstance "model:System$AnyContext") (externalRoleType contextType)) >>= addCorrelationIdentifiersToTransactie
            addCreatedContextToTransaction id
          else pure unit
    else do
      (subjectType :: RoleType) <- lift2 $ typeOfSubjectOfAction subject
      logPerspectivesError $ UnauthorizedForContext "auteur" subjectType contextType

-- | Retrieves from the repository the model that holds the RoleType, if necessary.
executeUniverseRoleDelta :: UniverseRoleDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseRoleDelta (UniverseRoleDelta{id, roleType, roleInstances, authorizedRole, deltaType, subject}) s = do
  log (show deltaType <> " for/from " <> show id <> " with ids " <> show roleInstances <> " with type " <> show roleType)
  loadModelIfMissing (unsafeDeconstructModelName $ unwrap roleType)
  case deltaType of
    ConstructEmptyRole -> do
      -- We have to check this case: a user is allowed to create himself.
      if userCreatesThemselves
        then constructAnotherRole_
        else do
          -- Check if the author has a perspective on the role to be created that includes
          -- the verb Create.
          (lift2 $ roleHasPerspectiveOnRoleWithVerb subject roleType [Verbs.Create, Verbs.CreateAndFill]) >>= case _ of
            Left e -> handleError e
            Right _ -> constructAnotherRole_
    ConstructExternalRole -> do
      -- Notice that merely constructing a role has no consequences for the responsibilities
      -- QUERY UPDATES, RULE TRIGGERING, PERSISTENCE or CURRENTUSER.
      -- If roleType has the aspect model:System$RootContext$External, it can stand alone. These roles can
      -- be created by any user and usually will be created by parsing a CRL file.
      lift2 (roleType ###>> hasAspect (EnumeratedRoleType "model:System$RootContext$External")) >>= if _
        then constructExternalRole
        else (lift2 $ roleHasPerspectiveOnExternalRoleWithVerb subject authorizedRole Verbs.CreateAndFill) >>= case _ of
          Left e -> handleError e
          Right _ -> constructExternalRole
    RemoveRoleInstance -> do
      (lift2 $ roleHasPerspectiveOnRoleWithVerb subject roleType [Verbs.Remove]) >>= case _ of
        Left e -> handleError e
        Right _ -> for_ (toNonEmptyArray roleInstances) removeRoleInstance

    -- TODO Het lijkt niet nuttig om beide cases te behouden.
    RemoveUnboundExternalRoleInstance -> do
      (lift2 $ roleHasPerspectiveOnExternalRoleWithVerb subject authorizedRole Verbs.Delete) >>= case _ of
        Left e -> handleError e
        Right _ -> for_ (toArray roleInstances) (flip removeContextIfUnbound authorizedRole)
    RemoveExternalRoleInstance -> do
      (lift2 $ roleHasPerspectiveOnExternalRoleWithVerb subject authorizedRole Verbs.Delete) >>= case _ of
        Left e -> handleError e
        Right _ -> for_ (toArray roleInstances) (flip removeContextIfUnbound authorizedRole)
    where
      userCreatesThemselves :: Boolean
      userCreatesThemselves = case subject of
        (UserInstance r) -> r == (head ((toNonEmptyArray roleInstances) :: NonEmptyArray RoleInstance))
        otherwise -> false

      constructAnotherRole_ :: MonadPerspectivesTransaction Unit
      constructAnotherRole_ = do
        -- find the number of roleinstances in the context.
        offset <- (lift2 (id ##= getRoleInstances (ENR roleType))) >>= pure <<< getNextRolIndex
        forWithIndex_ (toNonEmptyArray roleInstances) \i roleInstance -> do
          (exists :: Maybe PerspectRol) <- lift2 $ tryGetPerspectEntiteit roleInstance
          if isNothing exists
            then void $ constructEmptyRole_ id (offset + i) roleInstance
            else pure unit
            -- TODO save it?

      constructEmptyRole_ :: ContextInstance -> Int -> RoleInstance -> MonadPerspectivesTransaction Boolean
      constructEmptyRole_ contextInstance i rolInstanceId = do
        (exists :: Maybe PerspectRol) <- lift2 $ tryGetPerspectEntiteit rolInstanceId
        if isNothing exists
          then do
            role <- pure (PerspectRol defaultRolRecord
                  { _id = rolInstanceId
                  , pspType = roleType
                  , context = contextInstance
                  , occurrence = i
                  , universeRoleDelta = s
                  , states = [StateIdentifier $ unwrap roleType]
                  })
            addCreatedRoleToTransaction rolInstanceId
            void $ lift2 $ cacheEntity rolInstanceId role
            pure true
          else pure false

      constructExternalRole  :: MonadPerspectivesTransaction Unit
      constructExternalRole = do
        externalRole <- pure (head $ toNonEmptyArray roleInstances)
        log ("ConstructExternalRole in " <> show id)
        constructEmptyRole_ id 0 externalRole >>= if _
          then lift2 $ void $ saveEntiteit externalRole
          else pure unit

-- | Execute all Deltas in a run that does not distrubute.
executeTransaction :: TransactionForPeer -> MonadPerspectives Unit
executeTransaction t@(TransactionForPeer{deltas}) = void $ (runMonadPerspectivesTransaction'
    false
    (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
    (for_ deltas f))
  where
    f :: SignedDelta -> MonadPerspectivesTransaction Unit
    f s@(SignedDelta{author, encryptedDelta}) = executeDelta $ authenticate (RoleInstance author) encryptedDelta
      where
        executeDelta :: Maybe String -> MonadPerspectivesTransaction Unit
        -- For now, we fail silently on deltas that cannot be authenticated.
        executeDelta Nothing = pure unit
        executeDelta (Just stringifiedDelta) = catchError
          (case runExcept $ decodeJSON stringifiedDelta of
            Right d1 -> executeRolePropertyDelta d1 s
            Left _ -> case runExcept $ decodeJSON stringifiedDelta of
              Right d2 -> executeRoleBindingDelta d2 s
              Left _ -> case runExcept $ decodeJSON stringifiedDelta of
                Right d3 -> executeContextDelta d3 s
                Left _ -> case runExcept $ decodeJSON stringifiedDelta of
                  Right d4 -> executeUniverseRoleDelta d4 s
                  Left _ -> case runExcept $ decodeJSON stringifiedDelta of
                    Right d5 -> executeUniverseContextDelta d5 s
                    Left _ -> log ("Failing to parse and execute: " <> stringifiedDelta))
          (\e -> liftEffect $ log (show e))
