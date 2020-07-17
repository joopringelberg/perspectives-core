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

-- | The functions in this module save and remove contexts and roles.
-- | These updates fully take care of the basic responsibilities of the PDR:
-- | PERSISTENCE
-- | SYNCHRONISATION
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | CURRENTUSER
-- | except for saveContextInstance (no CURRENTUSER, nor SYNCHRONISATION)

module Perspectives.SaveUserData
  ( saveContextInstance
  , removeContextInstance
  , removeRoleInstance
  , removeAllRoleInstances)

  where

import Control.Monad.AvarMonadAsk (modify) as AA
import Control.Monad.State (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (deleteAt, findIndex, index, modifyAt, nub)
import Data.Array.NonEmpty (elemIndex, fromArray, head, singleton, filter)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Foreign.Generic (encodeJSON)
import Foreign.Object (values)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.Update (removeBinding, removeRoleInstancesFromContext)
import Perspectives.Authenticate (sign)
import Perspectives.CollectAffectedContexts (addRoleObservingContexts, aisInRoleDelta, lift2, usersWithPerspectiveOnRoleInstance)
import Perspectives.ContextAndRole (context_buitenRol, context_iedereRolInContext, context_pspType)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, Updater, (##=))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addDelta)
import Perspectives.DependencyTracking.Dependency (findBinderRequests, findBindingRequests, findRoleRequests)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances, subjectForContextInstance)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Persistent (getPerspectContext, getPerspectRol, removeEntiteit, saveEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.Sync.AffectedContext (AffectedContext(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..), SubjectOfAction(..), UniverseContextDelta(..), UniverseContextDeltaType(..))
import Prelude (Unit, bind, discard, join, pure, unit, void, ($), (>>=), (<>), eq)

-- | This function takes care of
-- | PERSISTENCE
-- | QUERY UPDATES
-- | RULE TRIGGERING
-- | but nothing else.
saveContextInstance :: Updater ContextInstance
saveContextInstance id = do
  (ctxt :: PerspectContext) <- lift2 $ saveEntiteit id
  forWithIndex_ (context_iedereRolInContext ctxt) \roleName instances' ->
    case fromArray instances' of
      Nothing -> pure unit
      -- RULE TRIGGERING
      Just instances -> addRoleObservingContexts id (EnumeratedRoleType roleName) (head instances)
  for_ (iedereRolInContext ctxt) \(rol :: RoleInstance) -> do
    (PerspectRol{_id, binding, pspType, gevuldeRollen}) <- lift2 $ saveEntiteit rol
    case binding of
      Nothing -> pure unit
      Just b -> (lift2 $ findBinderRequests b pspType) >>= addCorrelationIdentifiersToTransactie
    forWithIndex_ gevuldeRollen \_ instances -> for_ instances \binder ->
      (lift2 $ findBindingRequests binder) >>= addCorrelationIdentifiersToTransactie
    -- For rule triggering, not for the delta or SYNCRONISATION:
    if isJust binding
      then void $ aisInRoleDelta $ RoleBindingDelta
        { id: rol
        , binding: binding
        , oldBinding: Nothing
        , deltaType: SetBinding
        , roleWillBeRemoved: false
        -- we do not use the subject but have to provide it.
        , subject: UserInstance (RoleInstance "")
        }
      else pure unit
  (_ :: PerspectRol) <- lift2 $ saveEntiteit (context_buitenRol ctxt)
  -- For roles with a binding equal to R: detect the binder <RoleType> requests for R
  -- For roles that are bound by role R: detect the binding requests for R.
  pure unit

iedereRolInContext :: PerspectContext -> Array RoleInstance
iedereRolInContext ctxt = nub $ join $ values (context_iedereRolInContext ctxt)

-- | Removes the ContextInstance both from the cache and from the database and adds it to the Transaction.
-- | This function is complete w.r.t. the five responsibilities (ignoring CURRENTUSER).
removeContextInstance :: Updater ContextInstance
removeContextInstance id = do
  (ctxt@(PerspectContext{pspType})) <- lift $ lift $ getPerspectContext id
  -- RULE TRIGGERING and QUERY UPDATES.
  (Tuple _ users) <- runWriterT $ do
    forWithIndex_ (context_iedereRolInContext ctxt) \roleName instances' -> remove (EnumeratedRoleType roleName) instances'
    remove (EnumeratedRoleType ((unwrap pspType) <> "$External")) [(context_buitenRol ctxt)]
  -- PERSISTENCE
  (_ :: PerspectContext) <- lift $ lift $ removeEntiteit id
  -- SYNCHRONISATION
  subject <- subjectForContextInstance id
  me <- lift2 getUserIdentifier
  addDelta $ DeltaInTransaction
    { users
    , delta: SignedDelta
        { author: me
        , encryptedDelta: sign $ encodeJSON $ UniverseContextDelta
          { id
          , contextType: context_pspType ctxt
          , deltaType: RemoveContextInstance
          , subject
          }}}

  -- now remove id from the affectedContexts in the Transaction.
  removeAffectedContext id

  where
    -- No need to take care of SYNCHRONISATION for individual role instances.
    -- The receiving PDR will recompute the role instances to remove.
    -- Takes care of RULE TRIGGERING and QUERY UPDATES.
    -- TODO. Maak hier een set van om dubbele te vermijden.
    remove :: EnumeratedRoleType -> Array RoleInstance -> WriterT (Array RoleInstance) MonadPerspectivesTransaction Unit
    remove roleType instances' =
      case fromArray instances' of
        Nothing -> pure unit
        Just instances -> do
          -- Also adds contexts that 'have a vantage point' on the removed roleInstances
          -- in the context, for RULE TRIGGERING (adds them as AffectedContexts).
          users <- lift $ usersWithPerspectiveOnRoleInstance id roleType (head instances)
          tell users
          -- Add correlation identifiers for outstanding Api requests, in order to update
          -- their query results, for QUERY UPDATES.
          lift ((lift2 $ findRoleRequests id roleType) >>= addCorrelationIdentifiersToTransactie)
          -- PERSISTENCE
          for_ instances removeRoleInstance_

    removeAffectedContext :: ContextInstance -> MonadPerspectivesTransaction Unit
    removeAffectedContext cinst = lift $ AA.modify \(Transaction r@{affectedContexts}) -> Transaction (r {affectedContexts = let
      i = findIndex
        (\(AffectedContext{contextInstances}) -> isJust $ elemIndex cinst contextInstances)
        affectedContexts
      in
        case i of
          Nothing -> affectedContexts
          Just n -> let
            AffectedContext {contextInstances, userTypes} = unsafePartial $ fromJust $ index affectedContexts n
            mcontextInstances' = fromArray $ filter (eq cinst) contextInstances
            in
              case mcontextInstances' of
                Nothing -> unsafePartial $ fromJust $ deleteAt n affectedContexts
                Just contextInstances' -> unsafePartial $ fromJust $ modifyAt n
                  (\_ -> AffectedContext{contextInstances: contextInstances', userTypes})
                  affectedContexts
        })

-- | Collects the union of the user role instances that occurr in the bindings.
-- | Takes care of PERSISTENCE for the role instance that is removed.
-- | Takes care of the five responsibilities wrt the binding and the  binders
-- | (the roles that bind the instance that is removed).
removeRoleInstance_ :: RoleInstance -> WriterT (Array RoleInstance) MonadPerspectivesTransaction Unit
removeRoleInstance_ roleId = do
  originalRole@(PerspectRol{gevuldeRollen, binding}) <- lift $ lift2 $ (getPerspectRol roleId)
  -- Remove the role instance from all roles that have it as their binding. This will push Deltas.
  forWithIndex_ gevuldeRollen \_ filledRollen ->
    for_ filledRollen \filledRolId -> do
      (lift $ removeBinding false filledRolId) >>= tell
  -- Remove the binding.
  if isJust binding
    then (lift $ removeBinding true roleId) >>= tell
    else pure unit
  -- Remove from couchdb, remove from the cache.
  void $ lift $ lift2 $ (removeEntiteit roleId :: MonadPerspectives PerspectRol)

-- | Calls removeBinding for the role instance prior to removing it.
-- | Calls removeBinding on all role instances that have this role as their binding.
-- | Calls removeRoleInstancesFromContext.
-- | This function is complete w.r.t. the five responsibilities.
-- | The opposite of this function creates a role instance first and then adds it to a context: [createAndAddRoleInstance](Perspectives.Instances.Builders.html#t:createAndAddRoleInstance).
removeRoleInstance :: RoleInstance -> MonadPerspectivesTransaction Unit
removeRoleInstance roleId = do
  PerspectRol{pspType, context} <- lift2 $ (getPerspectRol roleId)
  removeRoleInstancesFromContext context pspType (singleton roleId)
  void $ runWriterT $ removeRoleInstance_ roleId

-- | Remove all instances of EnumeratedRoleType from the context instance.
-- | Removes all instances from cache, from the database and adds then to deletedRoles in the Transaction.
-- | Removes the role instances from their context.
-- | ContextDelta's are not necessary (see removeRoleInstance).
removeAllRoleInstances :: EnumeratedRoleType -> Updater ContextInstance
removeAllRoleInstances et cid = do
  instances <- lift2 (cid ##= getEnumeratedRoleInstances et)
  case fromArray instances of
    Nothing -> pure unit
    Just instances' -> do
      removeRoleInstancesFromContext cid et instances'
      void $ runWriterT $ for_ instances removeRoleInstance_
