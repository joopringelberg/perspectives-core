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

module Perspectives.RunMonadPerspectivesTransaction where

import Control.Monad.AvarMonadAsk (get, modify) as AA
import Control.Monad.Error.Class (catchError, try)
import Control.Monad.Reader (lift, runReaderT)
import Data.Array (filterA, head, null, sort)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (new)
import Foreign.Object (empty)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.ContextStateCompiler (enteringState, evaluateContextState, exitingState)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, StateEvaluation(..), liftToInstanceLevel, (##>), (##>>), (##=))
import Perspectives.Deltas (distributeTransaction)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DependencyTracking.Dependency (lookupActiveSupportedEffect)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (addModelToLocalStore')
import Perspectives.Identifiers (hasLocalName)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.Combinators (exists')
import Perspectives.Instances.ObjectGetters (context, contextType, getActiveRoleStates, getActiveStates, roleType)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile, tryRemoveEntiteit)
import Perspectives.PerspectivesState (addBinding, publicRepository, pushFrame, restoreFrame)
import Perspectives.Query.UnsafeCompiler (getCalculatedRoleInstances, getMyType)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RoleStateCompiler (enteringRoleState, evaluateRoleState, exitingRoleState)
import Perspectives.SaveUserData (removeContextInstance, removeRoleInstance)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransaction, isEmptyTransaction)
import Perspectives.Types.ObjectGetters (roleRootStates, contextRootStates)
import Prelude (Unit, bind, discard, join, pure, show, unit, void, ($), (<$>), (<<<), (<>), (=<<), (>=>), (>>=))

-----------------------------------------------------------
-- RUN MONADPERSPECTIVESTRANSACTION
-----------------------------------------------------------
-- | Runs an update function (a function in MonadPerspectivesTransaction that produces deltas),
-- | runs actions as long as they are triggered, sends deltas to other participants and re-runs active queries
runMonadPerspectivesTransaction :: forall o.
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives (Array o))
runMonadPerspectivesTransaction authoringRole a = runMonadPerspectivesTransaction' true authoringRole a

runMonadPerspectivesTransaction' :: forall o.
  Boolean ->
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives (Array o))
runMonadPerspectivesTransaction' share authoringRole a = getUserIdentifier >>= lift <<< createTransaction authoringRole >>= lift <<< new >>= runReaderT (runArrayT run)
  where
    run :: MonadPerspectivesTransaction o
    run = do
      -- 1. Execute the value that accumulates Deltas in a Transaction.
      r <- a

      -- 2. Now run all actions.
      ft@(Transaction{correlationIdentifiers}) <- lift AA.get >>= runAllAutomaticActions

      -- 3. Send deltas to other participants, save changed domeinfiles.
      if share then lift $ lift $ distributeTransaction ft else pure unit

      -- 4. Run effects.
      -- log "==========RUNNING EFFECTS============"
      -- Sort from low to high, so we can never actualise a client side component after it has been removed.
      lift $ lift $ for_ (sort correlationIdentifiers) \corrId -> do
        mEffect <- pure $ lookupActiveSupportedEffect corrId
        case mEffect of
          Nothing -> pure unit
          (Just {runner}) -> do
            -- logShow corrId
            runner unit
      pure r

    runAllAutomaticActions :: Transaction -> MonadPerspectivesTransaction Transaction
    runAllAutomaticActions previousTransaction = do
      -- Run monotonic actions first.
      (ft@(Transaction{contextsToBeRemoved, rolesToBeRemoved, modelsToBeRemoved}) :: Transaction) <- runMonotonicActions previousTransaction
      -- Install a fresh transaction.
      lift $ void $ AA.modify cloneEmptyTransaction
      -- Detach and remove instances, collecting new information in the fresh Transaction.
      -- log ("Will remove these contexts: " <> show contextsToBeRemoved)
      -- TODO: ZONDER AUTHORINGROLE zal de ontvanger het niet accepteren.
      for_ contextsToBeRemoved \(Tuple ctxt authorizedRole) -> removeContextInstance ctxt authorizedRole
      -- log ("Will remove these roles: " <> show rolesToBeRemoved)
      for_ rolesToBeRemoved removeRoleInstance
      -- log ("Will remove these models: " <> show modelsToBeRemoved)
      lift2 $ for_ modelsToBeRemoved tryRemoveEntiteit
      -- If the new transaction is not empty, run again.
      nt <- lift AA.get
      if isEmptyTransaction nt
        then pure previousTransaction
        else pure <<< (<>) previousTransaction =<< runAllAutomaticActions nt


-- | Run and discard the transaction.
runSterileTransaction :: forall o. MonadPerspectivesTransaction o -> (MonadPerspectives (Array o))
runSterileTransaction a =
  getUserIdentifier
  >>= lift <<< createTransaction (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
  >>= lift <<< new
  >>= runReaderT (runArrayT a)

-- | Evaluates state transitions. Executes automatic actions. Notifies users. Collects deltas in a new transaction.
-- | If any are collected, recursively calls itself.
-- | Notice that context- and role instances are still not actually removed from cache, nor from Couchdb, while
-- | this function runs.
runMonotonicActions :: Transaction -> MonadPerspectivesTransaction Transaction
runMonotonicActions previousTransaction@(Transaction{invertedQueryResults, createdContexts, createdRoles, contextsToBeRemoved, rolesToBeRemoved}) = do
  -- log "==========RUNNING STATES============"
  -- Install a fresh transaction.
  lift $ void $ AA.modify cloneEmptyTransaction
  (stateEvaluations :: Array StateEvaluation) <- lift2 $ join <$> traverse computeStateEvaluations invertedQueryResults
  -- Evaluate all collected stateEvaluations.
  for_ stateEvaluations \s -> case s of
    ContextStateEvaluation stateId contextId roleType -> do
      -- Provide a new frame for the current context variable binding.
      oldFrame <- lift2 pushFrame
      lift2 $ addBinding "currentcontext" [unwrap contextId]
      -- Error boundary.
      catchError (evaluateContextState contextId roleType stateId)
        \e -> logPerspectivesError $ Custom ("Cannot evaluate context state, because " <> show e)
      lift2 $ restoreFrame oldFrame
    RoleStateEvaluation stateId roleId roleType -> do
      cid <- lift2 (roleId ##>> context)
      oldFrame <- lift2 pushFrame
      -- NOTE. This may not be necessary; we have no analysis in runtime whether these variables occur in
      -- expressions in state.
      lift2 $ addBinding "currentcontext" [unwrap cid]
      -- TODO. add binding for "currentobject" or "currentsubject"?!
      -- `stateId` points the way: stateFulObject (StateFulObject) tells us whether it is subject- or object state.
      catchError (evaluateRoleState roleId roleType stateId)
        \e -> logPerspectivesError $ Custom ("Cannot evaluate role state, because " <> show e)
      lift2 $ restoreFrame oldFrame
  -- Enter the rootState of new contexts.
  for_ createdContexts
    \ctxt -> do
      (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
      case mmyType of
        Nothing -> pure unit
        Just myType -> do
          states <- lift2 (ctxt ##= contextType >=> liftToInstanceLevel contextRootStates)
          -- Provide a new frame for the current context variable binding.
          oldFrame <- lift2 pushFrame
          lift2 $ addBinding "currentcontext" [unwrap ctxt]
          -- Error boundary.
          catchError (for_ states (enteringState ctxt myType))
            \e -> logPerspectivesError $ Custom ("Cannot enter state, because " <> show e)
          lift2 $ restoreFrame oldFrame
  -- Enter the rootState of roles that are created.
  for_ createdRoles
    \rid -> do
      ctxt <- lift2 (rid ##>> context)
      (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
      case mmyType of
        Nothing -> pure unit
        Just myType -> do
          states <- lift2 (rid ##= roleType >=> liftToInstanceLevel roleRootStates)
          oldFrame <- lift2 pushFrame
          lift2 $ addBinding "currentcontext" [unwrap ctxt]
          -- Error boundary.
          catchError (for_ states (enteringRoleState rid myType))
            \e -> logPerspectivesError $ Custom ("Cannot enter role state, because " <> show e)
          lift2 $ restoreFrame oldFrame
  -- Exit the rootState of roles that are removed, unless we did so before.
  activeRoleInstances <- lift2 $ filterA (\rid -> rid ##>> exists' getActiveRoleStates) rolesToBeRemoved
  for_ activeRoleInstances
    \rid -> do
      ctxt <- lift2 (rid ##>> context)
      (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
      case mmyType of
        Nothing -> pure unit
        Just myType -> do
          states <- lift2 (rid ##= roleType >=> liftToInstanceLevel roleRootStates)
          if null states
            then pure unit
            else do
              oldFrame <- lift2 pushFrame
              lift2 $ addBinding "currentcontext" [unwrap ctxt]
              -- Error boundary.
              catchError (for_ states (exitingRoleState rid myType))
                \e -> logPerspectivesError $ Custom ("Cannot enter role state, because " <> show e)
              lift2 $ restoreFrame oldFrame
  -- Exit the rootState of contexts that are deleted, unless we did so before.
  activeContextInstances <- lift2 $ filterA (\(Tuple rid _) -> rid ##>> exists' getActiveStates) contextsToBeRemoved
  for_ activeContextInstances
    \(Tuple ctxt _) -> do
      (mmyType :: Maybe RoleType) <- lift2 (ctxt ##> getMyType)
      case mmyType of
        Nothing -> pure unit
        Just myType -> do
          states <- lift2 (ctxt ##= contextType >=> liftToInstanceLevel contextRootStates)
          if null states
            then pure unit
            else do
              -- Provide a new frame for the current context variable binding.
              oldFrame <- lift2 pushFrame
              lift2 $ addBinding "currentcontext" [unwrap ctxt]
              -- Error boundary.
              catchError (for_ states (exitingState ctxt myType))
                \e -> logPerspectivesError $ Custom ("Cannot exit state, because " <> show e)
              lift2 $ restoreFrame oldFrame
  nt <- lift AA.get
  if isEmptyTransaction nt
    then pure previousTransaction
    else pure <<< (<>) previousTransaction =<< runMonotonicActions nt
  where
    -- Add to each context or role instance the user role type and the RootState type.
    computeStateEvaluations :: InvertedQueryResult -> MonadPerspectives (Array StateEvaluation)
    computeStateEvaluations (ContextStateQuery contextInstances) = join <$> do
      activeInstances <- filterA (\rid -> rid ##>> exists' getActiveStates) contextInstances
      case head activeInstances of
        Nothing -> pure []
        Just contextInstance -> do
          -- States includes root states of Aspects.
          states <- contextInstance ##= contextType >=> liftToInstanceLevel contextRootStates
          for activeInstances \cid -> do
            -- Note that the user may play different roles in the various context instances.
            (mmyType :: Maybe RoleType) <- cid ##> getMyType
            case mmyType of
              Nothing -> pure []
              Just (CR myType) -> if isGuestRole myType
                then do
                  (mmguest :: Maybe RoleInstance) <- cid ##> getCalculatedRoleInstances myType
                  case mmguest of
                    -- If the Guest role is not filled, don't execute bots on its behalf!
                    Nothing -> pure []
                    otherwise -> pure $ (\state -> ContextStateEvaluation state cid (CR myType)) <$> states
                else pure $ (\state -> ContextStateEvaluation state cid (CR myType)) <$> states
              Just (ENR myType) -> pure $ (\state -> ContextStateEvaluation state cid (ENR myType)) <$> states
    computeStateEvaluations (RoleStateQuery roleInstances) = join <$> do
      activeInstances <- filterA (\rid -> rid ##>> exists' getActiveRoleStates) roleInstances
      case head activeInstances of
        Nothing -> pure []
        Just roleInstance -> do
          -- Neem aspecten hier ook mee!
          states <- roleInstance ##= roleType >=> liftToInstanceLevel roleRootStates
          -- If the roleInstance has no recorded states, this means it has been marked for removal
          -- and has exited all its states. We should not do that again!
          for activeInstances \rid -> do
            (mmyType :: Maybe RoleType) <- rid ##> context >=> getMyType
            case mmyType of
              Nothing -> pure []
              Just (CR myType) -> if isGuestRole myType
                then do
                  (mmguest :: Maybe RoleInstance) <- rid ##> context >=> getCalculatedRoleInstances myType
                  case mmguest of
                    Nothing -> pure []
                    otherwise -> pure $ (\state -> RoleStateEvaluation state rid (CR myType)) <$> states
                else pure $ (\state -> RoleStateEvaluation state rid (CR myType)) <$> states
              Just (ENR myType) -> pure $ (\state -> RoleStateEvaluation state rid (ENR myType)) <$> states

    isGuestRole :: CalculatedRoleType -> Boolean
    isGuestRole (CalculatedRoleType cr) = cr `hasLocalName` "Guest"

lift2 :: forall a. MonadPerspectives a -> MonadPerspectivesTransaction a
lift2 = lift <<< lift

-----------------------------------------------------------
-- LOADMODELIFMISSING
-----------------------------------------------------------
-- | Retrieves from the repository the model, if necessary.
-- TODO. This function relies on a repository URL in PerspectivesState. That is a stub.
loadModelIfMissing :: String -> MonadPerspectivesTransaction Unit
loadModelIfMissing modelName = do
  mDomeinFile <- lift2 $ tryRetrieveDomeinFile modelName
  if isNothing mDomeinFile
    then do
      repositoryUrl <- lift2 publicRepository
      addModelToLocalStore' (repositoryUrl <> modelName) true
      -- Now create a binding of the model description in sys:PerspectivesSystem$ModelsInUse.
      (lift2 $ try $ getDomeinFile (DomeinFileId modelName)) >>=
        handleDomeinFileError "loadModelIfMissing"
        \(DomeinFile{modelDescription}) -> do
          mySys <- lift2 $ getMySystem
          case modelDescription of
            Nothing -> pure unit
            Just (PerspectRol{_id}) -> void $ createAndAddRoleInstance
              (EnumeratedRoleType "model:System$PerspectivesSystem$ModelsInUse")
              mySys
              (RolSerialization{id: Nothing, properties: PropertySerialization empty, binding: Just $ unwrap _id})
    else pure unit
