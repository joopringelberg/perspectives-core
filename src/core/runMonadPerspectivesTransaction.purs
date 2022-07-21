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
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Reader (lift, runReaderT)
import Data.Array (filterA, head, length, null, reverse, sort, unsafeIndex)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, traverse)
import Effect.Aff.AVar (new, put, take, tryRead)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.ContextStateCompiler (enteringState, evaluateContextState, exitingState)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, StateEvaluation(..), MPT, liftToInstanceLevel, (##=), (##>), (##>>))
import Perspectives.Deltas (distributeTransaction)
import Perspectives.DependencyTracking.Dependency (lookupActiveSupportedEffect)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (addModelToLocalStore')
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (hasLocalName)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.Combinators (exists')
import Perspectives.Instances.ObjectGetters (context, contextType, getActiveRoleStates, getActiveStates, roleType)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Models (modelsInUseRole)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile, tryRemoveEntiteit)
import Perspectives.PerspectivesState (addBinding, publicRepository, pushFrame, restoreFrame, transactionFlag)
import Perspectives.Query.UnsafeCompiler (getCalculatedRoleInstances, getMyType)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), DomeinFileId(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RoleStateCompiler (enteringRoleState, evaluateRoleState, exitingRoleState)
import Perspectives.SaveUserData (changeRoleBinding, removeContextInstance, removeRoleInstance, stateEvaluationAndQueryUpdatesForContext, stateEvaluationAndQueryUpdatesForRole)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..), contextsToBeRemoved)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransaction, isEmptyTransaction)
import Perspectives.Types.ObjectGetters (roleRootStates, contextRootStates)
import Prelude (Unit, bind, discard, join, pure, show, unit, void, ($), (*>), (+), (<$>), (<<<), (<>), (=<<), (>=>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- RUN MONADPERSPECTIVESTRANSACTION
-----------------------------------------------------------
-- | Runs an update function (a function in MonadPerspectivesTransaction that produces deltas),
-- | runs actions as long as they are triggered, sends deltas to other participants and re-runs active queries
runMonadPerspectivesTransaction :: forall o.
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runMonadPerspectivesTransaction authoringRole a = runMonadPerspectivesTransaction' true authoringRole a

runMonadPerspectivesTransaction' :: forall o.
  Boolean ->
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runMonadPerspectivesTransaction' share authoringRole a = getUserIdentifier >>= lift <<< createTransaction authoringRole >>= lift <<< new >>= runReaderT run
  where
    run :: MonadPerspectivesTransaction o
    run = do
      -- 0. Only execute the transaction when we can take the flag down.
      t <- lift transactionFlag
      transactionNumber <- lift $ lift $ take t
      log ("Starting transaction " <> show transactionNumber)
      -- 1. Execute the value that accumulates Deltas in a Transaction.
      r <- a

      -- 2. Now run all actions.
      ft@(Transaction{correlationIdentifiers}) <- AA.get >>= runAllAutomaticActions

      -- 3. Send deltas to other participants, save changed domeinfiles.
      if share then lift $ distributeTransaction ft else pure unit

      -- 4. Run effects.
      log "==========RUNNING EFFECTS============"
      -- Sort from low to high, so we can never actualise a client side component after it has been removed.
      for_ (sort correlationIdentifiers) \corrId -> do
        mEffect <- pure $ lookupActiveSupportedEffect corrId
        case mEffect of
          Nothing -> pure unit
          (Just {runner}) -> do
            -- logShow corrId
            lift $ runner unit
      -- 5. Raise the flag
      log ("Ending transaction " <> show transactionNumber)
      _ <- lift $ lift $ put (transactionNumber + 1) t
      pure r

    runAllAutomaticActions :: Transaction -> MonadPerspectivesTransaction Transaction
    runAllAutomaticActions previousTransaction = do
      -- Run monotonic actions, after
      --  * adding all ContextRemovals to the untouchableContexts and
      --  * adding rolesToExit to the untouchableRoles.
      AA.modify (\t -> over Transaction (\tr -> tr
        { untouchableRoles = tr.untouchableRoles <> tr.rolesToExit
        , untouchableContexts = tr.untouchableContexts <> (contextsToBeRemoved tr.scheduledAssignments)
        })
        t)
      at@(Transaction{modelsToBeRemoved, rolesToExit, invertedQueryResults, scheduledAssignments}) <- runEntryAndExitActions previousTransaction

      -- Install a fresh transaction (cloning preserves the untouchableContexts and untouchableRoles
      -- but removes the scheduledAssignments and rolesToExit).
      log "==========RUNNING SCHEDULED ASSIGNMENTS============"
      void $ AA.modify cloneEmptyTransaction
      -- Detach and remove instances, collecting new information in the fresh Transaction.
      for_ (reverse scheduledAssignments) case _ of
        ContextRemoval ctxt authorizedRole -> log ("Remove context " <> unwrap ctxt) *> removeContextInstance ctxt authorizedRole
        RoleRemoval rid -> log ("Remove role " <> unwrap rid) *> removeRoleInstance rid
        -- TODO: moeten we msignedDelta niet meegeven?
        RoleUnbinding filled mNewFiller msignedDelta -> log ("Remove filler of " <> unwrap filled) *> changeRoleBinding filled mNewFiller
        ExecuteDestructiveEffect functionName origin values -> log ("DestructiveEffect: " <> functionName) *> executeEffect functionName origin values
      -- log ("Will remove these models: " <> show modelsToBeRemoved)
      lift $ for_ modelsToBeRemoved tryRemoveEntiteit
      -- Now state has changed. Re-evaluate the resources that may change state.
      (stateEvaluations :: Array StateEvaluation) <- lift $ join <$> traverse computeStateEvaluations invertedQueryResults
      log ("==========RUNNING " <> (show $ length stateEvaluations) <> " OTHER STATE EVALUTIONS============")
      for_ stateEvaluations \s -> case s of
        ContextStateEvaluation stateId contextId roleType -> do
          -- Provide a new frame for the current context variable binding.
          oldFrame <- lift pushFrame
          lift $ addBinding "currentcontext" [unwrap contextId]
          -- Error boundary.
          catchError (evaluateContextState contextId stateId)
            \e -> logPerspectivesError $ Custom ("Cannot evaluate context state, because " <> show e)
          lift $ restoreFrame oldFrame
        RoleStateEvaluation stateId roleId roleType -> do
          cid <- lift (roleId ##>> context)
          oldFrame <- lift pushFrame
          -- NOTE. This may not be necessary; we have no analysis in runtime whether these variables occur in
          -- expressions in state.
          lift $ addBinding "currentcontext" [unwrap cid]
          -- TODO. add binding for "currentobject" or "currentsubject"?!
          -- `stateId` points the way: stateFulObject (StateFulObject) tells us whether it is subject- or object state.
          catchError (evaluateRoleState roleId stateId)
            \e -> logPerspectivesError $ Custom ("Cannot evaluate role state, because " <> show e)
          lift $ restoreFrame oldFrame
      -- If the new transaction is not empty, run again.
      nt <- AA.get
      if isEmptyTransaction nt
        then pure at
        else pure <<< (<>) at =<< runAllAutomaticActions nt
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

-- | Run and discard the transaction.
runSterileTransaction :: forall o. MonadPerspectivesTransaction o -> (MonadPerspectives o)
runSterileTransaction a =
  getUserIdentifier
  >>= lift <<< createTransaction (ENR $ EnumeratedRoleType sysUser)
  >>= lift <<< new
  >>= runReaderT a

runEmbeddedTransaction :: forall o.
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runEmbeddedTransaction authoringRole a = do
  t <- transactionFlag
  flagEmpty <- lift $ tryRead t
  case flagEmpty of
    Nothing -> do
      -- 1. Raise the flag
      _ <- lift $ put 0 t
      log "Starting embedded transaction."
      -- 2. Run the transaction (this will lower and raise the flag again).
      result <- runMonadPerspectivesTransaction authoringRole a
      -- 2. Lower it again.
      log "Ending embedded transaction."
      _ <- lift $ take t
      pure result
    _ -> throwError (error "runEmbeddedTransaction is not run inside another transaction.")

-- | Evaluates state transitions. Executes automatic actions. Notifies users. Collects deltas in a new transaction.
-- | If any are collected, recursively calls itself.
-- | Notice that context- and role instances are still not actually removed from cache, nor from Couchdb, while
-- | this function runs.
-- | Terminates on the fixpoint where no more states need to be evaluated.
-- | Invariant: when called, the rolesToExit are already part of the untouchableRoles and the ContextRemovals
-- | are part of untouchableContexts, for the transaction in state.
runEntryAndExitActions :: Transaction -> MonadPerspectivesTransaction Transaction
runEntryAndExitActions previousTransaction@(Transaction{createdContexts, createdRoles, scheduledAssignments, rolesToExit}) = do
  log "==========RUNNING ON ENTRY AND ON EXIT ACTIONS============"
  -- Install a fresh transaction, keeping the untouchableRoles and untouchableContexts.
  void $ AA.modify cloneEmptyTransaction
  -- Enter the rootState of new contexts.
  for_ createdContexts
    \ctxt -> do
      states <- lift (ctxt ##= contextType >=> liftToInstanceLevel contextRootStates)
      -- Provide a new frame for the current context variable binding.
      oldFrame <- lift pushFrame
      lift $ addBinding "currentcontext" [unwrap ctxt]
      -- Error boundary.
      catchError (for_ states (enteringState ctxt))
        \e -> logPerspectivesError $ Custom ("Cannot enter state, because " <> show e)
      lift $ restoreFrame oldFrame
  -- Enter the rootState of roles that are created.
  for_ createdRoles
    \rid -> do
      ctxt <- lift (rid ##>> context)
      states <- lift (rid ##= roleType >=> liftToInstanceLevel roleRootStates)
      oldFrame <- lift pushFrame
      lift $ addBinding "currentcontext" [unwrap ctxt]
      -- Error boundary.
      catchError (for_ states (enteringRoleState rid))
        \e -> logPerspectivesError $ Custom ("Cannot enter role state, because " <> show e)
      lift $ restoreFrame oldFrame
  -- Exit the rootState of roles that are scheduled to be removed, unless we did so before.
  log ("Roles to exit: " <> show rolesToExit)
  rolesThatHaveNotExited <- lift $ filterA (\rid -> rid ##>> exists' getActiveRoleStates) rolesToExit
  log ("Roles that have not exited: " <> show rolesThatHaveNotExited)
  for_ rolesThatHaveNotExited
    \rid -> do
      ctxt <- lift (rid ##>> context)
      states <- lift (rid ##= roleType >=> liftToInstanceLevel roleRootStates)
      if null states
        then pure unit
        else do
          oldFrame <- lift pushFrame
          lift $ addBinding "currentcontext" [unwrap ctxt]
          stateEvaluationAndQueryUpdatesForRole rid
          -- Error boundary.
          catchError (for_ states (exitingRoleState rid))
            \e -> logPerspectivesError $ Custom ("Cannot exit role state, because " <> show e)
          lift $ restoreFrame oldFrame
  -- Exit the rootState of contexts that scheduled to be removed, unless we did so before.
  contextsThatHaveNotExited <- lift $ filterA (\sa -> case sa of
      ContextRemoval ctxt _ -> ctxt ##>> exists' getActiveStates
      _ -> pure false)
    scheduledAssignments
  for_ contextsThatHaveNotExited (unsafePartial exitContext)
    -- First append the collected rolesToExit and ContextRemovals to the untouchableRoles and untouchableContexts
    -- to preserve the invariant.
  AA.modify (\t -> over Transaction (\tr -> tr
    { untouchableRoles = tr.untouchableRoles <> tr.rolesToExit
    , untouchableContexts = tr.untouchableContexts <> (contextsToBeRemoved tr.scheduledAssignments)
    })
    t)
  nt <- AA.get
  if isEmptyTransaction nt
    then pure previousTransaction
    else pure <<< (<>) previousTransaction =<< runEntryAndExitActions nt
  where
    exitContext :: Partial => ScheduledAssignment -> MonadPerspectivesTransaction Unit
    exitContext (ContextRemoval ctxt authorizedRole) = do
      stateEvaluationAndQueryUpdatesForContext ctxt authorizedRole
      states <- lift (ctxt ##= contextType >=> liftToInstanceLevel contextRootStates)
      if null states
        then pure unit
        else do
          -- Provide a new frame for the current context variable binding.
          oldFrame <- lift pushFrame
          lift $ addBinding "currentcontext" [unwrap ctxt]
          -- Error boundary.
          catchError (for_ states (exitingState ctxt))
            \e -> logPerspectivesError $ Custom ("Cannot exit state, because " <> show e)
          lift $ restoreFrame oldFrame

      

-----------------------------------------------------------
-- LOADMODELIFMISSING
-----------------------------------------------------------
-- | Retrieves from the repository the model, if necessary.
-- TODO. This function relies on a repository URL in PerspectivesState. That is a stub.
loadModelIfMissing :: String -> MonadPerspectivesTransaction Unit
loadModelIfMissing modelName = do
  mDomeinFile <- lift $ tryRetrieveDomeinFile modelName
  if isNothing mDomeinFile
    then do
      repositoryUrl <- lift publicRepository
      addModelToLocalStore' (repositoryUrl <> modelName) true
      -- Now create a binding of the model description in sys:PerspectivesSystem$ModelsInUse.
      (lift $ try $ getDomeinFile (DomeinFileId modelName)) >>=
        handleDomeinFileError "loadModelIfMissing"
        \(DomeinFile{modelDescription}) -> do
          mySys <- lift $ getMySystem
          case modelDescription of
            Nothing -> pure unit
            Just (PerspectRol{_id}) -> void $ createAndAddRoleInstance
              modelsInUseRole
              mySys
              (RolSerialization{id: Nothing, properties: PropertySerialization empty, binding: Just $ unwrap _id})
    else pure unit

-----------------------------------------------------------
-- EXECUTEEFFECT
-----------------------------------------------------------
executeEffect :: String -> String -> Array (Array String) -> MonadPerspectivesTransaction Unit
executeEffect functionName origin values = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
    0 -> (unsafeCoerce f :: String -> MPT Unit) origin
    1 -> (unsafeCoerce f :: (Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      origin
    2 -> (unsafeCoerce f :: (Array String -> Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      (unsafePartial (unsafeIndex values 1))
      origin
    3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      (unsafePartial (unsafeIndex values 1))
      (unsafePartial (unsafeIndex values 2))
      origin
    4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      (unsafePartial (unsafeIndex values 1))
      (unsafePartial (unsafeIndex values 2))
      (unsafePartial (unsafeIndex values 3))
      origin
    _ -> throwError (error "Too many arguments for external core module: maximum is 4")
