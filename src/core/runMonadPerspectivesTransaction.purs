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
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Reader (lift, runReaderT)
import Data.Array (filterA, head, length, null, reverse, sort, unsafeIndex)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, traverse)
import Effect.Aff.AVar (new, put, take, tryRead)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextStateCompiler (enteringState, evaluateContextState, exitingState)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, StateEvaluation(..), MPT, liftToInstanceLevel, (##=), (##>), (##>>))
import Perspectives.Deltas (distributeTransaction)
import Perspectives.DependencyTracking.Dependency (lookupActiveSupportedEffect)
import Perspectives.DomeinCache (tryRetrieveDomeinFile)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (addModelToLocalStore, isInitialLoad)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (hasLocalName)
import Perspectives.Instances.Combinators (exists')
import Perspectives.Instances.ObjectGetters (context, contextType, getActiveRoleStates, getActiveStates, roleType, roleType_)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (tryRemoveEntiteit)
import Perspectives.PerspectivesState (addBinding, pushFrame, restoreFrame, transactionFlag)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Query.UnsafeCompiler (context2propertyValue, getCalculatedRoleInstances, getMyType)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), DomeinFileId, EnumeratedRoleType(..), RoleType(..))
import Perspectives.RoleStateCompiler (enteringRoleState, evaluateRoleState, exitingRoleState)
import Perspectives.SaveUserData (changeRoleBinding, removeContextInstance, removeRoleInstance, stateEvaluationAndQueryUpdatesForContext, stateEvaluationAndQueryUpdatesForRole)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..), contextsToBeRemoved)
import Perspectives.Sync.HandleTransaction (executeTransactionForPublicRole)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..), cloneEmptyTransaction, createTransaction, isEmptyTransaction)
import Perspectives.Types.ObjectGetters (contextRootStates, publicUrl_, roleRootStates)
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
runMonadPerspectivesTransaction authoringRole a = runMonadPerspectivesTransaction' shareWithPeers authoringRole a

shareWithPeers :: Boolean
shareWithPeers = true

doNotShareWithPeers :: Boolean
doNotShareWithPeers = false

runMonadPerspectivesTransaction' :: forall o.
  Boolean ->
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runMonadPerspectivesTransaction' share authoringRole a = getUserIdentifier >>= lift <<< createTransaction authoringRole >>= lift <<< new >>= runReaderT whenFlagIsDown
  where
    -- | Wait until the TransactionFlag can be taken down, then run the action; raise it again.
    whenFlagIsDown :: MonadPerspectivesTransaction o
    whenFlagIsDown = do 
      -- 0. Only execute the transaction when we can take the flag down.
      t <- lift $ transactionFlag
      transactionNumber <- lift $ lift $ take t
      log ("Starting transaction " <> show transactionNumber)
      catchError 
        do 
          r <- run
          -- 5. Raise the flag
          log ("Ending transaction " <> show transactionNumber)
          _ <- lift $ lift $ put (transactionNumber + 1) t
          pure r
        \e -> do 
          -- 5. Raise the flag
          log ("Ending transaction " <> show transactionNumber)
          _ <- lift $ lift $ put (transactionNumber + 1) t
          throwError e

    run :: MonadPerspectivesTransaction o
    run = do
      -- 1. Execute the value that accumulates Deltas in a Transaction.
      r <- a

      -- 2. Now run all actions.
      ft@(Transaction{correlationIdentifiers, scheduledAssignments}) <- AA.get >>= runAllAutomaticActions

      -- 3. Send deltas to other participants, save changed domeinfiles.
      publicRoleTransactions <- if share 
        then lift $ distributeTransaction ft 
        else pure empty

      -- This may result in transactions for public roles. We don't send them anywhere but execute them
      -- right here. Notice that no changes to local state will result from executing such a transaction.
      forWithIndex_ publicRoleTransactions
        \userId publicRoleTransaction -> do
          userType <- lift $ roleType_ (RoleInstance userId)
          mUrl <- lift $ publicUrl_ userType
          case mUrl of
            Nothing -> throwError (error $ "sendTransactie finds a user role type that is neither the system User nor a public role: " <> show userType <> " ('" <> userId <> "')")
            Just (Q qfd) -> do 
              ctxt <- lift $ ((RoleInstance userId) ##>> context)
              urlComputer <- lift $ context2propertyValue qfd
              (Value url) <- lift (ctxt ##>> urlComputer)
              -- Run embedded, do not share.
              lift $ runEmbeddedIfNecessary false authoringRole (executeTransactionForPublicRole publicRoleTransaction url)
            Just (S _) -> throwError (error ("Attempt to acces QueryFunctionDescription of the url of a public role before the expression has been compiled. This counts as a system programming error. User type = " <> (show userType)))

      -- Now finally remove contexts and roles.
      for_ (reverse scheduledAssignments) case _ of
        ContextRemoval ctxt authorizedRole -> lift (log ("Remove context " <> unwrap ctxt) *> removeContextInstance ctxt authorizedRole)
        RoleRemoval rid -> lift (log ("Remove role " <> unwrap rid) *> removeRoleInstance rid)
        -- TODO: moeten we msignedDelta niet meegeven?
        _ -> pure unit

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
        -- NOTICE that we do not remove contexts and roles yet. Distributing deltas for public proxies requires access to removed user roles and their contexts!
        RoleUnbinding filled mNewFiller msignedDelta -> log ("Remove filler of " <> unwrap filled) *> changeRoleBinding filled mNewFiller
        ExecuteDestructiveEffect functionName origin values -> log ("DestructiveEffect: " <> functionName) *> executeEffect functionName origin values
        _ -> pure unit
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

-- | Run a transaction even though another was already running.
-- | The transaction flag embedded in PerspectivesState enables us to run just one transaction at a time.
-- | We use this to execute transactions of various peers (including the own user) one by one,
-- | not mingling the effects.
-- | However, we have occasions where we want to run a transaction irrespective of whether another was running.
-- | Obviously, we should only do this when we can reason that no harm will be done.
-- | This is the case, for example, when we want to parse an ARC file, or when we want to add another model
-- | to the installation.
runEmbeddedTransaction :: forall o.
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runEmbeddedTransaction authoringRole a = do
  t <- transactionFlag
  flagIsDown <- isNothing <$> (lift $ tryRead t)
  if flagIsDown
    then do
      -- Because the transactionFlag AVar is empty (== the flag is down), we know a transaction is running.
      -- 1. Raise the flag, put 0 in it (we don't count embedded transactions)
      _ <- lift $ put 0 t
      log "Starting embedded transaction."
      catchError 
        do
          -- 2. Run the transaction (this will lower and raise the flag again, setting it to 1).
          -- runMonadPerspectivesTransaction has an internal Error Boundary that guarantees the flag is handled.
          -- In other words, it is guaranteed to be up again when it is finished. But it may throw!
          result <- runMonadPerspectivesTransaction authoringRole a
          -- 2. Lower it again.
          log "Ending embedded transaction."
          _ <- lift $ take t
          pure result
        \e -> do
          log ("Ending embedded transaction in failure. " <> show e)
          _ <- lift $ take t
          throwError e
    else throwError (error "runEmbeddedTransaction is not run inside another transaction.")

-- | Runs an embedded transaction if a transaction is being run; just runs it otherwise.
-- | Has an internal error boundary that guarantees that the flag is restored.
runEmbeddedIfNecessary :: forall o.
  Boolean ->
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runEmbeddedIfNecessary share authoringRole a = do
  t <- transactionFlag
  flagIsDown <- isNothing <$> (lift $ tryRead t)
  if flagIsDown
    then do
      -- Because the transactionFlag AVar is empty (== the flag is down), we know a transaction is running.
      -- 1. Raise the flag, put 0 in it (we don't count embedded transactions)
      _ <- lift $ put 0 t
      log "Starting embedded transaction."
      -- 2. Run the transaction (this is guaranteed to lower and raise the flag again because of an internal error boundary, setting it to 1 - but it may throw again!).
      catchError 
        do
          result <- runMonadPerspectivesTransaction' share authoringRole a
          -- 2. Lower it again.
          log "Ending embedded transaction."
          _ <- lift $ take t
          pure result
        \e -> do
          log ("Ending embedded transaction in failure. " <> show e)
          _ <- lift $ take t
          throwError e

    else runMonadPerspectivesTransaction' share authoringRole a


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
            \e -> do 
              logPerspectivesError $ Custom ("Cannot exit role state, because " <> show e)
              lift $ restoreFrame oldFrame
              throwError e
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
-- | Retrieves from the repository the model by its Model URI, if necessary.
loadModelIfMissing :: DomeinFileId -> MonadPerspectivesTransaction Unit
loadModelIfMissing dfId = do
  mDomeinFile <- lift $ tryRetrieveDomeinFile dfId
  if isNothing mDomeinFile
    then do
      addModelToLocalStore dfId isInitialLoad
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
