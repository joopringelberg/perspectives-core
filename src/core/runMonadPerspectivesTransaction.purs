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

import Control.Monad.AvarMonadAsk (get, gets, modify) as AA
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Reader (lift, runReaderT)
import Data.Array (difference, filter, filterA, index, intercalate, length, null, sort, unsafeIndex)
import Data.Foldable (for_)
import Data.Map as MAP
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Unfoldable (replicate)
import Effect.Aff.AVar (new, put, take, tryRead)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CollectAffectedContexts (reEvaluatePublicFillerChanges)
import Perspectives.ContextStateCompiler (enteringState, evaluateContextState, exitingState)
import Perspectives.CoreTypes (MPT, MonadPerspectives, MonadPerspectivesTransaction, liftToInstanceLevel, (##=), (##>), (##>>))
import Perspectives.Deltas (TransactionPerUser, distributeTransaction)
import Perspectives.DependencyTracking.Dependency (lookupActiveSupportedEffect)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunction, lookupHiddenFunctionNArgs)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (hasLocalName)
import Perspectives.Instances.Combinators (exists')
import Perspectives.Instances.Me (getMyType)
import Perspectives.Instances.ObjectGetters (Filler_(..), context, contextType, filler2filledFromDatabase_, getActiveRoleStates, getActiveStates, roleType, roleType_)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (tryRemoveEntiteit)
import Perspectives.PerspectivesState (addBinding, clearPublicRolesJustLoaded, decreaseTransactionLevel, getPublicRolesJustLoaded, increaseTransactionLevel, nextTransactionNumber, pushFrame, restoreFrame, transactionFlag, transactionLevel)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Query.UnsafeCompiler (context2propertyValue, getCalculatedRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance, Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RoleStateCompiler (enteringRoleState, evaluateRoleState, exitingRoleState)
import Perspectives.SaveUserData (changeRoleBinding, removeContextInstance, removeRoleInstance, stateEvaluationAndQueryUpdatesForContext, queryUpdatesForRole)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..), StateEvaluation(..), contextsToBeRemoved)
import Perspectives.Sync.HandleTransaction (executeDeltas, expandDeltas)
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.Transaction (Transaction(..), TransactionDestination(..), createTransaction)
import Perspectives.Types.ObjectGetters (contextRootStates, publicUrl_, roleRootStates)
import Prelude (Unit, bind, discard, flip, join, not, pure, show, unit, void, ($), (*>), (<$>), (<<<), (<>), (>), (>=>), (>>=), (||))
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- RUN MONADPERSPECTIVESTRANSACTION
-----------------------------------------------------------
-- | Runs an update function (a function in MonadPerspectivesTransaction that produces deltas),
-- | runs actions as long as they are triggered, sends deltas to other participants and re-runs active queries
-- | Create a Transaction with the role in TheWorld that fills SocialEnvironment$Me (which in turn fills PerspectivesSystem$User).
-- | The authoringRole winds up as 'subject' in deltas.
runMonadPerspectivesTransaction :: forall o.
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runMonadPerspectivesTransaction authoringRole a = runMonadPerspectivesTransaction' shareWithPeers authoringRole a

shareWithPeers :: Boolean
shareWithPeers = true

doNotShareWithPeers :: Boolean
doNotShareWithPeers = false

-- | Create a Transaction with the role in TheWorld that fills SocialEnvironment$Me (which in turn fills PerspectivesSystem$User).
runMonadPerspectivesTransaction' :: forall o.
  Boolean ->
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runMonadPerspectivesTransaction' share authoringRole a = (lift $ createTransaction authoringRole) >>= lift <<< new >>= runReaderT whenFlagIsDown
  where
    -- | Wait until the TransactionFlag can be taken down, then run the action; raise it again.
    whenFlagIsDown :: MonadPerspectivesTransaction o
    whenFlagIsDown = do 
      -- 0. Only execute the transaction when we can take the flag down.
      t <- lift $ transactionFlag
      lift $ lift $ void $ take t
      transactionNumber <- lift $ nextTransactionNumber
      padding <- lift transactionLevel
      log (padding <> "Starting " <> (if share then "" else "non-") <> "sharing transaction " <> show transactionNumber)
      catchError 
        do 
          -- Execute the value that accumulates Deltas in a Transaction.
          r <- a >>= phase1 share authoringRole
          -- 5. Raise the flag
          log (padding <> "Ending transaction " <> show transactionNumber)
          _ <- lift $ lift $ put true t
          pure r
        \e -> do 
          -- 5. Raise the flag
          log (padding <> "Ending transaction " <> show transactionNumber)
          _ <- lift $ lift $ put true t
          throwError e

-- | In phase1 we handle:
-- | createdContexts, createdRoles, rolesToExit and ScheduledAssignments that are a ContextRemoval, a RoleUnbinding or a ExecuteDestructiveEffect.
-- | The first three are cleared out; from the last we remove the ExecuteDestructiveEffect and RoleUnbinding items.
phase1 :: forall o. Boolean -> RoleType -> o -> MonadPerspectivesTransaction o
phase1 share authoringRole r = do
  padding <- lift transactionLevel
  log $ padding <> "Entering phase1."
  -- Run monotonic actions, after
  --  * adding all ContextRemovals to the untouchableContexts and
  --  * adding rolesToExit to the untouchableRoles.
  AA.modify (\t -> over Transaction (\tr -> tr
    { untouchableRoles = tr.untouchableRoles <> tr.rolesToExit
    , untouchableContexts = tr.untouchableContexts <> (contextsToBeRemoved tr.scheduledAssignments)
    })
    t)
  (Transaction{createdContexts, createdRoles, rolesToExit, scheduledAssignments}) <- AA.get
  -- AFWIJKING: we initialiseren deze members direct.
  -- Even though we use ContextRemovals here, we cannot remove them from the Transaction as we need them in 
  -- step2 to finally remove the contexts. Keeping them in the Transaction is not optimal as we have to 
  -- filter them on every recursive call to phase1, to see whether they have exited or not.
  AA.modify (\t -> over Transaction (\tr -> tr {createdContexts = [], createdRoles = [], rolesToExit = []}) t)
  -- Enter the rootState of new contexts.
  void $ for createdContexts
    \ctxt -> do
      states <- lift (ctxt ##= contextType >=> liftToInstanceLevel contextRootStates)
      -- Provide a new frame for the current context variable binding.
      oldFrame <- lift pushFrame
      lift $ addBinding "currentcontext" [unwrap ctxt]
      -- Error boundary.
      catchError (void $ runSharing share authoringRole (for states (enteringState ctxt)))
        \e -> logPerspectivesError $ Custom ("Cannot enter context state for " <> show ctxt <>  ", because " <> show e)
      lift $ restoreFrame oldFrame
  -- Enter the rootState of roles that are created.
  void $ for createdRoles
    \rid -> do
      ctxt <- lift (rid ##>> context)
      states <- lift (rid ##= roleType >=> liftToInstanceLevel roleRootStates)
      oldFrame <- lift pushFrame
      lift $ addBinding "currentcontext" [unwrap ctxt]
      -- Error boundary.
      catchError (void $ runSharing share authoringRole (for states (enteringRoleState rid)))
        \e -> logPerspectivesError $ Custom ("Cannot enter role state, because " <> show e)
      lift $ restoreFrame oldFrame
  -- Exit the rootState of roles that are scheduled to be removed, unless we did so before.
  rolesThatHaveNotExited <- lift $ filterA (\rid -> rid ##>> exists' getActiveRoleStates) rolesToExit
  if not $ null rolesThatHaveNotExited
    then log (padding <> "Roles that have not exited: " <> show rolesThatHaveNotExited)
    else pure unit
  if null rolesThatHaveNotExited
    then pure unit
    else void $ runSharing share authoringRole (for rolesThatHaveNotExited
      \rid -> do
        ctxt <- lift (rid ##>> context)
        states <- lift (rid ##= roleType >=> liftToInstanceLevel roleRootStates)
        if null states
          then pure unit
          else do
            oldFrame <- lift pushFrame
            lift $ addBinding "currentcontext" [unwrap ctxt]
            queryUpdatesForRole rid
            -- Error boundary.
            catchError (void $ for states (exitingRoleState rid))
              \e -> do 
                logPerspectivesError $ Custom ("Cannot exit role state for " <> show rid <> ", because " <> show e)
                lift $ restoreFrame oldFrame
                throwError e
            lift $ restoreFrame oldFrame)
  -- Exit the rootState of contexts that scheduled to be removed, unless we did so before.
  contextsThatHaveNotExited <- lift $ filterA (\sa -> case sa of
      ContextRemoval ctxt _ -> ctxt ##>> exists' getActiveStates
      _ -> pure false)
    scheduledAssignments
  if null contextsThatHaveNotExited
    then pure unit
    else void $ runSharing share authoringRole (for contextsThatHaveNotExited (unsafePartial exitContext))
    -- First append the collected rolesToExit and ContextRemovals to the untouchableRoles and untouchableContexts
    -- to preserve the invariant.
  AA.modify (\t -> over Transaction (\tr -> tr
    { untouchableRoles = tr.untouchableRoles <> tr.rolesToExit
    , untouchableContexts = tr.untouchableContexts <> (contextsToBeRemoved tr.scheduledAssignments)
    -- Remove the ScheduledAssignments in contextsThatHaveNotExited from scheduledAssignments
    })
    t)
  recur <- AA.gets( \(Transaction tr) -> 
      -- There may be new ContextRemovals scheduled; if so, recur must be true.
       (not $ null (filter isContextRemoval (difference tr.scheduledAssignments scheduledAssignments)))
    || (not $ null tr.rolesToExit)
    || (not $ null tr.createdContexts)
    || (not $ null tr.createdRoles) )
  if recur 
    then phase1 share authoringRole r
    else do 
      -- Unbind roles and execute destructive effects.
      void $ for scheduledAssignments case _ of
        RoleUnbinding filled mNewFiller msignedDelta -> log (padding <> "Remove filler of " <> unwrap filled) *> changeRoleBinding filled mNewFiller
        ExecuteDestructiveEffect functionName origin values -> log (padding <> "DestructiveEffect: " <> functionName) *> executeEffect functionName origin values
        _ -> pure unit
      -- We never need these again, so remove them from the Transaction.
      AA.modify \(Transaction tr) -> Transaction $ tr {scheduledAssignments = filter criterium tr.scheduledAssignments}
      -- Invariant: as we call phase2, there are no rolesToExit, no createdContexts, no createdRoles.
      -- Nor will there be RoleUnbinding or ExecuteDestructiveEffect items in scheduledAssignments.
      phase2 share authoringRole r
  where
    isContextRemoval :: ScheduledAssignment -> Boolean
    isContextRemoval (ContextRemoval _ _) = true
    isContextRemoval _ = false

    criterium :: ScheduledAssignment -> Boolean
    criterium (RoleUnbinding _ _ _ ) = false
    criterium (ExecuteDestructiveEffect _ _ _) = false
    criterium _ = true

phase2 :: forall o. Boolean -> RoleType -> o -> MonadPerspectivesTransaction o
phase2 share authoringRole r = do
  padding <- lift transactionLevel
  log $ padding <>  "Entering phase2."
  runSharing share authoringRole recursivelyEvaluateStates
  Transaction {createdContexts, createdRoles, rolesToExit, scheduledAssignments, modelsToBeRemoved} <- AA.get
  -- Is there a reason to run phase1 again?
  -- Only if there are new createdContexts, createdRoles, rolesToExit, 
  -- or new scheduledAssignments that are a ContextRemoval, a RoleUnbinding or a ExecuteDestructiveEffect.
  reRunPhase1 <- AA.gets( \(Transaction tr) -> 
       (not $ null (filter criterium (tr.scheduledAssignments `difference` scheduledAssignments)))
    || (not $ null (tr.rolesToExit `difference` rolesToExit))
    || (not $ null (tr.createdContexts `difference` createdContexts))
    || (not $ null (tr.createdRoles `difference` createdRoles)) )
  if reRunPhase1
    then (log $ padding <> "Rerun phase1") *> phase1 share authoringRole r
    else do
      -- Invariant: there are no rolesToExit, no createdContexts, no createdRoles. 
      -- Nor will there be RoleUnbinding or ExecuteDestructiveEffect items in scheduledAssignments.
      -- This is because phase1 has no items in these members and apparantly 
      -- there have been no new items added during state evaluation.
      -- Notice that there may be ContextRemoval and RoleRemoval items, though!
      ft <- AA.get
      (publicRoleTransactions :: TransactionPerUser) <- if share 
        then lift $ distributeTransaction ft 
        else pure MAP.empty

      -- Collect all deltas in order, add the public resource schemes and remove doubles. Then execute.
      void $ forWithIndex publicRoleTransactions
        \destination publicRoleTransaction -> case destination of 
          (PublicDestination userId) -> do
            userType <- lift $ roleType_ userId
            mUrl <- lift $ publicUrl_ userType
            case mUrl of
              Nothing -> throwError (error $ "A user role type that is neither the system User nor a public role: " <> show userType <> " ('" <> show userId <> "')")
              Just (Q qfd) -> do 
                ctxt <- lift $ (userId ##>> context)
                urlComputer <- lift $ context2propertyValue qfd
                murl <- lift (ctxt ##> urlComputer)
                case murl of 
                  Just (Value url) -> do 
                    deltas <- expandDeltas publicRoleTransaction url
                  -- These deltas for a public role aren't sent anywhere but executed
                  -- right here. Notice that no changes to local state will result from executing such a transaction.
                  -- (except that public instances will be cached)
                  -- Run embedded, do not share.
                    lift $ runEmbeddedIfNecessary false authoringRole (executeDeltas deltas)
                  -- If the URL is not computed, we log this and do nothing. In this installation, there probably should not be a proxy for the public role anyway; but we don't have a way of knowing that on constructing the context.
                  Nothing -> log (padding <> "Cannot compute a URL to publish to for this user role type and instance: " <> show userType <> " ('" <> show userId <> "')")
              Just (S _ _) -> throwError (error ("Attempt to acces QueryFunctionDescription of the url of a public role before the expression has been compiled. This counts as a system programming error. User type = " <> (show userType)))
          Peer _ -> pure unit
      -- Remove the deltas; we don't want to execute them again.
      AA.modify (\t -> over Transaction (\tr -> tr {deltas = []}) t)
      -- Now finally remove contexts and roles.
      void $ for scheduledAssignments case _ of
        ContextRemoval ctxt authorizedRole -> lift (log (padding <> "Remove context " <> unwrap ctxt) *> removeContextInstance ctxt authorizedRole)
        RoleRemoval rid -> lift (log (padding <> "Remove role " <> unwrap rid) *> removeRoleInstance rid)
        _ -> pure unit
      -- we can now remove ContextRemovals and RoleRemovals from scheduledAssignments. As these can be the only items left in the collection of scheduledAssignments,
      -- we can simply reset it.
      -- We can also remove the untouchables; all resources listed in them have gone, now.
      AA.modify \t -> over Transaction (\tr -> tr {scheduledAssignments = [], untouchableContexts = [], untouchableRoles = []}) t
      Transaction {postponedStateEvaluations, correlationIdentifiers} <- AA.get
      if null postponedStateEvaluations
        then do 
          if not $ null modelsToBeRemoved
            then log (padding <> "Will remove these models: " <> show modelsToBeRemoved)
            else pure unit
          lift $ void $ for modelsToBeRemoved tryRemoveEntiteit
          if not $ null correlationIdentifiers
            then do
              log $ padding <> "==========RUNNING EFFECTS============"
              -- Sort from low to high, so we can never actualise a client side component after it has been removed.
              for_ (sort correlationIdentifiers) \corrId -> do
                mEffect <- pure $ lookupActiveSupportedEffect corrId
                case mEffect of
                  Nothing -> pure unit
                  (Just {runner}) -> do
                    -- logShow corrId
                    lift $ runner unit
              -- As this is the end of execution of this Transaction, we don't bother with removing the postponedStateEvaluations.
              pure r
            else pure r
        else do 
          log $ padding <> "Re-evaluating state evaluations that depend on a removed resource: " <> (show postponedStateEvaluations)
          if null postponedStateEvaluations
            then pure unit
            else (runSharing share authoringRole (evaluateStates postponedStateEvaluations))
          AA.modify \t -> over Transaction (\tr -> tr {postponedStateEvaluations = [], modelsToBeRemoved = []}) t
          phase2 share authoringRole r

  where
    criterium :: ScheduledAssignment -> Boolean
    criterium (ContextRemoval _ _) = true
    criterium (RoleUnbinding _ _ _ ) = true
    criterium (ExecuteDestructiveEffect _ _ _) = true
    criterium _ = false

-- | Actions performed on behalf of the own user in the course of state transitions will result in deltas that
-- | must be synchronised with peers, even when the transaction in which this happens is received from a peer
-- | and thus is non-sharing.
runSharing :: forall o. Boolean -> RoleType -> MonadPerspectivesTransaction o -> MonadPerspectivesTransaction o
runSharing share authoringRole t = if share
  -- We already share, just execute the action.
  then t
  -- Run from within a non-sharing transaction. Run an embedded, sharing transaction.
  else do
    padding <- lift transactionLevel
    log $ padding <> "run sharing transaction from nonsharing transaction."
    r <- lift $ runEmbeddedTransaction shareWithPeers authoringRole t
    log $ padding <> "returning to nonsharing transaction from sharing transaction."
    pure r

-- Add to each context or role instance the user role type and the RootState type.
computeStateEvaluations :: InvertedQueryResult -> MonadPerspectives (Array StateEvaluation)
computeStateEvaluations (ContextStateQuery contextInstances) = join <$> do
  activeInstances <- filterA (\rid -> rid ##>> exists' getActiveStates) contextInstances
  for activeInstances \cid -> do
    -- States includes root states of Aspects.
    states <- cid ##= contextType >=> liftToInstanceLevel contextRootStates
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
            otherwise -> pure $ (\state -> ContextStateEvaluation state cid) <$> states
        else pure $ (\state -> ContextStateEvaluation state cid) <$> states
      Just _ -> pure $ (\state -> ContextStateEvaluation state cid ) <$> states
      
computeStateEvaluations (RoleStateQuery roleInstances) = join <$> do
  activeInstances <- filterA (\rid -> rid ##>> exists' getActiveRoleStates) roleInstances
  -- If the roleInstance has no recorded states, this means it has been marked for removal
  -- and has exited all its states. We should not do that again!
  for activeInstances \rid -> do
    (mmyType :: Maybe RoleType) <- rid ##> context >=> getMyType
      -- Neem aspecten hier ook mee!
    states <- rid ##= roleType >=> liftToInstanceLevel roleRootStates
    case mmyType of
      Nothing -> pure []
      Just (CR myType) -> if isGuestRole myType
        then do
          (mmguest :: Maybe RoleInstance) <- rid ##> context >=> getCalculatedRoleInstances myType
          case mmguest of
            Nothing -> pure []
            otherwise -> pure $ (\state -> RoleStateEvaluation state rid) <$> states
        else pure $ (\state -> RoleStateEvaluation state rid) <$> states
      Just _ -> pure $ (\state -> RoleStateEvaluation state rid) <$> states

isGuestRole :: CalculatedRoleType -> Boolean
isGuestRole (CalculatedRoleType cr) = cr `hasLocalName` "Guest"


exitContext :: Partial => ScheduledAssignment -> MonadPerspectivesTransaction Unit
exitContext (ContextRemoval ctxt authorizedRole) = do
  states <- lift (ctxt ##= contextType >=> liftToInstanceLevel contextRootStates)
  if null states
    then pure unit
    else do
      -- Provide a new frame for the current context variable binding.
      oldFrame <- lift pushFrame
      lift $ addBinding "currentcontext" [unwrap ctxt]
      -- Error boundary.
      catchError (void $ for states (exitingState ctxt))
        \e -> logPerspectivesError $ Custom ("Cannot exit state, because " <> show e)
      lift $ restoreFrame oldFrame
  -- Adds a RemoveExternalRoleInstance to the current transaction.
  -- Severes the link between the roles and their fillers.
  stateEvaluationAndQueryUpdatesForContext ctxt authorizedRole

  
recursivelyEvaluateStates :: MonadPerspectivesTransaction Unit
recursivelyEvaluateStates = do
  padding <- lift transactionLevel
  log $ padding <> "Evaluate states"
  Transaction {invertedQueryResults} <- AA.get
  AA.modify \t -> over Transaction (\tr -> tr {invertedQueryResults = []}) t
  (stateEvaluations :: Array StateEvaluation) <- lift $ join <$> traverse computeStateEvaluations invertedQueryResults
  if null stateEvaluations
    then pure unit
    else log (padding <> "==========RUNNING " <> (show $ length stateEvaluations) <> " OTHER STATE EVALUTIONS============")
  evaluateStates stateEvaluations
  -- We now may have new inverted query results (and other changes). If so, we re-run phase2.
  Transaction {invertedQueryResults:newResults} <- AA.get
  if null newResults
    then pure unit
    else recursivelyEvaluateStates

evaluateStates :: Array StateEvaluation -> MonadPerspectivesTransaction Unit 
evaluateStates stateEvaluations' =
  void $ for stateEvaluations' \s -> case s of
    ContextStateEvaluation stateId contextId -> do
      -- Provide a new frame for the current context variable binding.
      oldFrame <- lift pushFrame
      lift $ addBinding "currentcontext" [unwrap contextId]
      -- Error boundary.
      catchError (evaluateContextState contextId stateId)
        \e -> logPerspectivesError $ Custom ("Cannot evaluate context state, because " <> show e)
      lift $ restoreFrame oldFrame
    RoleStateEvaluation stateId roleId -> do
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

-- | Run and discard the transaction.
runSterileTransaction :: forall o. MonadPerspectivesTransaction o -> (MonadPerspectives o)
runSterileTransaction a =
  (lift $ createTransaction (ENR $ EnumeratedRoleType sysUser))
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
  Boolean ->
  RoleType ->
  MonadPerspectivesTransaction o
  -> (MonadPerspectives o)
runEmbeddedTransaction share authoringRole a = do
  t <- transactionFlag
  flagIsDown <- isNothing <$> (lift $ tryRead t)
  if flagIsDown
    then do
      -- Because the transactionFlag AVar is empty (== the flag is down), we know a transaction is running.
      -- 1. Raise the flag.
      increaseTransactionLevel
      padding <- transactionLevel
      _ <- lift $ put true t
      log $ padding <>  "Starting embedded " <> (if share then "" else "non-") <> "sharing transaction."
      catchError 
        do
          -- 2. Run the transaction (this will lower and raise the flag again, setting it to 1).
          -- runMonadPerspectivesTransaction has an internal Error Boundary that guarantees the flag is handled.
          -- In other words, it is guaranteed to be up again when it is finished. But it may throw!
          result <- runMonadPerspectivesTransaction' share authoringRole a
          -- 2. Lower it again.
          log $ padding <> "Ending embedded transaction."
          decreaseTransactionLevel
          _ <- lift $ take t
          pure result
        \e -> do
          log (padding <> "Ending embedded transaction in failure: " <> show e) 
          decreaseTransactionLevel
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
      -- 1. Raise the flag.
      _ <- lift $ put true t
      increaseTransactionLevel
      padding <- transactionLevel
      log $ padding <> "Starting embedded " <> (if share then "" else "non-") <> "sharing transaction" <> " because it was necessary."
      -- 2. Run the transaction (this is guaranteed to lower and raise the flag again because of an internal error boundary, setting it to 1 - but it may throw again!).
      catchError 
        do
          result <- runMonadPerspectivesTransaction' share authoringRole a
          -- 2. Lower it again.
          log $ padding <> "Ending transaction that needed to be embedded."
          decreaseTransactionLevel
          _ <- lift $ take t
          pure result
        \e -> do
          log $ padding <> ("Ending transaction that needed to be embedded in failure. " <> show e)
          decreaseTransactionLevel
          _ <- lift $ take t
          throwError e

    else runMonadPerspectivesTransaction' share authoringRole a

-----------------------------------------------------------
-- EXECUTEEFFECT
-----------------------------------------------------------
executeEffect :: String -> String -> Array (Array String) -> MonadPerspectivesTransaction Unit
executeEffect functionName origin values = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (nrOfParameters :: Int) <- pure $ unsafePartial (fromJust $ lookupHiddenFunctionNArgs functionName)
  -- Notice that the number of parameters given ignores the default argument (context or role) that the function is applied to anyway.
  -- If we do have an extra argument value, supply it as the last argument instead of r.
  (lastArgument :: String) <- case index values nrOfParameters of
    Nothing -> pure origin
    Just v -> pure (unsafePartial (unsafeIndex v 0))
  case nrOfParameters of
    0 -> (unsafeCoerce f :: String -> MPT Unit) lastArgument
    1 -> (unsafeCoerce f :: (Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      lastArgument
    2 -> (unsafeCoerce f :: (Array String -> Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      (unsafePartial (unsafeIndex values 1))
      lastArgument
    3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      (unsafePartial (unsafeIndex values 1))
      (unsafePartial (unsafeIndex values 2))
      lastArgument
    4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      (unsafePartial (unsafeIndex values 1))
      (unsafePartial (unsafeIndex values 2))
      (unsafePartial (unsafeIndex values 3))
      lastArgument
    5 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      (unsafePartial (unsafeIndex values 1))
      (unsafePartial (unsafeIndex values 2))
      (unsafePartial (unsafeIndex values 3))
      (unsafePartial (unsafeIndex values 4))
      lastArgument
    6 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> Array String -> String -> MPT Unit))
      (unsafePartial (unsafeIndex values 0))
      (unsafePartial (unsafeIndex values 1))
      (unsafePartial (unsafeIndex values 2))
      (unsafePartial (unsafeIndex values 3))
      (unsafePartial (unsafeIndex values 4))
      (unsafePartial (unsafeIndex values 5))
      lastArgument
    _ -> throwError (error "Too many arguments for external core module: maximum is 6")

-----------------------------------------------------------
-- DETECT STATE TRANSITIONS TRIGGERED BY PUBLIC RESOURCES
-----------------------------------------------------------
detectPublicStateChanges :: MonadPerspectives Unit
detectPublicStateChanges = do
  publicRoles <- getPublicRolesJustLoaded
  if length publicRoles > 0 
    then do
      clearPublicRolesJustLoaded
      runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser)
        (void $ for publicRoles f)
      detectPublicStateChanges
    else pure unit
  
  where 
    f :: RoleInstance -> MonadPerspectivesTransaction Unit 
    f filler = do 
      (filledCandidates :: Array RoleInstance) <- lift $ filler2filledFromDatabase_ (Filler_ filler)
      void $ for filledCandidates (flip reEvaluatePublicFillerChanges filler)

pad :: Int -> String
pad n = intercalate "" $ replicate n " "