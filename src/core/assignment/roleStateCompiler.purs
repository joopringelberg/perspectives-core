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

module Perspectives.RoleStateCompiler where

-- | From the description of a state, compute a function that computes for an instance of its context type
-- | whether that state is in effect. If entering the state, run all entry effects for the current user.
-- | On exiting the state, run all exit effects.
-- | Notify the current user on entering and exiting if specified.
-- | Cache the computed function for the duration of the session.

import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, elemIndex, filterA, foldMap, index, null)
import Data.Array.NonEmpty (fromArray)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map, delete, lookup)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF, over, unwrap)
import Data.Traversable (for_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Aff (error, killFiber)
import Effect.Class.Console (log)
import Foreign.Object (singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.SentenceCompiler (CompiledSentence, compileRoleSentence)
import Perspectives.Assignment.SerialiseAsDeltas (serialiseRoleInstancesAndProperties)
import Perspectives.Assignment.StateCache (CompiledAutomaticAction, CompiledNotification, CompiledRoleState, CompiledStateDependentPerspective, cacheCompiledRoleState, retrieveCompiledRoleState)
import Perspectives.Assignment.Update (ConditionResult(..), isUndetermined, setActiveRoleState, setInActiveRoleState)
import Perspectives.CompileRoleAssignment (compileAssignmentFromRole, withAuthoringRole)
import Perspectives.CompileTimeFacets (addTimeFacets)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), MP, MonadPerspectives, MonadPerspectivesTransaction, Updater, WithAssumptions, liftToInstanceLevel, runMonadPerspectivesQuery, (##=), (##>>))
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter, not') as COMB
import Perspectives.Instances.Me (isMe)
import Perspectives.Instances.ObjectGetters (Filled_(..), Filler_(..), contextType, filledBy, getActiveRoleStates_)
import Perspectives.ModelDependencies (contextWithNotification, notificationMessage, notifications)
import Perspectives.Names (getMySystem)
import Perspectives.PerspectivesState (addBinding, getPerspectivesUser, pushFrame, restoreFrame, transactionLevel)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), domain, range)
import Perspectives.Query.UnsafeCompiler (getRoleInstances, role2context, role2propertyValue)
import Perspectives.Representation.Action (AutomaticAction(..))
import Perspectives.Representation.Class.PersistentType (getState)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..), perspectivesUser2RoleInstance)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.State (Notification(..), State(..), StateDependentPerspective(..), StateFulObject(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), RoleType, StateIdentifier)
import Perspectives.ScheduledAssignment (StateEvaluation(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (hasContextAspect, subStates_)

-- | This function has a Partial constraint because it only handles the AutomaticRoleAction case of AutomaticAction,
-- | and idem of Notification and StateDependentPerspective.
-- | The function can be safely applied (with `unsafePartial`), however.
compileState :: Partial => StateIdentifier -> MP CompiledRoleState
compileState stateId = do
    State {id, query, automaticOnEntry, automaticOnExit, notifyOnEntry, notifyOnExit, perspectivesOnEntry} <- getState stateId
    (automaticOnEntry' :: Map RoleType CompiledAutomaticAction) <- traverseWithIndex compileEffect (unwrap automaticOnEntry)
    (automaticOnExit' :: Map RoleType CompiledAutomaticAction) <- traverseWithIndex compileEffect (unwrap automaticOnExit)
    (notifyOnEntry' :: Map RoleType CompiledNotification) <- traverseWithIndex
      (\subject notification -> compileNotification notification subject)
      (unwrap notifyOnEntry)
    (notifyOnExit' :: Map RoleType CompiledNotification) <- traverseWithIndex
      (\subject notification -> compileNotification notification subject)
      (unwrap notifyOnExit)
    (perspectivesOnEntry' :: Map RoleType CompiledStateDependentPerspective) <- traverseWithIndex
      (\subject (RolePerspective{currentContextCalculation, properties, selfOnly, authorOnly, isSelfPerspective}) -> do 
        contextGetter <- role2context currentContextCalculation
        pure {contextGetter, properties, selfOnly, authorOnly, isSelfPerspective})
      (unwrap perspectivesOnEntry)

    -- We postpone compiling substates until they're asked for.
    (lhs :: (RoleInstance ~~> Value)) <- role2propertyValue $ unsafePartial case query of Q qfd -> qfd
    pure $ cacheCompiledRoleState stateId
      { query: lhs
      , objectGetter: Nothing
      , automaticOnEntry: automaticOnEntry'
      , automaticOnExit: automaticOnExit'
      , notifyOnEntry: notifyOnEntry'
      , notifyOnExit: notifyOnExit'
      , perspectivesOnEntry: perspectivesOnEntry'
      }
  where
    compileEffect :: Partial => RoleType -> AutomaticAction -> MP CompiledAutomaticAction
    compileEffect subject (RoleAction r@{effect, currentContextCalculation}) = do
      contextGetter <- role2context currentContextCalculation
      updater' <- compileAssignmentFromRole effect >>= pure <<< withAuthoringRole subject
      updater <- addTimeFacets updater' r subject stateId
      pure {updater, contextGetter}
    
    compileNotification :: Partial => Notification -> RoleType -> MP CompiledNotification
    compileNotification (RoleNotification r@{sentence, currentContextCalculation, domain}) subject = do
      compiledSentence <- compileRoleSentence domain sentence
      contextGetter <- role2context currentContextCalculation
      updater' <- pure $ notify compiledSentence contextGetter
      updater <- addTimeFacets updater' r subject stateId
      pure {updater, contextGetter}
    
-- | This function is applied without knowing whether state condition is valid.
-- | Put an error boundary around this function.
evaluateRoleState :: RoleInstance -> StateIdentifier -> MonadPerspectivesTransaction Unit
evaluateRoleState roleId stateId = do
  roleIsInState' <- conditionSatisfied roleId stateId
  case roleIsInState' of 
    Determined roleIsInState -> if roleIsInState
      then do
        roleWasInState <- lift $ isActive stateId roleId
        if roleWasInState
          then do
            padding <- lift transactionLevel
            log (padding <> "Already in role state " <> unwrap stateId <> ": " <> unwrap roleId)
            subStates <- lift $ subStates_ stateId
            for_ subStates (evaluateRoleState roleId)
          else enteringRoleState roleId stateId
      else do
        roleWasInState <- lift $ isActive stateId roleId
        if roleWasInState
          then exitingRoleState roleId stateId
          else pure unit
    Undetermined -> modify 
      (\t -> over Transaction
        (\tr -> tr { postponedStateEvaluations = cons (RoleStateEvaluation stateId roleId) tr.postponedStateEvaluations }) 
        t )


-- | This function is only called (and should only be called) on states whose condition is valid.
-- | On entering a state, we register that state with the role instance and trigger client query updates.
-- | We run all automatic actions and create notifications.
-- | Finally, we look for the substate whose query returns true and apply `enteringState` to it.
-- | Invariant: the state is not registered as active but its query evaluates to true.
-- |
-- | Put an error boundary around this function.
-- | Ensure that the current context is available in the environment before applying this function!
enteringRoleState :: RoleInstance  -> StateIdentifier -> MonadPerspectivesTransaction Unit
enteringRoleState roleId stateId = do
  padding <- lift transactionLevel
  log (padding <> "Entering role state " <> unwrap stateId <> " for role " <> unwrap roleId)
  -- Add the state identifier to the states in the role instance, triggering query updates
  -- just before running the current Transaction is finished.
  setActiveRoleState stateId roleId
  {automaticOnEntry, notifyOnEntry, perspectivesOnEntry} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction, but only if
  -- the end user fills the allowedUser role in this context.
  -- NOTE that the userRoleType is computed prior to executing the transaction.
  -- It may happen that during the transaction, the end user is put into another role (too).
  -- So we really should compute the userRoleType only now - or check if the end user (sys:Me) ultimately
  -- fills the allowdUser RoleType.
  me <- lift getPerspectivesUser
  mySystem <- lift $ getMySystem
  forWithIndex_ automaticOnEntry \(allowedUser :: RoleType) {updater, contextGetter} -> whenRightUser 
    roleId
    contextGetter 
    allowedUser 
    \currentactors cid rid -> do
      oldFrame <- lift pushFrame
      -- no need to add currentcontext for context states; a binding has been added compile time.
      lift $ addBinding "currentcontext" [(unwrap cid)]
      lift $ addBinding "currentactor" (unwrap <$> currentactors)
      updater rid
      lift $ restoreFrame oldFrame

  forWithIndex_ notifyOnEntry \(allowedUser :: RoleType) ({contextGetter, updater} :: CompiledNotification) -> whenRightUser 
    roleId 
    contextGetter
    allowedUser 
    \notifiedUsers cid rid -> do
      oldFrame <- lift pushFrame
      lift $ addBinding "currentcontext" [(unwrap cid)]
      lift $ addBinding "notifieduser" (unwrap <$> notifiedUsers)
      updater rid
      lift $ restoreFrame oldFrame


  State {stateFulObject, object} <- lift $ getState stateId
  case object of
    Nothing -> pure unit
    Just objectQfd -> forWithIndex_ perspectivesOnEntry \(allowedUser :: RoleType) {contextGetter, properties, selfOnly, authorOnly, isSelfPerspective} -> do
      currentcontext <- lift $ (roleId ##>> contextGetter)
      userInstances <- lift (currentcontext ##= COMB.filter (getRoleInstances allowedUser) (COMB.not' (filledBy (Filler_ $ perspectivesUser2RoleInstance me)) <<< Filled_))
      case fromArray userInstances of
        Nothing -> pure unit
        -- As the user gets a new perspective, he should have the corresonding resources. Hence we serialise them as Deltas and 
        -- add these to the transaction.
        Just u' -> if authorOnly
          then pure unit
          else case stateFulObject of
            -- The users acquire a perspective on the subject because it enters a state. The users should only see this instance.
            Orole _ -> serialiseRoleInstancesAndProperties
              currentcontext
              u'
              (UQD (domain objectQfd) (UnaryCombinator RoleIndividualF) (SQD (domain objectQfd) (Constant PString (unwrap roleId)) (VDOM PString Nothing) True True) (range objectQfd) True True)
              properties
              selfOnly
              isSelfPerspective
            -- The alternative must be Srole. When a subject (=the user) enters a state and then acquires a new perspective, it is valid for all instances - but just for the user role that enters that state!
            Srole _ -> serialiseRoleInstancesAndProperties
                currentcontext
                (unsafePartial fromJust (fromArray [roleId]))
                objectQfd
                properties
                selfOnly
                isSelfPerspective
            --  Finally, the last case must be Cnt. When a context enters a state and then all users acquires a new perspective on all instances.
            _ -> serialiseRoleInstancesAndProperties
                currentcontext
                u'
                objectQfd
                properties
                selfOnly
                isSelfPerspective

  -- Recur.
  subStates <- lift $ subStates_ stateId
  for_ subStates (evaluateRoleState roleId)

whenRightUser :: RoleInstance -> (RoleInstance ~~> ContextInstance) -> RoleType -> (Array RoleInstance -> ContextInstance -> Updater RoleInstance) -> MonadPerspectivesTransaction Unit
whenRightUser roleId contextGetter allowedUser updater = do
  contextId <- lift $ (roleId ##>> contextGetter)
  currentactors <- lift $ (contextId ##= (getRoleInstances allowedUser))
  -- Find the actor(s) that the system user ultimately fills.
  actorsThatAreMe <- lift (filterA isMe currentactors)
  if null actorsThatAreMe
    then pure unit
    else updater actorsThatAreMe contextId roleId

-- | Creates a Notificaton role in either the User's System context, or in the context of the role that enters the state 
-- | of which the user is notified. In both cases, the Notification role is filled with the external role of the context
-- | of the role that enters the state.
notify :: CompiledSentence RoleInstance -> (RoleInstance ~~> ContextInstance) -> RoleInstance -> MonadPerspectivesTransaction Unit
notify compiledSentence contextGetter roleId = do
  currentcontext <- lift $ (roleId ##>> contextGetter)
  lift $ addBinding "currentcontext" [(unwrap currentcontext)]
  storeNotificationInContext <- lift (currentcontext ##>> (contextType >=> liftToInstanceLevel  (hasContextAspect (ContextType contextWithNotification))))
  sentenceText <- lift $ compiledSentence roleId
  mySystem <- lift $ getMySystem
  void $ createAndAddRoleInstance
    (EnumeratedRoleType notifications)
    (if storeNotificationInContext
      then (unwrap currentcontext)
      else mySystem)
    (RolSerialization
      { id: Nothing
      , properties: PropertySerialization $ singleton notificationMessage
        [sentenceText]
      , binding: Just $ buitenRol (unwrap currentcontext)})

-- | This function is only called on states that the role was in before. Moreover, it (the role) is no longer in
-- | the parent state of this state, hence we should exit it. We do not check the condition.
-- |
-- | On exiting a state, we find all substates that are still active (if any) and apply `exitingRoleState` to it.
-- | We then de-register that state with the role instance and trigger client query updates.
-- | We then run all automatic actions and create notifications.
-- |
-- | Invariant: the state is registered as inactive and its query evaluates to false.
-- |
-- | Put an error boundary around this function.
-- | Ensure that the current context is available in the environment before applying this function!
exitingRoleState :: RoleInstance -> StateIdentifier -> MonadPerspectivesTransaction Unit
exitingRoleState roleId stateId = do
  padding <- lift transactionLevel
  log (padding <> "Exiting role state " <> unwrap stateId <> " for role " <> unwrap roleId)
  -- Recur. We do this first, because we have to exit the deepest nested substate first.
  subStates <- lift $ subStates_ stateId
  for_ subStates \subStateId -> do
    roleWasInSubState <- lift $ isActive subStateId roleId
    if roleWasInSubState
      then exitingRoleState roleId subStateId
      else pure unit
  -- Remove the state identifier from the states in the role instance, triggering query updates
  -- just before running the current Transaction is finished.
  setInActiveRoleState stateId roleId
  
  -- Stop all repeating processes associated with this state.
  fibers <- lift $ gets _.transactionFibers
  case lookup (Tuple (unwrap roleId) stateId) fibers of
    Nothing -> pure unit
    Just f -> do 
      lift $ lift $ killFiber (error "Stopped execution of repeating action in state") f
      lift $ modify \s@{transactionFibers} -> s {transactionFibers = delete (Tuple (unwrap roleId) stateId) transactionFibers}

  {automaticOnExit, notifyOnExit} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction, but only if
  -- the end user fills the allowedUser role in this context.
  -- NOTE that the userRoleType is computed prior to executing the transaction.
  -- It may happen that during the transaction, the end user is put into another role (too).
  -- So we really should compute the userRoleType only now - or check if the end user (sys:Me) ultimately
  -- fills the allowdUser RoleType.
  forWithIndex_ automaticOnExit \(allowedUser :: RoleType) {updater, contextGetter} -> whenRightUser 
    roleId
    contextGetter 
    allowedUser 
    \currentactors cid rid -> do
      oldFrame <- lift pushFrame
      -- no need to add currentcontext for context states; a binding has been added compile time.
      lift $ addBinding "currentcontext" [(unwrap cid)]
      lift $ addBinding "currentactor" (unwrap <$> currentactors)
      updater rid
      lift $ restoreFrame oldFrame

  forWithIndex_ notifyOnExit \(allowedUser :: RoleType) ({contextGetter, updater} :: CompiledNotification) -> whenRightUser 
    roleId 
    contextGetter
    allowedUser 
    \notifiedUsers cid rid -> do
      oldFrame <- lift pushFrame
      lift $ addBinding "currentcontext" [(unwrap cid)]
      lift $ addBinding "notifieduser" (unwrap <$> notifiedUsers)
      updater rid
      lift $ restoreFrame oldFrame

  
  -- (notify roleId me)

-- | Absence of a condition result is interpreted as false.
conditionSatisfied :: RoleInstance -> StateIdentifier -> MonadPerspectivesTransaction ConditionResult
conditionSatisfied roleId stateId = do
  compiledState <- getCompiledState stateId
  (Tuple bools (ArrayWithoutDoubles a0) :: WithAssumptions Value) <- lift $ runMonadPerspectivesQuery roleId compiledState.query
  d <- isUndetermined a0
  if (not null a0) && d 
    then pure Undetermined
    else pure $ Determined $ (not null bools) && (alaF Conj foldMap (eq (Value "true")) bools)

getCompiledState :: StateIdentifier -> MonadPerspectivesTransaction CompiledRoleState
getCompiledState stateId = case retrieveCompiledRoleState stateId of
  Nothing -> lift $ unsafePartial $ compileState stateId
  Just c -> pure c

isActive :: StateIdentifier -> RoleInstance -> MonadPerspectives Boolean
isActive stateId roleId = getActiveRoleStates_ roleId >>= pure <<< isJust <<< elemIndex stateId

-- isEntering :: State -> ContextInstance -> Boolean
-- isExiting :: State -> ContextInstance -> Boolean

getActiveSubstate :: StateIdentifier -> RoleInstance -> MonadPerspectivesTransaction (Maybe StateIdentifier)
getActiveSubstate stateId roleId = (lift $ getActiveRoleStates_ roleId) >>= \states -> pure $ join ((\i -> index states (i + 1)) <$> (elemIndex stateId states))
