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

module Perspectives.ContextStateCompiler where

-- | From the description of a state, compute a function that computes for an instance of its context type
-- | whether that state is in effect. If entering the state, run all entry effects for the current user.
-- | On exiting the state, run all exit effects.
-- | Notify the current user on entering and exiting if specified.
-- | Cache the computed function for the duration of the session.

import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, elemIndex, filterA, foldMap, null)
import Data.Array.NonEmpty (fromArray)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map, lookup, delete)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF, over, unwrap)
import Data.Traversable (for_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Aff (error, killFiber)
import Effect.Class.Console (log)
import Foreign.Object (singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.SentenceCompiler (CompiledSentence, compileContextSentence)
import Perspectives.Assignment.SerialiseAsDeltas (serialiseRoleInstancesAndProperties)
import Perspectives.Assignment.StateCache (CompiledContextState, cacheCompiledContextState, retrieveCompiledContextState)
import Perspectives.Assignment.Update (ConditionResult(..), isUndetermined, setActiveContextState, setInActiveContextState)
import Perspectives.CompileAssignment (compileAssignment, withAuthoringRole)
import Perspectives.CompileTimeFacets (addTimeFacets)
import Perspectives.CoreTypes (type (~~>), ArrayWithoutDoubles(..), MP, MonadPerspectives, MonadPerspectivesTransaction, Updater, WithAssumptions, liftToInstanceLevel, runMonadPerspectivesQuery, (##=), (##>>))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter, not') as COMB
import Perspectives.Instances.Me (isMe)
import Perspectives.Instances.ObjectGetters (Filled_(..), Filler_(..), contextType, filledBy, getActiveStates_)
import Perspectives.ModelDependencies (contextWithNotification, notificationMessage, notifications)
import Perspectives.Names (getMySystem)
import Perspectives.PerspectivesState (addBinding, getPerspectivesUser, pushFrame, restoreFrame, transactionLevel)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Query.UnsafeCompiler (context2propertyValue, getRoleInstances, roleFunctionFromQfd)
import Perspectives.Representation.Action (AutomaticAction(..))
import Perspectives.Representation.Class.PersistentType (getState)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..), externalRole, perspectivesUser2RoleInstance)
import Perspectives.Representation.State (Notification(..), State(..), StateDependentPerspective(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), PropertyType, RoleType, StateIdentifier)
import Perspectives.ScheduledAssignment (StateEvaluation(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (hasContextAspect, subStates_)

compileState :: Partial => StateIdentifier -> MP CompiledContextState
compileState stateId = do
    State {query, object, automaticOnEntry, automaticOnExit, notifyOnEntry, notifyOnExit, perspectivesOnEntry} <- getState stateId
    (mobjectGetter :: Maybe (ContextInstance ~~> RoleInstance)) <- traverse roleFunctionFromQfd object
    (automaticOnEntry' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
      (\subject action -> 
        compileEffect action subject >>= pure <<< withAuthoringRole subject)
      (unwrap automaticOnEntry)
    (automaticOnExit' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
      (\subject action ->
        compileEffect action subject >>= pure <<< withAuthoringRole subject)
      (unwrap automaticOnExit)
    (notifyOnEntry' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
      (\subject notification -> compileNotification notification subject)
      (unwrap notifyOnEntry)
    (notifyOnExit' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
      (\subject notification -> compileNotification notification subject)
      (unwrap notifyOnExit)
    (perspectivesOnEntry' :: Map RoleType { properties :: Array PropertyType, selfOnly :: Boolean, authorOnly :: Boolean, isSelfPerspective :: Boolean }) <- traverseWithIndex
      (\subject (ContextPerspective r) -> pure r)
      (unwrap perspectivesOnEntry)

    -- We postpone compiling substates until they're asked for.
    (lhs :: (ContextInstance ~~> Value)) <- context2propertyValue $ unsafePartial case query of Q qfd -> qfd
    pure $ cacheCompiledContextState stateId
      { query: lhs
      , objectGetter: mobjectGetter
      , automaticOnEntry: automaticOnEntry'
      , automaticOnExit: automaticOnExit'
      , notifyOnEntry: notifyOnEntry'
      , notifyOnExit: notifyOnExit'
      , perspectivesOnEntry: perspectivesOnEntry'
      }
  where
    compileEffect :: Partial => AutomaticAction -> RoleType -> MP (Updater ContextInstance)
    compileEffect (ContextAction r@{effect}) subject = do
      compiledEffect <- compileAssignment effect
      addTimeFacets compiledEffect r subject stateId
    
    compileNotification :: Partial => Notification -> RoleType -> MP (Updater ContextInstance)
    compileNotification (ContextNotification r@{sentence, domain}) subject = do
      compiledSentence <- compileContextSentence domain sentence
      updater <- pure $ notify compiledSentence
      addTimeFacets updater r subject stateId
    
-- | This function is applied without knowing whether state condition is valid.
-- | Ensure that the current context is available in the environment before applying this function!
-- | Put an error boundary around this function.
evaluateContextState :: ContextInstance -> StateIdentifier -> MonadPerspectivesTransaction Unit
evaluateContextState contextId stateId = do
  contextIsInState' <- conditionSatisfied contextId stateId
  case contextIsInState' of 
    Determined contextIsInState -> if contextIsInState
      then do
        contextWasInState <- lift $ isActive stateId contextId
        if contextWasInState
          then do
            padding <- lift transactionLevel
            log (padding <> "Already in context state " <> unwrap stateId <> ": " <> unwrap contextId)
            subStates <- lift $ subStates_ stateId
            for_ subStates (evaluateContextState contextId)
          else enteringState contextId stateId
      else do
        contextWasInState <- lift $ isActive stateId contextId
        if contextWasInState
          then exitingState contextId stateId
          else pure unit
    Undetermined -> modify 
      (\t -> over Transaction 
        (\tr -> tr { postponedStateEvaluations = cons (ContextStateEvaluation stateId contextId) tr.postponedStateEvaluations }) 
        t )

-- | This function is only called (and should only be called) on states whose condition is valid.
-- | On entering a state, we register that state with the context instance and trigger client query updates.
-- | We run all automatic actions and create notifications.
-- | Finally, we look for the substate whose query returns true and apply `enteringState` to it.
-- | Invariant: the state is not registered as active but its query evaluates to true.
-- |
-- | Put an error boundary around this function.
-- | Ensure that the current context is available in the environment before applying this function!
enteringState :: ContextInstance -> StateIdentifier -> MonadPerspectivesTransaction Unit
enteringState contextId stateId = do
  padding <- lift transactionLevel
  log (padding <> "Entering context state " <> unwrap stateId <> " for context " <> unwrap contextId)
  -- Add the state identifier to the path of states in the context instance, triggering query updates
  -- just before running the current Transaction is finished.
  setActiveContextState stateId contextId
  {automaticOnEntry, perspectivesOnEntry, notifyOnEntry} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction, but only if
  -- the end user fills the allowedUser role in this context.
  -- NOTE that the userRoleType is computed prior to executing the transaction.
  -- It may happen that during the transaction, the end user is put into another role (too).
  -- So we really should compute the userRoleType only now - or check if the end user (sys:Me) ultimately
  -- fills the allowdUser RoleType.
  forWithIndex_ automaticOnEntry \(allowedUser :: RoleType) updater -> whenRightUser 
    contextId 
    allowedUser 
    \currentactors cid -> do
      oldFrame <- lift pushFrame
      -- no need to add currentcontext for context states; a binding has been added compile time.
      lift $ addBinding "currentactor" (unwrap <$> currentactors)
      updater cid
      lift $ restoreFrame oldFrame

  forWithIndex_ notifyOnEntry \(allowedUser :: RoleType) (compiledSentence :: Updater ContextInstance) -> whenRightUser 
    contextId 
    allowedUser 
    \notifiedUsers cid -> do
      oldFrame <- lift pushFrame
      lift $ addBinding "notifieduser" (unwrap <$> notifiedUsers)
      compiledSentence contextId 
      lift $ restoreFrame oldFrame

  -- NOTE. We have a compiled version of the object, too.
  State {object} <- lift $ getState stateId
  me <- lift getPerspectivesUser
  case object of
    Nothing -> pure unit
    Just objectQfd -> forWithIndex_ perspectivesOnEntry \(allowedUser :: RoleType) {properties, selfOnly, authorOnly, isSelfPerspective} -> do
      userInstances <- lift (contextId ##= COMB.filter (getRoleInstances allowedUser) ((COMB.not' (filledBy (Filler_ $ perspectivesUser2RoleInstance me))) <<< Filled_))
      case fromArray userInstances of
        Nothing -> pure unit
        Just u' -> if authorOnly
          then pure unit
          else serialiseRoleInstancesAndProperties
            contextId
            u'
            objectQfd
            properties
            selfOnly
            isSelfPerspective

  -- Recur.
  subStates <- lift $ subStates_ stateId
  for_ subStates (evaluateContextState contextId)

whenRightUser :: ContextInstance -> RoleType -> (Array RoleInstance -> Updater ContextInstance) -> MonadPerspectivesTransaction Unit
whenRightUser contextId allowedUser updater = do
  currentactors <- lift $ (contextId ##= (getRoleInstances allowedUser))
  actorsThatAreMe <- lift (filterA isMe currentactors)
  if null actorsThatAreMe
    then pure unit
    else updater actorsThatAreMe contextId

-- | Creates a Notificaton role in either the User's System context, or in the context that enters the state 
-- | of which the user is notified. In both cases, the Notification role is filled with the external role of the context that enters the state.
notify :: CompiledSentence ContextInstance -> ContextInstance -> MonadPerspectivesTransaction Unit
notify compiledSentence contextId = do
  storeNotificationInContext <- lift (contextId ##>> (contextType >=> liftToInstanceLevel (hasContextAspect (ContextType contextWithNotification))))
  sentenceText <- lift $ compiledSentence contextId
  mySystem <- lift $ getMySystem
  void $ createAndAddRoleInstance
    (EnumeratedRoleType notifications)
    (if storeNotificationInContext
      then (unwrap contextId)
      else mySystem)
    (RolSerialization
      { id: Nothing
      , properties: PropertySerialization $ singleton notificationMessage
        [sentenceText]
      , binding: Just $ unwrap $ externalRole contextId})

-- | This function is only called on states that the context was in before. Moreover, it (the context) is no longer in
-- | the parent state of this state, hence we should exit it. We do not check the condition.
-- |
-- | On exiting a state, we de-register that state with the context instance and trigger client query updates.
-- | We run all automatic actions and create notifications.
-- | Finally, we find the substate that is still active (if any) and apply `exitingState` to it.
-- | Invariant: the state is registered as active and its query evaluates to false.
-- |
-- | Put an error boundary around this function.
-- | Ensure that the current context is available in the environment before applying this function!
exitingState :: ContextInstance -> StateIdentifier -> MonadPerspectivesTransaction Unit
exitingState contextId stateId = do
  padding <- lift transactionLevel
  log (padding <> "Exiting context state " <> unwrap stateId <> " for context " <> unwrap contextId)
  -- Recur. We do this first, because we have to exit the deepest nested substate first.
  subStates <- lift $ subStates_ stateId
  for_ subStates \subStateId -> do
    contextWasInSubState <- lift $ isActive subStateId contextId
    if contextWasInSubState
      then exitingState contextId subStateId
      else pure unit
  -- Add the state identifier to the path of states in the context instance, triggering query updates
  -- just before running the current Transaction is finished.
  setInActiveContextState stateId contextId
  
  -- Stop all repeating processes associated with this state.
  fibers <- lift $ gets _.transactionFibers
  case lookup (Tuple (unwrap contextId) stateId) fibers of
    Nothing -> pure unit
    Just f -> do 
      lift $ lift $ killFiber (error "Stopped execution of repeating action in state") f
      lift $ modify \s@{transactionFibers} -> s {transactionFibers = delete (Tuple (unwrap contextId) stateId) transactionFibers}

  {automaticOnExit, notifyOnExit} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction, but only if
  -- the end user fills the allowedUser role in this context.
  -- NOTE that the userRoleType is computed prior to executing the transaction.
  -- It may happen that during the transaction, the end user is put into another role (too).
  -- So we really should compute the userRoleType only now - or check if the end user (sys:Me) ultimately
  -- fills the allowdUser RoleType.
  forWithIndex_ automaticOnExit \(allowedUser :: RoleType) updater -> whenRightUser 
    contextId 
    allowedUser 
    \currentactors cid -> do
      oldFrame <- lift pushFrame
      -- no need to add currentcontext for context states; a binding has been added compile time.
      lift $ addBinding "currentactor" (unwrap <$> currentactors)
      updater cid
      lift $ restoreFrame oldFrame

  forWithIndex_ notifyOnExit \(allowedUser :: RoleType) (compiledSentence :: Updater ContextInstance) -> whenRightUser 
    contextId 
    allowedUser 
    \notifiedUsers cid -> do
      oldFrame <- lift pushFrame
      lift $ addBinding "notifieduser" (unwrap <$> notifiedUsers)
      compiledSentence contextId 
      lift $ restoreFrame oldFrame

conditionSatisfied :: ContextInstance -> StateIdentifier -> MonadPerspectivesTransaction ConditionResult
conditionSatisfied contextId stateId = do
  compiledState <- getCompiledState stateId
  (Tuple bools (ArrayWithoutDoubles a0) :: WithAssumptions Value) <- lift $ runMonadPerspectivesQuery contextId compiledState.query
  d <- isUndetermined a0
  if (not null a0) && d 
    then pure Undetermined
    else pure $ Determined $ (not null bools) && (alaF Conj foldMap (eq (Value "true")) bools)

getCompiledState :: StateIdentifier -> MonadPerspectivesTransaction CompiledContextState
getCompiledState stateId = case retrieveCompiledContextState stateId of
  Nothing -> lift $ unsafePartial compileState stateId
  Just c -> pure c

isActive :: StateIdentifier -> ContextInstance -> MonadPerspectives Boolean
isActive stateId contextId = getActiveStates_ contextId >>= pure <<< isJust <<< elemIndex stateId
