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
import Data.Array (elemIndex, foldMap, index, null)
import Data.Array.NonEmpty (fromArray)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (ala, alaF, over, unwrap)
import Data.Traversable (for_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class.Console (log)
import Foreign.Object (singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.SentenceCompiler (compileRoleSentence)
import Perspectives.Assignment.SerialiseAsDeltas (serialiseRoleInstancesAndProperties)
import Perspectives.Assignment.StateCache (CompiledAutomaticAction, CompiledNotification, CompiledRoleState, CompiledStateDependentPerspective, cacheCompiledRoleState, retrieveCompiledRoleState)
import Perspectives.Assignment.Update (setActiveRoleState, setInActiveRoleState)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CompileRoleAssignment (compileAssignmentFromRole)
import Perspectives.CoreTypes (type (~~>), MP, MonadPerspectives, MonadPerspectivesTransaction, Updater, WithAssumptions, liftToInstanceLevel, runMonadPerspectivesQuery, (##=), (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter, not') as COMB
import Perspectives.Instances.ObjectGetters (boundByRole, contextType, getActiveRoleStates_)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.PerspectivesState (addBinding, pushFrame, restoreFrame)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Query.UnsafeCompiler (getRoleInstances, role2context, role2propertyValue)
import Perspectives.Representation.Action (AutomaticAction(..))
import Perspectives.Representation.Class.PersistentType (getState)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.State (Notification(..), State(..), StateDependentPerspective(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), RoleType, StateIdentifier)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (hasContextAspect, subStates_)
import Perspectives.Utilities (findM)

-- | This function has a Partial constraint because it only handles the AutomaticRoleAction case of AutomaticAction,
-- | and idem of Notification and StateDependentPerspective.
-- | The function can be safely applied (with `unsafePartial`), however.
compileState :: Partial => StateIdentifier -> MP CompiledRoleState
compileState stateId = do
    State {id, query, automaticOnEntry, automaticOnExit, notifyOnEntry, notifyOnExit, perspectivesOnEntry} <- getState stateId
    (automaticOnEntry' :: Map RoleType CompiledAutomaticAction) <- traverseWithIndex
      (\subject (RoleAction{currentContextCalculation, effect}) -> do
        updater <- compileAssignmentFromRole effect >>= pure <<< withAuthoringRole subject
        contextGetter <- role2context currentContextCalculation
        pure {updater, contextGetter})
      (unwrap automaticOnEntry)
    (automaticOnExit' :: Map RoleType CompiledAutomaticAction) <- traverseWithIndex
      (\subject (RoleAction{currentContextCalculation, effect}) -> do
        updater <- compileAssignmentFromRole effect >>= pure <<< withAuthoringRole subject
        contextGetter <- role2context currentContextCalculation
        pure {updater, contextGetter})
      (unwrap automaticOnExit)
    (notifyOnEntry' :: Map RoleType CompiledNotification) <- traverseWithIndex
      (\subject (RoleNotification{currentContextCalculation, sentence}) -> do
        compiledSentence <- compileRoleSentence sentence
        contextGetter <- role2context currentContextCalculation
        pure {compiledSentence, contextGetter})
      (unwrap notifyOnEntry)
    (notifyOnExit' :: Map RoleType CompiledNotification) <- traverseWithIndex
      (\subject (RoleNotification{currentContextCalculation, sentence}) -> do
        compiledSentence <- compileRoleSentence sentence
        contextGetter <- role2context currentContextCalculation
        pure {compiledSentence, contextGetter})
      (unwrap notifyOnExit)
    (perspectivesOnEntry' :: Map RoleType CompiledStateDependentPerspective) <- traverseWithIndex
      (\subject (RolePerspective{currentContextCalculation, properties, selfOnly, isSelfPerspective}) -> do
        contextGetter <- role2context currentContextCalculation
        pure {contextGetter, properties, selfOnly, isSelfPerspective})
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
    withAuthoringRole :: forall a. RoleType -> Updater a -> a -> MonadPerspectivesTransaction Unit
    withAuthoringRole aRole updater a = do
      originalRole <- lift $ gets (_.authoringRole <<< unwrap)
      lift $ modify (over Transaction \t -> t {authoringRole = aRole})
      updater a
      lift $ modify (over Transaction \t -> t {authoringRole = originalRole})

-- | This function is applied without knowing whether state condition is valid.
-- | Put an error boundary around this function.
evaluateRoleState :: RoleInstance -> StateIdentifier -> MonadPerspectivesTransaction Unit
evaluateRoleState roleId stateId = do
  roleIsInState <- conditionSatisfied roleId stateId
  if roleIsInState
    then do
      roleWasInState <- lift2 $ isActive stateId roleId
      if roleWasInState
        then do
          log ("Already in role state " <> unwrap stateId <> ": " <> unwrap roleId)
          subStates <- lift2 $ subStates_ stateId
          for_ subStates (evaluateRoleState roleId)
        else enteringRoleState roleId stateId
    else do
      roleWasInState <- lift2 $ isActive stateId roleId
      if roleWasInState
        then exitingRoleState roleId stateId
        else pure unit

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
  log ("Entering role state " <> unwrap stateId <> " for role " <> unwrap roleId)
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
  me <- lift2 getUserIdentifier
  mySystem <- lift2 $ getMySystem
  forWithIndex_ automaticOnEntry \(allowedUser :: RoleType) {updater, contextGetter} -> do
    currentcontext <- lift2 $ (roleId ##>> contextGetter)
    currentactors <- lift2 $ (currentcontext ##= (getRoleInstances allowedUser))
    bools <- lift2 $ (currentactors ##= ((\_ -> ArrayT $ pure currentactors) >=> boundByRole (RoleInstance me)))
    if ala Conj foldMap bools
      then do
          oldFrame <- lift2 $ pushFrame
          lift2 $ addBinding "currentcontext" [(unwrap currentcontext)]
          lift2 $ addBinding "origin" [unwrap roleId]
          lift2 $ addBinding "currentactor" (unwrap <$> currentactors)
          updater roleId
          lift2 $ restoreFrame oldFrame
      else pure unit
  forWithIndex_ notifyOnEntry (notify roleId me)
  State {object} <- lift2 $ getState stateId
  case object of
    Nothing -> pure unit
    Just objectQfd -> forWithIndex_ perspectivesOnEntry \(allowedUser :: RoleType) {contextGetter, properties, selfOnly, isSelfPerspective} -> do
      currentcontext <- lift2 $ (roleId ##>> contextGetter)
      userInstances <- lift2 (currentcontext ##= COMB.filter (getRoleInstances allowedUser) (COMB.not' (boundByRole (RoleInstance me))))
      case fromArray userInstances of
        Nothing -> pure unit
        Just u' -> serialiseRoleInstancesAndProperties
          currentcontext
          u'
          objectQfd
          properties
          selfOnly
          isSelfPerspective

  -- Recur.
  subStates <- lift2 $ subStates_ stateId
  for_ subStates (evaluateRoleState roleId)

notify :: RoleInstance -> String -> RoleType -> CompiledNotification -> MonadPerspectivesTransaction Unit
notify roleId me allowedUser {compiledSentence, contextGetter} = do
  currentcontext <- lift2 $ (roleId ##>> contextGetter)
  currentactors <- lift2 $ (currentcontext ##= (getRoleInstances allowedUser))
  bools <- lift2 $ (currentactors ##= ((\_ -> ArrayT $ pure currentactors) >=> boundByRole (RoleInstance me)))
  if ala Conj foldMap bools
    then do
      oldFrame <- lift2 $ pushFrame
      lift2 $ addBinding "currentcontext" [(unwrap currentcontext)]
      lift2 $ addBinding "origin" [unwrap roleId]
      lift2 $ addBinding "currentactor" (unwrap <$> currentactors)
      storeNotificationInContext <- lift2 (currentcontext ##>> (contextType >=> liftToInstanceLevel  (hasContextAspect (ContextType "model:System$ContextWithNotification"))))
      sentenceText <- lift2 $ compiledSentence roleId
      mySystem <- lift2 $ getMySystem
      void $ createAndAddRoleInstance
        (EnumeratedRoleType "model:System$ContextWithNotification$Notifications")
        (if storeNotificationInContext
          then (unwrap currentcontext)
          else mySystem)
        (RolSerialization
          { id: Nothing
          , properties: PropertySerialization $ singleton "model:System$ContextWithNotification$Notifications$Message"
            [sentenceText]
          -- TODO. Dit is niet zeker. Moeten we niet hier de externe rol van de context van de roleId geven?
          , binding: Just $ unwrap roleId})
      lift2 $ restoreFrame oldFrame
    else pure unit

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
  log ("Exiting role state " <> unwrap stateId <> " for role " <> unwrap roleId)
  -- Recur. We do this first, because we have to exit the deepest nested substate first.
  subStates <- lift2 $ subStates_ stateId
  for_ subStates \subStateId -> do
    roleWasInSubState <- lift2 $ isActive subStateId roleId
    if roleWasInSubState
      then exitingRoleState roleId subStateId
      else pure unit
  -- Remove the state identifier from the states in the role instance, triggering query updates
  -- just before running the current Transaction is finished.
  setInActiveRoleState stateId roleId
  {automaticOnExit, notifyOnExit} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction, but only if
  -- the end user fills the allowedUser role in this context.
  -- NOTE that the userRoleType is computed prior to executing the transaction.
  -- It may happen that during the transaction, the end user is put into another role (too).
  -- So we really should compute the userRoleType only now - or check if the end user (sys:Me) ultimately
  -- fills the allowdUser RoleType.
  me <- lift2 getUserIdentifier
  forWithIndex_ automaticOnExit \(allowedUser :: RoleType) {updater, contextGetter} -> do
    bools <- lift2 (roleId ##= contextGetter >=> getRoleInstances allowedUser >=> boundByRole (RoleInstance me))
    if ala Conj foldMap bools
      then updater roleId
      else pure unit
  forWithIndex_ notifyOnExit (notify roleId me)

-- | Check all substates until one of them is found for which conditionSatisfied holds;
findSatisfiedSubstate :: StateIdentifier -> RoleInstance -> MonadPerspectivesTransaction (Maybe StateIdentifier)
findSatisfiedSubstate stateId roleId = (lift2 $ subStates_ stateId) >>= findM (conditionSatisfied roleId)

-- | Absence of a condition result is interpreted as false.
conditionSatisfied :: RoleInstance -> StateIdentifier -> MonadPerspectivesTransaction Boolean
conditionSatisfied roleId stateId = do
  compiledState <- getCompiledState stateId
  (Tuple bools a0 :: WithAssumptions Value) <- lift $ lift $ runMonadPerspectivesQuery roleId compiledState.query
  pure $ (not null bools) && (alaF Conj foldMap (eq (Value "true")) bools)

getCompiledState :: StateIdentifier -> MonadPerspectivesTransaction CompiledRoleState
getCompiledState stateId = case retrieveCompiledRoleState stateId of
  Nothing -> lift2 $ unsafePartial $ compileState stateId
  Just c -> pure c

isActive :: StateIdentifier -> RoleInstance -> MonadPerspectives Boolean
isActive stateId roleId = getActiveRoleStates_ roleId >>= pure <<< isJust <<< elemIndex stateId

-- isEntering :: State -> ContextInstance -> Boolean
-- isExiting :: State -> ContextInstance -> Boolean

getActiveSubstate :: StateIdentifier -> RoleInstance -> MonadPerspectivesTransaction (Maybe StateIdentifier)
getActiveSubstate stateId roleId = (lift $ lift $ getActiveRoleStates_ roleId) >>= \states -> pure $ join ((\i -> index states (i + 1)) <$> (elemIndex stateId states))
