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
import Data.Array (elemIndex, foldMap, null)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (ala, alaF, over, unwrap)
import Data.Traversable (for_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class.Console (log)
import Foreign.Object (singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.StateCache (CompiledContextState, cacheCompiledContextState, retrieveCompiledContextState)
import Perspectives.Assignment.Update (setActiveContextState, setInActiveContextState, withAuthoringRole)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CompileAssignment (compileAssignment)
import Perspectives.CoreTypes (type (~~>), MP, MonadPerspectivesTransaction, Updater, WithAssumptions, MonadPerspectives, runMonadPerspectivesQuery, (##=))
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (boundByRole, getActiveStates_)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.PerspectivesState (addBinding, pushFrame, restoreFrame)
import Perspectives.Query.QueryTypes (Calculation(..), QueryFunctionDescription)
import Perspectives.Query.UnsafeCompiler (context2propertyValue, getRoleInstances, roleFunctionFromQfd)
import Perspectives.Representation.Class.PersistentType (getState)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType, StateIdentifier)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (subStates_)

compileState :: StateIdentifier -> MP CompiledContextState
compileState stateId = do
    State {query, object, automaticOnEntry, automaticOnExit} <- getState stateId
    (mobjectGetter :: Maybe (ContextInstance ~~> RoleInstance)) <- traverse roleFunctionFromQfd object
    (automaticOnEntry' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
      (\subject (effect :: QueryFunctionDescription) -> compileAssignment effect >>= pure <<< withAuthoringRole subject)
      (unwrap automaticOnEntry)
    (automaticOnExit' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
      (\subject (effect :: QueryFunctionDescription) ->
        compileAssignment effect >>= pure <<< withAuthoringRole subject)
      (unwrap automaticOnExit)
    -- TODO notifyOnEntry, notifyOnExit.

    -- We postpone compiling substates until they're asked for.
    (lhs :: (ContextInstance ~~> Value)) <- context2propertyValue $ unsafePartial case query of Q qfd -> qfd
    pure $ cacheCompiledContextState stateId
      { query: lhs
      , objectGetter: mobjectGetter
      , automaticOnEntry: automaticOnEntry'
      , automaticOnExit: automaticOnExit'
      }
  where
    withAuthoringRole :: forall a. RoleType -> Updater a -> a -> MonadPerspectivesTransaction Unit
    withAuthoringRole aRole updater a = do
      originalRole <- lift $ gets (_.authoringRole <<< unwrap)
      lift $ modify (over Transaction \t -> t {authoringRole = aRole})
      updater a
      lift $ modify (over Transaction \t -> t {authoringRole = originalRole})

-- | This function is applied without knowing whether state condition is valid.
-- | Ensure that the current context is available in the environment before applying this function!
-- | Put an error boundary around this function.
evaluateContextState :: ContextInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
evaluateContextState contextId userRoleType stateId = do
  log ("Evaluating context state " <> unwrap stateId <> " for context " <> unwrap contextId)
  contextIsInState <- conditionSatisfied contextId stateId
  if contextIsInState
    then do
      contextWasInState <- lift2 $ isActive stateId contextId
      if contextWasInState
        then do
          subStates <- lift2 $ subStates_ stateId
          for_ subStates (evaluateContextState contextId userRoleType)
        else enteringState contextId userRoleType stateId
    else do
      contextWasInState <- lift2 $ isActive stateId contextId
      if contextWasInState
        then exitingState contextId userRoleType stateId
        else pure unit

-- | This function is only called (and should only be called) on states whose condition is valid.
-- | On entering a state, we register that state with the context instance and trigger client query updates.
-- | We run all automatic actions and create notifications.
-- | Finally, we look for the substate whose query returns true and apply `enteringState` to it.
-- | Invariant: the state is not registered as active but its query evaluates to true.
-- |
-- | Put an error boundary around this function.
-- | Ensure that the current context is available in the environment before applying this function!
enteringState :: ContextInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
enteringState contextId userRoleType stateId = do
  log ("Entering context state " <> unwrap stateId <> " for context " <> unwrap contextId)
  -- Add the state identifier to the path of states in the context instance, triggering query updates
  -- just before running the current Transaction is finished.
  setActiveContextState stateId contextId
  {automaticOnEntry, objectGetter} <- getCompiledState stateId
  objects <- case objectGetter of
    Nothing -> pure []
    Just getter -> lift2 (contextId ##= getter)
  -- Run automatic actions in the current Transaction, but only if
  -- the end user fills the allowedUser role in this context.
  -- NOTE that the userRoleType is computed prior to executing the transaction.
  -- It may happen that during the transaction, the end user is put into another role (too).
  -- So we really should compute the userRoleType only now - or check if the end user (sys:Me) ultimately
  -- fills the allowdUser RoleType.
  forWithIndex_ automaticOnEntry \(allowedUser :: RoleType) updater -> do
    me <- lift2 getUserIdentifier
    bools <- lift2 (contextId ##= getRoleInstances allowedUser >=> boundByRole (RoleInstance me))
    if ala Conj foldMap bools
      -- Run for each object.
      then withAuthoringRole allowedUser
        if isJust objectGetter
          then for_ objects \object -> do
            oldFrame <- lift2 pushFrame
            lift2 $ addBinding "object" [unwrap object]
            updater contextId
            lift2 $ restoreFrame oldFrame
          -- Note we do not push an empty set of values for object, like we did for rules.
          -- The modeller should not use the 'object' variable.
          else updater contextId
      else pure unit
  State {notifyOnEntry} <- lift2 $ getState stateId
  case lookup userRoleType (unwrap notifyOnEntry) of
    Nothing -> pure unit
    Just l -> do
      mySystem <- lift2 $ getMySystem
      void $ createAndAddRoleInstance
        (EnumeratedRoleType "model:System$PerspectivesSystem$ContextNotification")
        mySystem
        (RolSerialization
          { id: Nothing
          , properties: PropertySerialization $ singleton "model:System$PerspectivesSystem$ContextNotification$Level"
            [(show l)]
          , binding: Just $ buitenRol (unwrap contextId)})
  -- Recur.
  subStates <- lift2 $ subStates_ stateId
  for_ subStates (evaluateContextState contextId userRoleType)

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
exitingState :: ContextInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
exitingState contextId userRoleType stateId = do
  log ("Exiting context state " <> unwrap stateId <> " for context " <> unwrap contextId)
  -- Recur. We do this first, because we have to exit the deepest nested substate first.
  subStates <- lift2 $ subStates_ stateId
  for_ subStates \subStateId -> do
    contextWasInSubState <- lift2 $ isActive subStateId contextId
    if contextWasInSubState
      then exitingState contextId userRoleType subStateId
      else pure unit
  -- Add the state identifier to the path of states in the context instance, triggering query updates
  -- just before running the current Transaction is finished.
  setInActiveContextState stateId contextId
  {automaticOnExit, objectGetter} <- getCompiledState stateId
  objects <- case objectGetter of
    Nothing -> pure []
    Just getter -> lift2 (contextId ##= getter)
  -- Run automatic actions in the current Transaction, but only if
  -- the end user fills the allowedUser role in this context.
  -- NOTE that the userRoleType is computed prior to executing the transaction.
  -- It may happen that during the transaction, the end user is put into another role (too).
  -- So we really should compute the userRoleType only now - or check if the end user (sys:Me) ultimately
  -- fills the allowdUser RoleType.
  forWithIndex_ automaticOnExit \(allowedUser :: RoleType) updater -> do
    me <- lift2 getUserIdentifier
    bools <- lift2 (contextId ##= getRoleInstances allowedUser >=> boundByRole (RoleInstance me))
    if ala Conj foldMap bools
      -- Run for each object.
      then if isJust objectGetter
        then for_ objects \object -> do
          oldFrame <- lift2 pushFrame
          lift2 $ addBinding "object" [unwrap object]
          updater contextId
          lift2 $ restoreFrame oldFrame
        else updater contextId
      else pure unit
  State {notifyOnExit} <- lift2 $ getState stateId
  case lookup userRoleType (unwrap notifyOnExit) of
    Nothing -> pure unit
    Just l -> do
      mySystem <- lift2 $ getMySystem
      void $ createAndAddRoleInstance
        (EnumeratedRoleType "model:System$PerspectivesSystem$ContextNotification")
        mySystem
        (RolSerialization
          { id: Nothing
          , properties: PropertySerialization $ singleton "model:System$PerspectivesSystem$ContextNotification$Level"
            [(show l)]
          , binding: Just $ buitenRol (unwrap contextId)})

conditionSatisfied :: ContextInstance -> StateIdentifier -> MonadPerspectivesTransaction Boolean
conditionSatisfied contextId stateId = do
  compiledState <- getCompiledState stateId
  (Tuple bools a0 :: WithAssumptions Value) <- lift $ lift $ runMonadPerspectivesQuery contextId compiledState.query
  pure $ (not null bools) && (alaF Conj foldMap (eq (Value "true")) bools)

getCompiledState :: StateIdentifier -> MonadPerspectivesTransaction CompiledContextState
getCompiledState stateId = case retrieveCompiledContextState stateId of
  Nothing -> lift2 $ compileState stateId
  Just c -> pure c

isActive :: StateIdentifier -> ContextInstance -> MonadPerspectives Boolean
isActive stateId contextId = getActiveStates_ contextId >>= pure <<< isJust <<< elemIndex stateId
