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
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF, over, unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.StateCache (CompiledRoleState, cacheCompiledRoleState, retrieveCompiledRoleState)
import Perspectives.Assignment.Update (setActiveRoleState, setInActiveRoleState)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CompileRoleAssignment (compileAssignmentFromRole)
import Perspectives.CoreTypes (type (~~>), MP, MonadPerspectivesTransaction, Updater, WithAssumptions, MonadPerspectives, runMonadPerspectivesQuery)
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (getActiveRoleStates_)
import Perspectives.Names (getMySystem)
import Perspectives.Query.QueryTypes (Calculation(..), QueryFunctionDescription)
import Perspectives.Query.UnsafeCompiler (role2propertyValue)
import Perspectives.Representation.Class.PersistentType (getState)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance, Value(..))
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType, StateIdentifier)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (specialisesRoleType_, subStates_)
import Perspectives.Utilities (findM)

compileState :: StateIdentifier -> MP CompiledRoleState
compileState stateId = do
    State {query, object, automaticOnEntry, automaticOnExit} <- getState stateId
    (automaticOnEntry' :: Map RoleType (Updater RoleInstance)) <- traverseWithIndex
      (\subject (effect :: QueryFunctionDescription) -> compileAssignmentFromRole effect >>= pure <<< withAuthoringRole subject)
      (unwrap automaticOnEntry)
    (automaticOnExit' :: Map RoleType (Updater RoleInstance)) <- traverseWithIndex
      (\subject (effect :: QueryFunctionDescription) -> compileAssignmentFromRole effect >>= pure <<< withAuthoringRole subject)
      (unwrap automaticOnExit)
    -- TODO notifyOnEntry, notifyOnExit.

    -- We postpone compiling substates until they're asked for.
    (lhs :: (RoleInstance ~~> Value)) <- role2propertyValue $ unsafePartial case query of Q qfd -> qfd
    pure $ cacheCompiledRoleState stateId
      { query: lhs
      , objectGetter: Nothing
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

-- | Put an error boundary around this function.
evaluateRoleState :: RoleInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
evaluateRoleState roleId userRoleType stateId = do
  mactive <- getActiveSubstate stateId roleId
  case mactive of
    Nothing -> findSatisfiedSubstate stateId roleId >>= case _ of
      Nothing -> pure unit
      Just sub -> enteringRoleState roleId userRoleType sub
    Just sub -> conditionSatisfied roleId stateId >>= if _
      then evaluateRoleState roleId userRoleType sub
      else do
        exitingRoleState roleId userRoleType sub
        findSatisfiedSubstate stateId roleId >>= (void <<< traverse (enteringRoleState roleId userRoleType))

-- | On entering a state, we register that state with the role instance and trigger client query updates.
-- | We run all automatic actions and create notifications.
-- | Finally, we look for the substate whose query returns true and apply `enteringRoleState` to it.
-- | Invariant: the state is not registered as active but its query evaluates to true.
-- |
-- | Put an error boundary around this function.
-- | Ensure that the current context is available in the environment before applying this function!
enteringRoleState :: RoleInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
enteringRoleState roleId userRoleType stateId = do
  -- Add the state identifier to the path of states in the role instance, triggering query updates
  -- just before running the current Transaction is finished.
  setActiveRoleState stateId roleId
  {automaticOnEntry} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction.
  forWithIndex_ automaticOnEntry \allowedUser updater ->  (lift2 $ specialisesRoleType_ userRoleType allowedUser) >>= if _
    then updater roleId
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
          -- TODO. Dit is niet zeker. Moeten we niet hier de externe rol van de context van de roleId geven?
          , binding: Just $ unwrap roleId})
  -- Recur.
  findSatisfiedSubstate stateId roleId >>= void <<< traverse (enteringRoleState roleId userRoleType)

-- | On exiting a state, we de-register that state with the role instance and trigger client query updates.
-- | We run all automatic actions and create notifications.
-- | Finally, we find the substate that is still active (if any) and apply `exitingRoleState` to it.
-- | Invariant: the state is registered as active and its query evaluates to false.
-- |
-- | Put an error boundary around this function.
-- | Ensure that the current context is available in the environment before applying this function!
exitingRoleState :: RoleInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
exitingRoleState roleId userRoleType stateId = do
  -- Recur. We do this first, because we have to exit the deepest nested substate first.
  getActiveSubstate stateId roleId >>= void <<< traverse (exitingRoleState roleId userRoleType)
  -- Add the state identifier to the path of states in the context instance, triggering query updates
  -- just before running the current Transaction is finished.
  setInActiveRoleState stateId roleId
  {automaticOnExit} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction.
  forWithIndex_ automaticOnExit \allowedUser updater ->  (lift2 $ specialisesRoleType_ userRoleType allowedUser) >>= if _
    then updater roleId
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
          -- TODO. Dit is niet zeker. Moeten we niet hier de externe rol van de context van de roleId geven?
          , binding: Just $ unwrap roleId})

-- | Check all substates until one of them is found for which conditionSatisfied holds;
findSatisfiedSubstate :: StateIdentifier -> RoleInstance -> MonadPerspectivesTransaction (Maybe StateIdentifier)
findSatisfiedSubstate stateId roleId = (lift2 $ subStates_ stateId) >>= findM (conditionSatisfied roleId)

conditionSatisfied :: RoleInstance -> StateIdentifier -> MonadPerspectivesTransaction Boolean
conditionSatisfied roleId stateId = do
  compiledState <- getCompiledState stateId
  (Tuple bools a0 :: WithAssumptions Value) <- lift $ lift $ runMonadPerspectivesQuery roleId compiledState.query
  pure $ (not null bools) && (alaF Conj foldMap (eq (Value "true")) bools)

getCompiledState :: StateIdentifier -> MonadPerspectivesTransaction CompiledRoleState
getCompiledState stateId = case retrieveCompiledRoleState stateId of
  Nothing -> lift2 $ compileState stateId
  Just c -> pure c

isActive :: StateIdentifier -> RoleInstance -> MonadPerspectives Boolean
isActive stateId roleId = getActiveRoleStates_ roleId >>= pure <<< isJust <<< elemIndex stateId

-- isEntering :: State -> ContextInstance -> Boolean
-- isExiting :: State -> ContextInstance -> Boolean

getActiveSubstate :: StateIdentifier -> RoleInstance -> MonadPerspectivesTransaction (Maybe StateIdentifier)
getActiveSubstate stateId roleId = (lift $ lift $ getActiveRoleStates_ roleId) >>= \states -> pure $ join ((\i -> index states (i + 1)) <$> (elemIndex stateId states))
