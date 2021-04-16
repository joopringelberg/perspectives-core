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

module Perspectives.StateCompiler where

-- | From the description of a state, compute a function that computes for an instance of its context type
-- | whether that state is in effect. If entering the state, run all entry effects for the current user.
-- | On exiting the state, run all exit effects.
-- | Notify the current user on entering and exiting if specified.
-- | Cache the computed function for the duration of the session.

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (elemIndex, foldMap, index, null)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF, unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (singleton)
import Partial.Unsafe (unsafePartial)
import Perspectives.Actions (compileAssignment)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.StateCache (CompiledState, cacheCompiledState, retrieveCompiledState)
import Perspectives.Assignment.Update (setActive, setInActive)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (type (~~>), MP, MonadPerspectivesTransaction, Updater, WithAssumptions, MonadPerspectives, runMonadPerspectivesQuery)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (getActiveStates_)
import Perspectives.Names (getMySystem)
import Perspectives.PerspectivesState (addBinding, pushFrame)
import Perspectives.Query.QueryTypes (Calculation(..))
import Perspectives.Query.UnsafeCompiler (context2propertyValue, roleFunctionFromQfd)
import Perspectives.Representation.Class.PersistentType (getState)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType, StateIdentifier)
import Perspectives.Types.ObjectGetters (specialisesRoleType_, subStates_)
import Perspectives.Utilities (findM)

-- Put an error boundary around this function.
compileState :: StateIdentifier -> MP CompiledState
compileState stateId = do
    State {context, query, object, automaticOnEntry, automaticOnExit} <- getState stateId
    (mobjectGetter :: Maybe (ContextInstance ~~> RoleInstance)) <- traverse
      -- we know the queryfunction description is there, by now, hence unsafePartial.
      (unsafePartial case _ of
        Q calc -> roleFunctionFromQfd calc) object
    (automaticOnEntry' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
      (\_ (effect :: SideEffect) ->
        unsafePartial case effect of
          EF qfd -> compileAssignment qfd)
      (unwrap automaticOnEntry)
    (automaticOnExit' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
      (\_ (effect :: SideEffect) ->
        unsafePartial case effect of
          EF qfd -> compileAssignment qfd)
      (unwrap automaticOnExit)
    -- TODO notifyOnEntry, notifyOnExit.

    -- We postpone compiling substates until they're asked for.
    (lhs :: (ContextInstance ~~> Value)) <- context2propertyValue $ unsafePartial case query of Q qfd -> qfd
    pure $ cacheCompiledState stateId
      { query: lhs
      , automaticOnEntry: automaticOnEntry'
      , automaticOnExit: automaticOnExit'
      }
  where
    stateRunner ::
      (ContextInstance ~~> Value) ->
      (Map RoleType (Updater ContextInstance)) ->
      (Maybe (ContextInstance ~~> RoleInstance)) ->
      (Updater ContextInstance)
    stateRunner lhs aOnEntry mobjectGetter contextId = do
      oldEnvironment <- lift2 pushFrame
      lift2 $ addBinding "currentcontext" [unwrap contextId]

evaluateState :: ContextInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
evaluateState contextId userRoleType stateId = do
  mactive <- getActiveSubstate stateId contextId
  case mactive of
    Nothing -> findSatisfiedSubstate stateId contextId >>= case _ of
      Nothing -> pure unit
      Just sub -> enteringState contextId userRoleType sub
    Just sub -> conditionSatisfied contextId stateId >>= if _
      then evaluateState contextId userRoleType sub
      else do
        exitingState contextId userRoleType sub
        findSatisfiedSubstate stateId contextId >>= (void <<< traverse (enteringState contextId userRoleType))

-- | On entering a state, we register that state with the context instance and trigger client query updates.
-- | We run all automatic actions and create notifications.
-- | Finally, we look for the substate whose query returns true and apply `enteringState` to it.
enteringState :: ContextInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
enteringState contextId userRoleType stateId = do
  -- Add the state identifier to the path of states in the context instance, triggering query updates
  -- when the current Transaction is run.
  setActive stateId contextId
  {automaticOnEntry} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction.
  forWithIndex_ automaticOnEntry \allowedUser updater ->  (lift2 $ specialisesRoleType_ userRoleType allowedUser) >>= if _
    then updater contextId
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
  findSatisfiedSubstate stateId contextId >>= void <<< traverse (enteringState contextId userRoleType)

-- | On exiting a state, we de-register that state with the context instance and trigger client query updates.
-- | We run all automatic actions and create notifications.
-- | Finally, we find the substate that is still active (if any) and apply `exitingState` to it.
exitingState :: ContextInstance -> RoleType -> StateIdentifier -> MonadPerspectivesTransaction Unit
exitingState contextId userRoleType stateId = do
  -- Recur. We do this first, because we have to exit the deepest nested substate first.
  getActiveSubstate stateId contextId >>= void <<< traverse (exitingState contextId userRoleType)
  -- Add the state identifier to the path of states in the context instance, triggering query updates
  -- when the current Transaction is run.
  setInActive stateId contextId
  {automaticOnExit} <- getCompiledState stateId
  -- Run automatic actions in the current Transaction.
  forWithIndex_ automaticOnExit \allowedUser updater ->  (lift2 $ specialisesRoleType_ userRoleType allowedUser) >>= if _
    then updater contextId
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

-- | Check all substates until one of them is found for which conditionSatisfied holds;
findSatisfiedSubstate :: StateIdentifier -> ContextInstance -> MonadPerspectivesTransaction (Maybe StateIdentifier)
findSatisfiedSubstate stateId contextId = (lift2 $ subStates_ stateId) >>= findM (conditionSatisfied contextId)

conditionSatisfied :: ContextInstance -> StateIdentifier -> MonadPerspectivesTransaction Boolean
conditionSatisfied contextId stateId = do
  compiledState <- getCompiledState stateId
  (Tuple bools a0 :: WithAssumptions Value) <- lift $ lift $ runMonadPerspectivesQuery contextId compiledState.query
  pure $ (not null bools) && (alaF Conj foldMap (eq (Value "true")) bools)

getCompiledState :: StateIdentifier -> MonadPerspectivesTransaction CompiledState
getCompiledState stateId = case retrieveCompiledState stateId of
  Nothing -> lift2 $ compileState stateId
  Just c -> pure c

isActive :: StateIdentifier -> ContextInstance -> MonadPerspectives Boolean
isActive stateId contextId = getActiveStates_ contextId >>= pure <<< isJust <<< elemIndex stateId

-- isEntering :: State -> ContextInstance -> Boolean
-- isExiting :: State -> ContextInstance -> Boolean

getActiveSubstate :: StateIdentifier -> ContextInstance -> MonadPerspectivesTransaction (Maybe StateIdentifier)
getActiveSubstate stateId contextId = (lift $ lift $ getActiveStates_ contextId) >>= \states -> pure $ join ((\i -> index states (i + 1)) <$> (elemIndex stateId states))
