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

import Control.Monad.AvarMonadAsk (modify, gets)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldMap, null, uncons, unsafeIndex)
import Data.Array.NonEmpty (fromArray, head)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF, over, unwrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.Actions (compileAssignment)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..), defaultContextSerializationRecord)
import Perspectives.Assignment.ActionCache (cacheAction, retrieveAction)
import Perspectives.Assignment.StateCache (cacheState, retrieveState)
import Perspectives.Assignment.Update (addProperty, deleteProperty, moveRoleInstancesToAnotherContext, removeProperty, setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (type (~~>), MP, MPT, Updater, WithAssumptions, MonadPerspectivesTransaction, runMonadPerspectivesQuery, (##=), (##>), (##>>))
import Perspectives.Error.Boundaries (handlePerspectRolError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionNArgs, lookupHiddenFunction)
import Perspectives.Guid (guid)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (allRoleBinders, getRoleBinders) as OG
import Perspectives.Instances.ObjectGetters (getConditionState, roleType_, setConditionState)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getPerspectEntiteit, getPerspectRol)
import Perspectives.PerspectivesState (addBinding, getVariableBindings, pushFrame, restoreFrame)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..), Calculation(..))
import Perspectives.Query.UnsafeCompiler (compileFunction, context2context, context2propertyValue, context2role, context2string, roleFunctionFromQfd)
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Action (condition, effect, objectQfd, subject)
import Perspectives.Representation.Class.Identifiable (displayName)
import Perspectives.Representation.Class.PersistentType (ActionType, getPerspectType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.SideEffect (SideEffect(..))
import Perspectives.Representation.State (State(..), StateIdentifier)
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.SaveUserData (removeAllRoleInstances, handleNewPeer, removeRoleInstance, setBinding, removeBinding)
import Perspectives.Sync.Transaction (Transaction(..))
import Unsafe.Coerce (unsafeCoerce)

-- Put an error boundary around this function.
compileState :: StateIdentifier -> MP (Updater ContextInstance)
compileState stateId = do
  -- Try to get the (compiled) state out of cache.
  case retrieveState stateId of
    (Just a) -> pure a
    Nothing -> do
      State {context, query, object, automaticOnEntry} <- getState stateId
      (mobjectGetter :: Maybe (ContextInstance ~~> RoleInstance)) <- traverse
        -- we know the queryfunction description is there, by now, hence unsafePartial.
        (unsafePartial case _ of
          Q calc -> roleFunctionFromQfd calc) object
      (automaticOnEntry' :: Map RoleType (Updater ContextInstance)) <- traverseWithIndex
        (\_ (effect :: SideEffect) ->
          unsafePartial case effect of
            EF qfd -> compileAssignment qfd)
        (unwrap automaticOnEntry)
      -- TODO automaticOnExit, notifyOnEntry, notifyOnExit.

      -- We postpone compiling substates until they're asked for.
      (lhs :: (ContextInstance ~~> Value)) <- context2propertyValue $ unsafePartial case query of Q qfd -> qfd
      updater <- pure $ stateRunner lhs automaticOnEntry' mobjectGetter
      void $ pure $ cacheState stateId updater
      pure updater
  where
    stateRunner ::
      (ContextInstance ~~> Value) ->
      (Map RoleType (Updater ContextInstance)) ->
      (Maybe (ContextInstance ~~> RoleInstance)) ->
      (Updater ContextInstance)
    stateRunner lhs aOnEntry mobjectGetter contextId = do
      oldEnvironment <- lift2 pushFrame
      lift2 $ addBinding "currentcontext" [unwrap contextId]
