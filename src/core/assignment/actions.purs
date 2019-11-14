-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Actions where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect.
import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldMap, union)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF, unwrap)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign.Object (empty)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.ActionCache (cacheAction, retrieveAction)
import Perspectives.Assignment.DependencyTracking (ActionInstance(..), cacheActionInstanceDependencies, removeContextInstanceDependencies)
import Perspectives.Assignment.Update (PropertyUpdater, RoleUpdater, addProperty, addRol, deleteProperty, deleteRol, removeProperty, removeRol, setProperty, setRol)
import Perspectives.BasicConstructors (constructAnotherRol)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction, RoleGetter, Updater, WithAssumptions, ContextPropertyValueGetter, runMonadPerspectivesQuery, (##>>), (##=), MP)
import Perspectives.Instances.ObjectGetters (contextType)
import Perspectives.Query.Compiler (context2context, context2propertyValue, context2role)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Assignment (AssignmentStatement(..))
import Perspectives.Representation.Class.Action (condition, effect)
import Perspectives.Representation.Class.PersistentType (ActionType, getPerspectType)
import Perspectives.Representation.Context (Context, actions)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType)

-----------------------------------------------------------
-- CONSTRUCTACTIONFUNCTION
-----------------------------------------------------------
type RHS = WithAssumptions Value -> MonadPerspectivesTransaction Unit

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect for a Context, conditional on a set of given boolean values.
constructRHS :: RoleGetter -> ActionType -> AssignmentStatement -> MonadPerspectives (ContextInstance -> RHS)
constructRHS objectGetter actionType a = case a of
  (SetRol rt (query :: QueryFunctionDescription)) -> do
    (valueComputer :: RoleGetter) <- context2role query
    pure $ f rt valueComputer setRol
  (AddToRol rt (query :: QueryFunctionDescription)) -> do
    valueComputer <- context2role query
    pure $ f rt valueComputer addRol
  (RemoveFromRol rt (query :: QueryFunctionDescription)) -> do
    valueComputer <- context2role query
    pure $ f rt valueComputer removeRol
  (DeleteRol rt) -> pure $ f' rt
  (SetProperty pt (query :: QueryFunctionDescription)) -> do
    (valueComputer :: ContextPropertyValueGetter) <- context2propertyValue query
    pure $ g pt valueComputer setProperty
  (AddToProperty pt (query :: QueryFunctionDescription)) -> do
    (valueComputer :: ContextPropertyValueGetter) <- context2propertyValue query
    pure $ g pt valueComputer addProperty
  (RemoveFromProperty pt (query :: QueryFunctionDescription)) -> do
    (valueComputer :: ContextPropertyValueGetter) <- context2propertyValue query
    pure $ g pt valueComputer removeProperty
  (DeleteProperty pt) -> pure $ g' pt
  (EffectFullFunction fun args) -> case fun of
    -- TODO: vervang dit zodra de parser weer werkt.
    "storeDomeinFile" -> pure \contextInstance bools -> pure unit
    otherwise -> throwError (error ("Unknown EffectFullFunction in constructRHS: " <> show a))

  where
    f :: EnumeratedRoleType -> RoleGetter -> RoleUpdater -> (ContextInstance -> RHS)
    f rt roleGetter roleUpdater contextId (Tuple bools a0 :: WithAssumptions Value) = if (alaF Conj foldMap (eq (Value "true")) bools)
        then do
          (Tuple value a1 :: WithAssumptions RoleInstance) <- lift $ lift $ runMonadPerspectivesQuery contextId roleGetter
          -- The Object of the Action (where the bot is the Subject).
          -- We compute it here just for the assumptions.
          (Tuple object a2 :: WithAssumptions RoleInstance) <- lift $ lift $ runMonadPerspectivesQuery contextId objectGetter
          -- Cache the association between the assumptions found for this ActionInstance.
          pure $ cacheActionInstanceDependencies (ActionInstance contextId actionType) (union a0 (union a1 a2))
          void $ pure $ roleUpdater contextId rt value
        else pure unit

    -- Delete an entire role.
    f' :: EnumeratedRoleType -> (ContextInstance -> RHS)
    f' rt contextId (Tuple bools a0 :: WithAssumptions Value) = if (alaF Conj foldMap (eq (Value "true")) bools)
        then do
          -- The Object of the Action (where the bot is the Subject).
          -- We compute it here just for the assumptions.
          (Tuple object a2 :: WithAssumptions RoleInstance) <- lift $ lift $ runMonadPerspectivesQuery contextId objectGetter
          -- Cache the association between the assumptions found for this ActionInstance.
          pure $ cacheActionInstanceDependencies (ActionInstance contextId actionType) (union a0 a2)
          void $ pure $ deleteRol contextId rt
        else pure unit

    g :: EnumeratedPropertyType -> ContextPropertyValueGetter -> PropertyUpdater -> (ContextInstance -> RHS)
    g rt valueGetter propertyUpdater contextId (Tuple bools a0 :: WithAssumptions Value) = if (alaF Conj foldMap (eq (Value "true")) bools)
        then do
          (Tuple value a1 :: WithAssumptions Value) <- lift $ lift $ runMonadPerspectivesQuery contextId valueGetter
          -- The Object of the Action (where the bot is the Subject).
          (Tuple object a2 :: WithAssumptions RoleInstance) <- lift $ lift $ runMonadPerspectivesQuery contextId objectGetter
          -- Cache the association between the assumptions found for this ActionInstance.
          pure $ cacheActionInstanceDependencies (ActionInstance contextId actionType) (union a0 (union a1 a2))
          void $ pure $ propertyUpdater object rt value
        else pure unit

    -- delete an entire property
    g' :: EnumeratedPropertyType -> (ContextInstance -> RHS)
    g' rt contextId (Tuple bools a0 :: WithAssumptions Value) = if (alaF Conj foldMap (eq (Value "true")) bools)
        then do
          -- The Object of the Action (where the bot is the Subject).
          (Tuple object a2 :: WithAssumptions RoleInstance) <- lift $ lift $ runMonadPerspectivesQuery contextId objectGetter
          -- Cache the association between the assumptions found for this ActionInstance.
          pure $ cacheActionInstanceDependencies (ActionInstance contextId actionType) (union a0 a2)
          void $ pure $ deleteProperty object rt
        else pure unit

-- | For a Context, set up its Actions. Register these Actions in the ActionRegister. Register their dependency
-- | on Assumptions in the actionAssumptionRegister in PerspectivesState.
setupBotActions :: ContextInstance -> MonadPerspectives Unit
setupBotActions cid = do
  -- TODO: filter, keeping just those actions that are to be executed by a Bot.
  (ct :: ContextType) <- cid ##>> contextType
  (actions :: Array ActionType) <- (getPerspectType ct :: MonadPerspectives Context) >>= pure <<< actions
  -- Run the updater once. It will collect the Assumptions it depends on and register itself in the
  -- actionAssumptionRegister in PerspectivesState.
  for_ actions \(a :: ActionType) -> (compileBotAction a) >>= \(updater :: Updater ContextInstance) -> pure $ updater cid

-- | Remove all actions associated with this context.
tearDownBotActions :: ContextInstance -> MonadPerspectives Unit
tearDownBotActions = pure <<< removeContextInstanceDependencies

-- | Compile the action to an Updater. Cache for later use.
compileBotAction :: ActionType -> MP (Updater ContextInstance)
compileBotAction actionType =
  case retrieveAction actionType of
    (Just a) -> pure a
    Nothing -> do
      (action :: Action) <- getPerspectType actionType
      eff <- effect action
      (effectFullFunction :: Updater ContextInstance) <- compileAssignment eff
      (lhs :: (ContextInstance ~~> Value)) <- condition action >>= context2propertyValue
      updater <- pure $ ruleRunner lhs effectFullFunction
      void $ pure $ cacheAction actionType updater
      pure updater

  where
    -- | Actual effectfull function for which we track dependencies. If one of them changes,
    -- | the function is executed again.
    ruleRunner :: (ContextInstance ~~> Value) ->
      (Updater ContextInstance) ->
      (Updater ContextInstance)
    ruleRunner lhs effectFullFunction (contextId :: ContextInstance) = do
      (Tuple bools a0 :: WithAssumptions Value) <- lift $ lift $ runMonadPerspectivesQuery contextId lhs
      if (alaF Conj foldMap (eq (Value "true")) bools)
          then do
            -- Cache the association between the assumptions found for this ActionInstance.
            pure $ cacheActionInstanceDependencies (ActionInstance contextId actionType) a0
            effectFullFunction contextId
          else pure unit

compileAssignment :: QueryFunctionDescription -> MP (Updater ContextInstance)
compileAssignment (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextGetterDescription
  pure \contextId -> do
    ctxts <- lift $ lift (contextId ##= contextGetter)
    for_ ctxts \ctxt -> constructAnotherRol qualifiedRoleIdentifier (unwrap ctxt) (RolSerialization{properties: PropertySerialization empty, binding: Nothing})

-- compileAssignment (BQD _ QF.Move roleToMove contextToMoveTo _) = do
--   (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextToMoveTo
--   (roleGetter :: (ContextInstance ~~> RoleInstance)) = context2role roleToMove

compileAssignment (BQD _ (QF.BinaryCombinator "sequence") _ _ _ ) = pure \_ -> pure unit

-- Vergeet EffectFullFunction niet!

-- Catchall, remove when all cases have been covered.
compileAssignment _ = pure \_ -> pure unit
