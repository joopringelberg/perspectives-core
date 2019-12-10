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

import Control.Monad.Trans.Class (lift)
import Data.Array (foldMap, uncons)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF, unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object (empty, values)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.ActionCache (LHS, cacheAction, retrieveAction)
import Perspectives.Assignment.DependencyTracking (areBotActionsSetUp, cacheActionInstanceDependencies, removeContextInstanceDependencies)
import Perspectives.Assignment.Update (addProperty, deleteProperty, moveRoles, removeBinding, removeProperty, removeRolFromContext, setBinding, setProperty)
import Perspectives.BasicConstructors (constructAnotherRol)
import Perspectives.ContextAndRole (context_me, rol_isMe)
import Perspectives.CoreTypes (type (~~>), ActionInstance(..), MP, MonadPerspectives, Updater, WithAssumptions, runMonadPerspectivesQuery, (##=), (##>), (##>>))
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.ObjectGetters (allRoleBinders, getRoleBinders, roleType, context) as OG
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, saveEntiteit_)
import Perspectives.Query.Compiler (context2context, context2propertyValue, context2role)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Action (condition, effect, isExecutedByBot)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (ActionType, getAction, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.SaveUserData (removeRoleInstance, saveAndConnectRoleInstance)

-- | For a Context, set up the automtic Actions of the me role. Register these Actions in the ActionRegister.
-- | Register their dependency on Assumptions in the actionAssumptionRegister in
-- | PerspectivesState. Does **not** execute the actions.
setupBotActions :: ContextInstance -> MonadPerspectives Unit
setupBotActions cid = areBotActionsSetUp cid >>= if _
  then pure unit
  else do
    -- For the me role, get its type.
    maybeMe <- getPerspectContext cid >>= pure <<< context_me
    case maybeMe of
      Nothing -> pure unit
      Just me -> do
        u <- me ##>> OG.roleType
        perspectives <- getEnumeratedRole u >>= pure <<< _.perspectives <<< unwrap
        for_ (values perspectives) \actions ->
          for_ actions \(a :: ActionType) -> do
            -- Only when it is an automatic action
            shouldRun <- getAction a >>= pure <<< isExecutedByBot
            if shouldRun
              then (compileBotAction a) >>= \((Tuple lhs _) :: Tuple LHS (Updater ContextInstance)) -> do
                  -- Evaluate the lhs to set up the dependency administration.
                  (Tuple bools ass :: WithAssumptions Value) <- runMonadPerspectivesQuery cid lhs
                  cacheActionInstanceDependencies (ActionInstance cid a) ass
              else pure unit

-- | Remove all actions associated with this context.
tearDownBotActions :: ContextInstance -> MonadPerspectives Unit
tearDownBotActions = removeContextInstanceDependencies

-- | Compile the action to an Updater. Cache for later use.
compileBotAction :: ActionType -> MP (Tuple LHS (Updater ContextInstance))
compileBotAction actionType = do
  case retrieveAction actionType of
    (Just a) -> pure a
    Nothing -> do
      (action :: Action) <- getPerspectType actionType
      eff <- effect action
      (effectFullFunction :: Updater ContextInstance) <- compileAssignment eff
      (lhs :: (ContextInstance ~~> Value)) <- condition action >>= context2propertyValue
      updater <- pure $ ruleRunner lhs effectFullFunction
      void $ pure $ cacheAction actionType (Tuple lhs updater)
      pure $ Tuple lhs updater

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
            lift $ lift $ cacheActionInstanceDependencies (ActionInstance contextId actionType) a0
            effectFullFunction contextId
          else pure unit

compileAssignment :: QueryFunctionDescription -> MP (Updater ContextInstance)
compileAssignment (UQD _ QF.Remove rle _ _ mry) = do
  roleGetter <- context2role rle
  pure \contextId -> do
    (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
    case uncons roles of
      Nothing -> pure unit
      Just {head, tail} -> do
        ((PerspectRol{context, pspType}) :: PerspectRol) <- lift $ lift $ getPerspectEntiteit head
        removeRolFromContext context pspType roles
        for_ roles removeRoleInstance

compileAssignment (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextGetterDescription
  pure \contextId -> do
    ctxts <- lift $ lift (contextId ##= contextGetter)
    for_ ctxts \ctxt -> do
      role <- (lift $ lift $ constructAnotherRol qualifiedRoleIdentifier (unwrap ctxt) (RolSerialization {properties: PropertySerialization empty, binding: Nothing}))
      -- save and add to context:
      saveAndConnectRoleInstance (identifier role)

compileAssignment (BQD _ QF.Move roleToMove contextToMoveTo _ _ mry) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextToMoveTo
  (roleGetter :: (ContextInstance ~~> RoleInstance)) <- context2role roleToMove
  if (pessimistic mry)
    then pure \contextId -> do
      c <- lift $ lift (contextId ##>> contextGetter)
      (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
      case uncons roles of
        Nothing -> pure unit
        Just {head, tail} -> do
          ((PerspectRol{context, pspType}) :: PerspectRol) <- lift $ lift $ getPerspectEntiteit head
          moveRoles context c pspType roles
    else pure \contextId -> do
      ctxt <- lift $ lift (contextId ##> contextGetter)
      case ctxt of
        Nothing -> pure unit
        Just c -> do
          (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
          case uncons roles of
            Nothing -> pure unit
            Just {head, tail} -> do
              ((PerspectRol{context, pspType}) :: PerspectRol) <- lift $ lift $ getPerspectEntiteit head
              moveRoles context c pspType roles

compileAssignment (BQD _ (QF.Bind qualifiedRoleIdentifier) bindings contextToBindIn _ _ _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextToBindIn
  (bindingsGetter :: (ContextInstance ~~> RoleInstance)) <- context2role bindings
  pure \contextId -> do
    ctxts <- lift $ lift (contextId ##= contextGetter)
    (bindings' :: Array RoleInstance) <- lift $ lift (contextId ##= bindingsGetter)
    -- TODO: handle errors when creating a new Role instance in Bind.
    for_ ctxts \ctxt -> do
      for_ bindings' \bndg -> do
        role <- (lift $ lift $ constructAnotherRol qualifiedRoleIdentifier (unwrap ctxt)
          (RolSerialization{ properties: PropertySerialization empty, binding: Just (unwrap bndg)}))
        saveAndConnectRoleInstance (identifier role)

compileAssignment (BQD _ QF.Bind_ bindings binders _ _ _) = do
  (bindingsGetter :: (ContextInstance ~~> RoleInstance)) <- context2role bindings
  (bindersGetter :: (ContextInstance ~~> RoleInstance)) <- context2role binders
  pure \contextId -> do
    (binding :: Maybe RoleInstance) <- lift $ lift (contextId ##> bindingsGetter)
    (binder :: Maybe RoleInstance) <- lift $ lift (contextId ##> bindersGetter)
    -- setBinding caches, saves, sets isMe and me.
    maybe (pure unit) identity (setBinding <$> binder <*> binding)

compileAssignment (UQD _ (QF.Unbind mroleType) bindings _ _ _) = do
  (bindingsGetter :: (ContextInstance ~~> RoleInstance)) <- context2role bindings
  case mroleType of
    Nothing -> pure
      \contextId -> do
        binders <- lift $ lift (contextId ##= bindingsGetter >=> OG.allRoleBinders)
        for_ binders removeBinding
    Just roleType -> pure
      \contextId -> do
        binders <- lift $ lift (contextId ##= bindingsGetter >=> OG.getRoleBinders roleType)
        for_ binders removeBinding

compileAssignment (BQD _ QF.Unbind_ bindings binders _ _ _) = do
  (bindingsGetter :: (ContextInstance ~~> RoleInstance)) <- context2role bindings
  (bindersGetter :: (ContextInstance ~~> RoleInstance)) <- context2role binders
  pure \contextId -> do
    (binding :: Maybe RoleInstance) <- lift $ lift (contextId ##> bindingsGetter)
    (binder :: Maybe RoleInstance) <- lift $ lift (contextId ##> bindersGetter)
    -- TODO. As soon as we introduce multiple values for a binding, we have to adapt this so the binding argument
    -- is taken into account, too.
    maybe (pure unit) identity (removeBinding <$> binder)

compileAssignment (UQD _ (QF.DeleteProperty qualifiedProperty) roleQfd _ _ _) = do
  (roleGetter :: (ContextInstance ~~> RoleInstance)) <- context2role roleQfd
  pure \contextId -> do
    (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
    deleteProperty roles qualifiedProperty

compileAssignment (BQD _ (QF.RemovePropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (ContextInstance ~~> RoleInstance)) <- context2role roleQfd
  (valueGetter :: (ContextInstance ~~> Value)) <- context2propertyValue valueQfd
  pure \contextId -> do
    (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
    (values :: Array Value) <- lift $ lift (contextId ##= valueGetter)
    removeProperty roles qualifiedProperty values

compileAssignment (BQD _ (QF.AddPropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (ContextInstance ~~> RoleInstance)) <- context2role roleQfd
  (valueGetter :: (ContextInstance ~~> Value)) <- context2propertyValue valueQfd
  pure \contextId -> do
    (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
    (values :: Array Value) <- lift $ lift (contextId ##= valueGetter)
    addProperty roles qualifiedProperty values

compileAssignment (BQD _ (QF.SetPropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (ContextInstance ~~> RoleInstance)) <- context2role roleQfd
  (valueGetter :: (ContextInstance ~~> Value)) <- context2propertyValue valueQfd
  pure \contextId -> do
    (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
    (values :: Array Value) <- lift $ lift (contextId ##= valueGetter)
    setProperty roles qualifiedProperty values

-- Even though SequenceF is compiled in the QueryCompiler, we need to handle it here, too.
-- In the QueryCompiler, the components will be variable bindings.
-- Here they will be assignments.
compileAssignment (BQD _ (BinaryCombinator SequenceF) f1 f2 _ _ _) = do
  f1' <- compileAssignment f1
  f2' <- compileAssignment f2
  pure \c -> (f1' c *> f2' c)

-- Vergeet EffectFullFunction niet!

-- Catchall, remove when all cases have been covered.
compileAssignment _ = pure \_ -> pure unit
