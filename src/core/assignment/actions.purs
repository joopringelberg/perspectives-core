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
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (alaF, unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object (empty, values)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.ActionCache (LHS, cacheAction, retrieveAction)
import Perspectives.Assignment.DependencyTracking (areBotActionsSetUp, cacheActionInstanceDependencies, removeContextInstanceDependencies)
import Perspectives.Assignment.Update (moveRoles, removeRolFromContext)
import Perspectives.BasicConstructors (constructAnotherRol)
import Perspectives.ContextAndRole (changeContext_me, context_me, rol_isMe)
import Perspectives.CoreTypes (type (~~>), ActionInstance(..), MP, MonadPerspectives, Updater, WithAssumptions, MonadPerspectivesTransaction, runMonadPerspectivesQuery, (##=), (##>), (##>>))
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.ObjectGetters (roleType)
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, saveEntiteit_)
import Perspectives.Query.Compiler (context2context, context2propertyValue, context2role)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.Class.Action (condition, effect, isExecutedByBot)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (ActionType, getAction, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.SaveUserData (removeRoleInstance, saveRoleInstance)


-- | For a Context, set up the automtic Actions of the me role. Register these Actions in the ActionRegister.
-- | Register their dependency on Assumptions in the actionAssumptionRegister in
-- | PerspectivesState. Execute the actions once.
setupAndRunBotActions :: ContextInstance -> MonadPerspectives Unit
setupAndRunBotActions cid = areBotActionsSetUp cid >>= if _
  then pure unit
  else do
    -- For the me role, get its type.
    maybeMe <- getPerspectContext cid >>= pure <<< context_me
    case maybeMe of
      Nothing -> pure unit
      Just me -> do
        u <- me ##>> roleType
        void $ runMonadPerspectivesTransaction do
          perspectives <- lift $ lift $ getEnumeratedRole u >>= pure <<< _.perspectives <<< unwrap
          for_ (values perspectives) \actions ->
            for_ actions \(a :: ActionType) -> lift $ lift $ do
              -- Only when it is an automatic action
              shouldRun <- getAction a >>= pure <<< isExecutedByBot
              if shouldRun
                then (compileBotAction a) >>= \((Tuple _ updater) :: Tuple LHS (Updater ContextInstance)) -> void $ runMonadPerspectivesTransaction $ updater cid
                else pure unit

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
        u <- me ##>> roleType
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
        lift $ lift $ setupAndRunBotActions context
        removeRolFromContext context pspType roles
        for_ roles removeRoleInstance

compileAssignment (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextGetterDescription
  pure \contextId -> do
    ctxts <- lift $ lift (contextId ##= contextGetter)
    for_ ctxts \ctxt -> do
      role <- (lift $ lift $ constructAnotherRol qualifiedRoleIdentifier (unwrap ctxt) (RolSerialization {properties: PropertySerialization empty, binding: Nothing}))
      if rol_isMe role
        then lift $ lift $ do
          c <- getPerspectEntiteit ctxt
          void $ saveEntiteit_ ctxt (changeContext_me c (Just (identifier role)))
        else pure unit
      saveRoleInstance (identifier role)

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
          moveRoles context pspType roles
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
              moveRoles context pspType roles

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
        saveRoleInstance (identifier role)
        if rol_isMe role
          then lift $ lift $ do
            c <- getPerspectEntiteit ctxt
            void $ saveEntiteit_ ctxt (changeContext_me c (Just (identifier role)))
          else pure unit


compileAssignment (BQD _ (QF.BinaryCombinator SequenceF) _ _ _ _ _) = pure \_ -> pure unit

-- Vergeet EffectFullFunction niet!

-- Catchall, remove when all cases have been covered.
compileAssignment _ = pure \_ -> pure unit

-----------------------------------------------------------
-- CONSTRUCTACTIONFUNCTION
-----------------------------------------------------------
type RHS = WithAssumptions Value -> MonadPerspectivesTransaction Unit

{-
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
    pure $ f rt valueComputer removeRolFromContext
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
-}
