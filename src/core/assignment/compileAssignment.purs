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

module Perspectives.CompileAssignment where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect.
import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (uncons)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign.Object (empty)
-- import Perspectives.Actions (setupBotActions)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (moveRoles, removeRol)
import Perspectives.BasicConstructors (constructAnotherRol)
import Perspectives.CoreTypes (type (~~>), MonadPerspectivesTransaction, Updater, WithAssumptions, (##>>), (##=), MP, (##>))
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Persistent (getPerspectEntiteit)
import Perspectives.Query.Compiler (context2context, context2role)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.QueryFunction (FunctionName(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.SaveUserData (saveRoleInstance)


compileAssignment :: QueryFunctionDescription -> MP (Updater ContextInstance)
compileAssignment (UQD _ QF.Remove rle _ _ mry) = do
  roleGetter <- context2role rle
  pure \contextId -> do
    (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
    case uncons roles of
      Nothing -> pure unit
      Just {head, tail} -> do
        ((PerspectRol{context, pspType}) :: PerspectRol) <- lift $ lift $ getPerspectEntiteit head
        -- TODO: removeRoleInstance instead
        -- setupBotActions context
        removeRol context pspType roles

compileAssignment (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextGetterDescription
  pure \contextId -> do
    ctxts <- lift $ lift (contextId ##= contextGetter)
    for_ ctxts \ctxt -> (lift $ lift $ constructAnotherRol qualifiedRoleIdentifier (unwrap ctxt) (RolSerialization {properties: PropertySerialization empty, binding: Nothing})) >>= saveRoleInstance

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
        (lift $ lift $ constructAnotherRol qualifiedRoleIdentifier (unwrap ctxt)
          (RolSerialization{ properties: PropertySerialization empty, binding: Just (unwrap bndg)})) >>= saveRoleInstance

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
-}
