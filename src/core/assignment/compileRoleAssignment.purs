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

module Perspectives.CompileRoleAssignment where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect, **starting with a Role instance**.

import Prelude

import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (unsafeIndex)
import Data.Array.NonEmpty (fromArray, head)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..), defaultContextSerializationRecord)
import Perspectives.Assignment.Update (addProperty, deleteProperty, moveRoleInstancesToAnotherContext, removeProperty, setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (type (~~>), MP, MPT, Updater, (##=), (##>), (##>>))
import Perspectives.Error.Boundaries (handlePerspectRolError)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionNArgs, lookupHiddenFunction)
import Perspectives.Guid (guid)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (allRoleBinders, getRoleBinders) as OG
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.Persistent (getPerspectEntiteit, getPerspectRol)
import Perspectives.PerspectivesState (addBinding, getVariableBindings)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Query.UnsafeCompiler (compileFunction, role2context, role2propertyValue, role2role, role2string)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.SaveUserData (removeAllRoleInstances, handleNewPeer, removeRoleInstance, setBinding, removeBinding)
import Unsafe.Coerce (unsafeCoerce)

-- Put an error boundary around this function.
compileAssignmentFromRole :: QueryFunctionDescription -> MP (Updater RoleInstance)
compileAssignmentFromRole (UQD _ QF.Remove rle _ _ mry) = do
  roleGetter <- role2role rle
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
    for_ roles removeRoleInstance

-- Delete all instances of the role.
compileAssignmentFromRole (UQD _ (QF.DeleteRole qualifiedRoleIdentifier) contextsToDeleteFrom _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextsToDeleteFrom
  pure \roleId -> do
    ctxts <- lift $ lift (roleId ##= contextGetter)
    for_ ctxts \ctxt -> do
      removeAllRoleInstances qualifiedRoleIdentifier ctxt

compileAssignmentFromRole (UQD _ (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \(roleId :: RoleInstance) -> do
    ctxts <- lift2 (roleId ##= contextGetter)
    for_ ctxts \ctxt -> do
      -- TODO. Breid qualifiedRoleIdentifier uit naar RoleType: nu hanteren we alleen EnumeratedRoleType.
      g <- liftEffect guid
      newContext <- runExceptT $ constructContext (Just $ ENR qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
        { id = "model:User$c" <> (show g)
        , ctype = unwrap qualifiedContextTypeIdentifier
        })
      void $ createAndAddRoleInstance qualifiedRoleIdentifier (unwrap roleId) (RolSerialization
        { id: Nothing
        , properties: PropertySerialization empty
        , binding: Just $ buitenRol $ "model:User$c" <> (show g) })

compileAssignmentFromRole (UQD _ (QF.CreateContext_ qualifiedContextTypeIdentifier) roleGetterDescription _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleGetterDescription
  pure \(roleId :: RoleInstance) -> do
    roles <- lift2 (roleId ##= roleGetter)
    for_ roles \roleInstance -> do
      -- TODO. Breid qualifiedRoleIdentifier uit naar RoleType: nu hanteren we alleen EnumeratedRoleType.
      rtype <- lift2 $ roleType_ roleInstance
      g <- liftEffect guid
      newContextId <- pure $ "model:User$c" <> (show g)
      newContext <- runExceptT $ constructContext (Just $ ENR rtype) (ContextSerialization defaultContextSerializationRecord
        { id = newContextId
        , ctype = unwrap qualifiedContextTypeIdentifier
        })
      -- now bind it in the role instance.
      void $ setBinding roleInstance (RoleInstance $ buitenRol newContextId) Nothing
      handleNewPeer (RoleInstance newContextId)

compileAssignmentFromRole (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \roleId -> do
    ctxts <- lift2 (roleId ##= contextGetter)
    for_ ctxts \ctxt -> do
      roleIdentifier <- unsafePartial $ fromJust <$> createAndAddRoleInstance qualifiedRoleIdentifier (unwrap ctxt) (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})
      -- No need to handle retrieval errors as we've just created the role.
      lift2 $ getPerspectRol roleIdentifier

compileAssignmentFromRole (BQD _ QF.Move roleToMove contextToMoveTo _ _ mry) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextToMoveTo
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleToMove
  if (pessimistic mry)
    then pure \roleId -> do
      c <- lift $ lift (roleId ##>> contextGetter)
      (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
      case fromArray roles of
        Nothing -> pure unit
        Just roles' ->  try (lift $ lift $ getPerspectEntiteit (head roles')) >>=
          handlePerspectRolError "compileAssignmentFromRole, Move"
            (\((PerspectRol{context, pspType}) :: PerspectRol) -> moveRoleInstancesToAnotherContext context c pspType roles')

    else pure \roleId -> do
      ctxt <- lift $ lift (roleId ##> contextGetter)
      case ctxt of
        Nothing -> pure unit
        Just c -> do
          (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
          case fromArray roles of
            Nothing -> pure unit
            Just roles' ->  try (lift $ lift $ getPerspectEntiteit (head roles')) >>=
              handlePerspectRolError "compileAssignmentFromRole, Move"
                (\((PerspectRol{context, pspType}) :: PerspectRol) -> moveRoleInstancesToAnotherContext context c pspType roles')

compileAssignmentFromRole (BQD _ (QF.Bind qualifiedRoleIdentifier) bindings contextToBindIn _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextToBindIn
  (bindingsGetter :: (RoleInstance ~~> RoleInstance)) <- role2role bindings
  pure \roleId -> do
    ctxts <- lift2 (roleId ##= contextGetter)
    (bindings' :: Array RoleInstance) <- lift2 (roleId ##= bindingsGetter)
    for_ ctxts \ctxt -> do
      for_ bindings' \bndg -> createAndAddRoleInstance
        qualifiedRoleIdentifier
        (unwrap ctxt)
        (RolSerialization{id: Nothing, properties: PropertySerialization empty, binding: Just (unwrap bndg)})

compileAssignmentFromRole (BQD _ QF.Bind_ binding binder _ _ _) = do
  (bindingGetter :: (RoleInstance ~~> RoleInstance)) <- role2role binding
  (binderGetter :: (RoleInstance ~~> RoleInstance)) <- role2role binder
  pure \roleId -> do
    (binding' :: Maybe RoleInstance) <- lift $ lift (roleId ##> bindingGetter)
    (binder' :: Maybe RoleInstance) <- lift $ lift (roleId ##> binderGetter)
    -- setBinding caches, saves, sets isMe and me.
    void $ case binding' of
      Nothing -> pure []
      Just binding'' -> case binder' of
        Nothing -> pure []
        Just binder'' -> do
          setBinding binder'' binding'' Nothing <* handleNewPeer binder''

compileAssignmentFromRole (UQD _ (QF.Unbind mroleType) bindings _ _ _) = do
  (bindingsGetter :: (RoleInstance ~~> RoleInstance)) <- role2role bindings
  case mroleType of
    Nothing -> pure
      \roleId -> do
        binders <- lift $ lift (roleId ##= bindingsGetter >=> OG.allRoleBinders)
        for_ binders (removeBinding false)
    Just roleType -> pure
      \roleId -> do
        binders <- lift $ lift (roleId ##= bindingsGetter >=> OG.getRoleBinders roleType)
        for_ binders (removeBinding false)

compileAssignmentFromRole (BQD _ QF.Unbind_ bindings binders _ _ _) = do
  (bindingsGetter :: (RoleInstance ~~> RoleInstance)) <- role2role bindings
  (bindersGetter :: (RoleInstance ~~> RoleInstance)) <- role2role binders
  pure \roleId -> do
    (binding :: Maybe RoleInstance) <- lift $ lift (roleId ##> bindingsGetter)
    (binder :: Maybe RoleInstance) <- lift $ lift (roleId ##> bindersGetter)
    -- TODO. As soon as we introduce multiple values for a binding, we have to adapt this so the binding argument
    -- is taken into account, too.
    void $ case binder of
      Nothing -> pure []
      Just binder' -> removeBinding false binder'

compileAssignmentFromRole (UQD _ (QF.DeleteProperty qualifiedProperty) roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
    deleteProperty roles qualifiedProperty

compileAssignmentFromRole (BQD _ (QF.RemovePropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  (valueGetter :: (RoleInstance ~~> Value)) <- role2propertyValue valueQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
    (values :: Array Value) <- lift $ lift (roleId ##= valueGetter)
    removeProperty roles qualifiedProperty values

compileAssignmentFromRole (BQD _ (QF.AddPropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  (valueGetter :: (RoleInstance ~~> Value)) <- role2propertyValue valueQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
    (values :: Array Value) <- lift $ lift (roleId ##= valueGetter)
    addProperty roles qualifiedProperty (flip Tuple Nothing <$> values)

compileAssignmentFromRole (BQD _ (QF.SetPropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  (valueGetter :: (RoleInstance ~~> Value)) <- role2propertyValue valueQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
    (values :: Array Value) <- lift $ lift (roleId ##= valueGetter)
    setProperty roles qualifiedProperty values

-- Even though SequenceF is compiled in the QueryCompiler, we need to handle it here, too.
-- In the QueryCompiler, the components will be variable bindings.
-- Here they will be assignments.
compileAssignmentFromRole (BQD _ (BinaryCombinator SequenceF) f1 f2 _ _ _) = do
  f1' <- compileAssignmentFromRole f1
  f2' <- compileAssignmentFromRole f2
  pure \c -> (f1' c *> f2' c)

compileAssignmentFromRole (UQD _ WithFrame f1 _ _ _) = do
  f1' <- compileAssignmentFromRole f1
  pure \c -> do
    old <- lift $ lift $ getVariableBindings
    void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
    r <- f1' c
    void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = old}
    pure unit

compileAssignmentFromRole (UQD _ (BindVariable varName) f1 _ _ _) = do
  f1' <- role2string f1
  pure \roleId -> do
    v <- lift $ lift (roleId ##= f1')
    lift $ lift $ addBinding varName (unsafeCoerce v)
    pure unit

compileAssignmentFromRole (MQD dom (ExternalEffectFullFunction functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions :: Array (RoleInstance ~~> String)) <- traverse (unsafeCoerce compileFunction) args
  pure (\c -> do
    (values :: Array (Array String)) <- lift $ lift $ traverse (\g -> c ##= g) argFunctions
    case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
      0 -> (unsafeCoerce f :: RoleInstance -> MPT Unit) c
      1 -> (unsafeCoerce f :: (Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        c
      2 -> (unsafeCoerce f :: (Array String -> Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        c
      3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        c
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        c
      _ -> throwError (error "Too many arguments for external core module: maximum is 4")
    )

-- Catchall, remove when all cases have been covered.
compileAssignmentFromRole otherwise = throwError (error ("Found unknown case for compileAssignmentFromRole: " <> show otherwise))
