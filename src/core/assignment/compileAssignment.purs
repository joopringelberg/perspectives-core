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

module Perspectives.CompileAssignment where

-- | From the description of an assignment or effectful function, construct a function
-- | that actually assigns a value or sorts an effect.
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
import Perspectives.Query.UnsafeCompiler (compileFunction, context2context, context2propertyValue, context2role, context2string)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.SaveUserData (removeAllRoleInstances, handleNewPeer, removeRoleInstance, setBinding, removeBinding)
import Unsafe.Coerce (unsafeCoerce)

-- Put an error boundary around this function.
compileAssignment :: QueryFunctionDescription -> MP (Updater ContextInstance)
compileAssignment (UQD _ QF.Remove rle _ _ mry) = do
  roleGetter <- context2role rle
  pure \contextId -> do
    (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
    for_ roles removeRoleInstance

-- Delete all instances of the role. Model
compileAssignment (UQD _ (QF.DeleteRole qualifiedRoleIdentifier) contextsToDeleteFrom _ _ _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextsToDeleteFrom
  pure \contextId -> do
    ctxts <- lift $ lift (contextId ##= contextGetter)
    for_ ctxts \ctxt -> do
      removeAllRoleInstances qualifiedRoleIdentifier ctxt

compileAssignment (UQD _ (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextGetterDescription
  pure \(contextId :: ContextInstance) -> do
    ctxts <- lift2 (contextId ##= contextGetter)
    for_ ctxts \ctxt -> do
      -- TODO. Breid qualifiedRoleIdentifier uit naar RoleType: nu hanteren we alleen EnumeratedRoleType.
      g <- liftEffect guid
      newContext <- runExceptT $ constructContext (Just $ ENR qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
        { id = "model:User$c" <> (show g)
        , ctype = unwrap qualifiedContextTypeIdentifier
        })
      void $ createAndAddRoleInstance qualifiedRoleIdentifier (unwrap contextId) (RolSerialization
        { id: Nothing
        , properties: PropertySerialization empty
        , binding: Just $ buitenRol $ "model:User$c" <> (show g) })

compileAssignment (UQD _ (QF.CreateContext_ qualifiedContextTypeIdentifier) roleGetterDescription _ _ _) = do
  (roleGetter :: (ContextInstance ~~> RoleInstance)) <- context2role roleGetterDescription
  pure \(contextId :: ContextInstance) -> do
    roles <- lift2 (contextId ##= roleGetter)
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

compileAssignment (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextGetterDescription
  pure \contextId -> do
    ctxts <- lift2 (contextId ##= contextGetter)
    for_ ctxts \ctxt -> do
      roleIdentifier <- unsafePartial $ fromJust <$> createAndAddRoleInstance qualifiedRoleIdentifier (unwrap ctxt) (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})
      -- No need to handle retrieval errors as we've just created the role.
      lift2 $ getPerspectRol roleIdentifier

compileAssignment (BQD _ QF.Move roleToMove contextToMoveTo _ _ mry) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextToMoveTo
  (roleGetter :: (ContextInstance ~~> RoleInstance)) <- context2role roleToMove
  if (pessimistic mry)
    then pure \contextId -> do
      c <- lift $ lift (contextId ##>> contextGetter)
      (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
      case fromArray roles of
        Nothing -> pure unit
        Just roles' ->  try (lift $ lift $ getPerspectEntiteit (head roles')) >>=
          handlePerspectRolError "compileAssignment, Move"
            (\((PerspectRol{context, pspType}) :: PerspectRol) -> moveRoleInstancesToAnotherContext context c pspType roles')

    else pure \contextId -> do
      ctxt <- lift $ lift (contextId ##> contextGetter)
      case ctxt of
        Nothing -> pure unit
        Just c -> do
          (roles :: Array RoleInstance) <- lift $ lift (contextId ##= roleGetter)
          case fromArray roles of
            Nothing -> pure unit
            Just roles' ->  try (lift $ lift $ getPerspectEntiteit (head roles')) >>=
              handlePerspectRolError "compileAssignment, Move"
                (\((PerspectRol{context, pspType}) :: PerspectRol) -> moveRoleInstancesToAnotherContext context c pspType roles')

compileAssignment (BQD _ (QF.Bind qualifiedRoleIdentifier) bindings contextToBindIn _ _ _) = do
  (contextGetter :: (ContextInstance ~~> ContextInstance)) <- context2context contextToBindIn
  (bindingsGetter :: (ContextInstance ~~> RoleInstance)) <- context2role bindings
  pure \contextId -> do
    ctxts <- lift2 (contextId ##= contextGetter)
    (bindings' :: Array RoleInstance) <- lift2 (contextId ##= bindingsGetter)
    for_ ctxts \ctxt -> do
      for_ bindings' \bndg -> createAndAddRoleInstance
        qualifiedRoleIdentifier
        (unwrap ctxt)
        (RolSerialization{id: Nothing, properties: PropertySerialization empty, binding: Just (unwrap bndg)})

compileAssignment (BQD _ QF.Bind_ binding binder _ _ _) = do
  (bindingGetter :: (ContextInstance ~~> RoleInstance)) <- context2role binding
  (binderGetter :: (ContextInstance ~~> RoleInstance)) <- context2role binder
  pure \contextId -> do
    (binding' :: Maybe RoleInstance) <- lift $ lift (contextId ##> bindingGetter)
    (binder' :: Maybe RoleInstance) <- lift $ lift (contextId ##> binderGetter)
    -- setBinding caches, saves, sets isMe and me.
    void $ case binding' of
      Nothing -> pure []
      Just binding'' -> case binder' of
        Nothing -> pure []
        Just binder'' -> do
          setBinding binder'' binding'' Nothing <* handleNewPeer binder''

compileAssignment (UQD _ (QF.Unbind mroleType) bindings _ _ _) = do
  (bindingsGetter :: (ContextInstance ~~> RoleInstance)) <- context2role bindings
  case mroleType of
    Nothing -> pure
      \contextId -> do
        binders <- lift $ lift (contextId ##= bindingsGetter >=> OG.allRoleBinders)
        for_ binders (removeBinding false)
    Just roleType -> pure
      \contextId -> do
        binders <- lift $ lift (contextId ##= bindingsGetter >=> OG.getRoleBinders roleType)
        for_ binders (removeBinding false)

compileAssignment (BQD _ QF.Unbind_ bindings binders _ _ _) = do
  (bindingsGetter :: (ContextInstance ~~> RoleInstance)) <- context2role bindings
  (bindersGetter :: (ContextInstance ~~> RoleInstance)) <- context2role binders
  pure \contextId -> do
    (binding :: Maybe RoleInstance) <- lift $ lift (contextId ##> bindingsGetter)
    (binder :: Maybe RoleInstance) <- lift $ lift (contextId ##> bindersGetter)
    -- TODO. As soon as we introduce multiple values for a binding, we have to adapt this so the binding argument
    -- is taken into account, too.
    void $ case binder of
      Nothing -> pure []
      Just binder' -> removeBinding false binder'

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
    addProperty roles qualifiedProperty (flip Tuple Nothing <$> values)

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

compileAssignment (UQD _ WithFrame f1 _ _ _) = do
  f1' <- compileAssignment f1
  pure \c -> do
    old <- lift $ lift $ getVariableBindings
    void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
    r <- f1' c
    void $ lift $ lift $ modify \s@{variableBindings} -> s {variableBindings = old}
    pure unit

compileAssignment (UQD _ (BindVariable varName) f1 _ _ _) = do
  f1' <- context2string f1
  pure \contextId -> do
    v <- lift $ lift (contextId ##= f1')
    lift $ lift $ addBinding varName (unsafeCoerce v)
    pure unit

compileAssignment (MQD dom (ExternalEffectFullFunction functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions :: Array (ContextInstance ~~> String)) <- traverse (unsafeCoerce compileFunction) args
  pure (\c -> do
    (values :: Array (Array String)) <- lift $ lift $ traverse (\g -> c ##= g) argFunctions
    case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
      0 -> (unsafeCoerce f :: ContextInstance -> MPT Unit) c
      1 -> (unsafeCoerce f :: (Array String -> ContextInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        c
      2 -> (unsafeCoerce f :: (Array String -> Array String -> ContextInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        c
      3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> ContextInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        c
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> ContextInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        c
      _ -> throwError (error "Too many arguments for external core module: maximum is 4")
    )

-- Catchall, remove when all cases have been covered.
compileAssignment otherwise = throwError (error ("Found unknown case for compileAssignment: " <> show otherwise))
