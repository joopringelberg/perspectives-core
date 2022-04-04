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

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, concat, cons, elemIndex, find, head, nub, union, unsafeIndex)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (empty, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..), defaultContextSerializationRecord)
import Perspectives.Assignment.Update (addProperty, deleteProperty, moveRoleInstanceToAnotherContext, removeProperty, setProperty)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (type (~~>), MP, MPT, MonadPerspectivesTransaction, Updater, (##=), (##>), (##>>), (###=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionNArgs, lookupHiddenFunction)
import Perspectives.Guid (guid)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (buitenRol, deconstructNamespace_)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (allRoleBinders, getFilledRoles) as OG
import Perspectives.Instances.ObjectGetters (binding, context, getUnlinkedRoleInstances, roleType_)
import Perspectives.Persistent (getPerspectContext, getPerspectEntiteit, getPerspectRol)
import Perspectives.PerspectivesState (addBinding, getVariableBindings)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Query.UnsafeCompiler (compileFunction, getRoleInstances, role2context, role2propertyValue, role2role, role2string, typeTimeOnly)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.SaveUserData (removeBinding, setBinding, setFirstBinding)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (allUnlinkedRoles, computesDatabaseQueryRole, isDatabaseQueryRole)
import Unsafe.Coerce (unsafeCoerce)

-- | Add the role instance to the end of the roles to exit.
-- | Add the actual removal instruction to the end of the scheduledAssignments.
-- | (we build a last-in, last-out stack of destructive effects)
scheduleRoleRemoval :: RoleInstance -> MonadPerspectivesTransaction Unit
scheduleRoleRemoval id = do
  -- If the role's context is scheduled to be removed, don't add its removal to the scheduledAssignments.
  contextIsScheduledToBeRemoved <- lift $ gets (\(Transaction{scheduledAssignments}) -> let
    contextOfRole = ContextInstance $ deconstructNamespace_ (unwrap id)
    in isJust $ find
      (case _ of
        ContextRemoval ctxt _ -> ctxt == contextOfRole
        _ -> false)
      scheduledAssignments)
  roleIsUntouchable <- lift $ gets (\(Transaction{untouchableRoles}) -> isJust $ elemIndex id untouchableRoles)
  if contextIsScheduledToBeRemoved || roleIsUntouchable
    then pure unit
    else lift $ modify (over Transaction \t@{scheduledAssignments, rolesToExit} -> t
      { rolesToExit = rolesToExit `union` [id]
      , scheduledAssignments = scheduledAssignments `union` [RoleRemoval id]
      })

-- | Schedules all roles in the context, including its external role, for removal.
-- | Add the actual removal instruction to the end of the scheduledAssignments.
-- | (we build a last-in, last-out stack of destructive effects)
scheduleContextRemoval :: Maybe RoleType -> ContextInstance -> MonadPerspectivesTransaction Unit
scheduleContextRemoval authorizedRole id = (lift2 $ try $ getPerspectContext id) >>=
  handlePerspectContextError "removeContextInstance"
  \(ctxt@(PerspectContext{rolInContext, buitenRol, pspType:contextType})) -> do
    unlinkedRoleTypes <- lift2 (contextType ###= allUnlinkedRoles)
    unlinkedInstances <- lift2 $ concat <$> (for unlinkedRoleTypes \rt -> id ##= getUnlinkedRoleInstances rt)
    lift $ modify (over Transaction \t@{scheduledAssignments, rolesToExit} -> t
      { scheduledAssignments = scheduledAssignments `union` [(ContextRemoval id authorizedRole)]
      , rolesToExit = nub $ rolesToExit <> (cons buitenRol $ unlinkedInstances <> (concat $ values rolInContext))})

-- Deletes, from all contexts, the role instance.
compileAssignmentFromRole :: QueryFunctionDescription -> MP (Updater RoleInstance)
compileAssignmentFromRole (UQD _ QF.RemoveRole rle _ _ mry) = do
  roleGetter <- role2role rle
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
    for_ roles scheduleRoleRemoval

-- Removes, from all contexts, both the role with kind ContextRole that hold the contexts, and the context themselves.
-- Handles DBQ roles by not trying to remove a context role.
compileAssignmentFromRole (UQD _ QF.RemoveContext rle _ _ mry) = do
  roleGetter <- computesDatabaseQueryRole rle >>=
    if _
      -- A database query role is calculated. No context role instances can be removed.
      then pure \_ -> ArrayT $ pure []
      else role2role rle
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
    contextsToBeRemoved <- lift2 (roleId ##= roleGetter >=> binding >=> context)
    case head roles of
      Nothing -> pure unit
      Just ri -> do
        authorizedRole <- lift2 $ roleType_ ri
        for_ roles scheduleRoleRemoval
        for_ contextsToBeRemoved (scheduleContextRemoval $ Just $ ENR authorizedRole)

-- Deletes, from all contexts, all instances of the role.
compileAssignmentFromRole (UQD _ (QF.DeleteRole qualifiedRoleIdentifier) contextsToDeleteFrom _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextsToDeleteFrom
  roleGetter <- pure $ getRoleInstances (ENR qualifiedRoleIdentifier)
  pure \roleId -> do
    ctxtsToDeleteFrom <- lift $ lift (roleId ##= contextGetter)
    for_ ctxtsToDeleteFrom \ctxtToDeleteFrom -> do
      (roles :: Array RoleInstance) <- lift $ lift (ctxtToDeleteFrom ##= roleGetter)
      for_ roles scheduleRoleRemoval

-- Deletes, from all contexts, all instances of the role and the context that fills it.
-- Handles DBQ roles by not trying to remove a context role.
compileAssignmentFromRole (UQD _ (QF.DeleteContext qualifiedRoleIdentifier) contextsToDeleteFrom _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- isDatabaseQueryRole qualifiedRoleIdentifier >>=
    if _
      -- A database query role is calculated. No context role instances can be removed.
      then pure \_ -> ArrayT $ pure []
      else role2context contextsToDeleteFrom
  pure \roleId -> do
    ctxts <- lift $ lift (roleId ##= contextGetter)
    for_ ctxts \ctxtToDeleteFrom -> do
      -- Remove all role instances and remove all contexts bound to them.
      rolesToBeRemoved <- lift2 (ctxtToDeleteFrom ##= getRoleInstances qualifiedRoleIdentifier)
      contextsToBeRemoved <- lift2 (ctxtToDeleteFrom ##= getRoleInstances qualifiedRoleIdentifier >=> binding >=> context)
      for_ rolesToBeRemoved scheduleRoleRemoval
      for_ contextsToBeRemoved (scheduleContextRemoval $ Just qualifiedRoleIdentifier)

-- Create a context. Fill a new context role instance with its external role, unless it is a DBQ role.
compileAssignmentFromRole (UQD _ (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \(roleId :: RoleInstance) -> do
    ctxts <- lift2 (roleId ##= contextGetter)
    for_ ctxts \ctxt -> do
      g <- liftEffect guid
      case qualifiedRoleIdentifier of
        -- calculatedType is guaranteed by the statementcompiler to be a DBQ role.
        -- Create a context without binding contextrole.
        CR calculatedType -> void $ runExceptT $ constructContext (Just qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
          { id = "model:User$c" <> (show g)
          , ctype = unwrap qualifiedContextTypeIdentifier
          })

        ENR enumeratedType -> do
          void $ runExceptT $ constructContext (Just qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
            { id = "model:User$c" <> (show g)
            , ctype = unwrap qualifiedContextTypeIdentifier
            })
          void $ createAndAddRoleInstance enumeratedType (unwrap roleId) (RolSerialization
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
      void $ setFirstBinding roleInstance (RoleInstance $ buitenRol newContextId) Nothing

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
      case head roles of
        Nothing -> pure unit
        Just role -> try (lift $ lift $ getPerspectEntiteit role) >>=
          handlePerspectRolError "compileAssignmentFromRole, Move"
            (\((PerspectRol{context, pspType}) :: PerspectRol) -> for roles (moveRoleInstanceToAnotherContext context c pspType))

    else pure \roleId -> do
      ctxt <- lift $ lift (roleId ##> contextGetter)
      case ctxt of
        Nothing -> pure unit
        Just c -> do
          (roles :: Array RoleInstance) <- lift $ lift (roleId ##= roleGetter)
          case head roles of
            Nothing -> pure unit
            Just role -> try (lift $ lift $ getPerspectEntiteit role) >>=
              handlePerspectRolError "compileAssignmentFromRole, Move"
                (\((PerspectRol{context, pspType}) :: PerspectRol) -> for roles  (moveRoleInstanceToAnotherContext context c pspType))

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
    void $ case binding', binder' of
      Just binding'', Just binder'' -> setBinding binder'' binding'' Nothing
      _, _ -> pure []

compileAssignmentFromRole (UQD _ (QF.Unbind mroleType) bindings _ _ _) = do
  (bindingsGetter :: (RoleInstance ~~> RoleInstance)) <- role2role bindings
  case mroleType of
    Nothing -> pure
      \roleId -> do
        binders <- lift $ lift (roleId ##= bindingsGetter >=> OG.allRoleBinders)
        for_ binders removeBinding
    Just roleType -> do
      EnumeratedRole role <- getEnumeratedRole roleType
      pure \roleId -> do
        -- We have no information about the ContextType in which these binders of type `roleType` are a member.
        -- Therefore we currently just remove them from the lexical context of roleType.
        -- When we implement the `remove filler` syntax, the modeller can specify the ContextType as well.
        binders <- lift $ lift (roleId ##= bindingsGetter >=> OG.getFilledRoles role.context roleType)
        for_ binders removeBinding

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
      Just binder' -> removeBinding binder'

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
  if (typeTimeOnly f1)
    -- Skip all VarBindings that were meant for the description compiler only.
    -- These will be bindings that are added by the core in the StateCompilers.
    then if (typeTimeOnly f2)
      then pure \c -> ArrayT $ pure []
      else compileAssignmentFromRole f2
    else do
      if (typeTimeOnly f2)
        then compileAssignmentFromRole f1
        else do
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

compileAssignmentFromRole (UQD _ (BindResultFromCreatingAssignment varName) f1 _ _ _) = do
  f1' <- compileCreatingAssignments f1
  pure \contextId -> do
    v <- f1' contextId
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

compileAssignmentFromRole (MQD dom (ExternalDestructiveFunction functionName) args _ _ _) = do
  (argFunctions :: Array (RoleInstance ~~> String)) <- traverse (unsafeCoerce compileFunction) args
  pure (\c -> do
    (values :: Array (Array String)) <- lift $ lift $ traverse (\g -> c ##= g) argFunctions
    lift $ modify (over Transaction \t@{scheduledAssignments} -> t
      { scheduledAssignments = scheduledAssignments `union` [ExecuteDestructiveEffect functionName (unwrap c) values] }))

-- Catchall, remove when all cases have been covered.
compileAssignmentFromRole otherwise = throwError (error ("Found unknown case for compileAssignmentFromRole: " <> show otherwise))

-- | This is a special way to compile Creating assignment statements, that result in a function that actually returns
-- | something, as opposed to ordinary assignment statements (including Creating statements) that return nothing.
-- | For a context, the role that binds the external role of the newly created context is returned.
compileCreatingAssignments :: QueryFunctionDescription -> MP (RoleInstance -> MonadPerspectivesTransaction (Array String))
compileCreatingAssignments (UQD _ (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \(roleId :: RoleInstance) -> do
    ctxts <- lift2 (roleId ##= contextGetter)
    results <- for ctxts \ctxt -> do
      -- TODO. Breid qualifiedRoleIdentifier uit naar RoleType: nu hanteren we alleen EnumeratedRoleType.
      g <- liftEffect guid
      case qualifiedRoleIdentifier of
        -- calculatedType is guaranteed by the statementcompiler to be a DBQ role.
        -- Create a context without binding contextrole.
        CR calculatedType -> do
          r <- runExceptT $ constructContext (Just qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
            { id = "model:User$c" <> (show g)
            , ctype = unwrap qualifiedContextTypeIdentifier
            })
          case r of
            Left e -> do
              logPerspectivesError e
              pure $ Left e
            Right ci -> pure $ Right $ unwrap ci

        ENR enumeratedType -> do
          r <- runExceptT $ constructContext (Just qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
            { id = "model:User$c" <> (show g)
            , ctype = unwrap qualifiedContextTypeIdentifier
            })
          case r of
            Left e -> do
              logPerspectivesError e
              pure $ Left e
            Right _ -> (Right <<< unwrap <<< unsafePartial fromJust) <$> createAndAddRoleInstance
              enumeratedType (unwrap ctxt)
              (RolSerialization
                { id: Nothing
                , properties: PropertySerialization empty
                , binding: Just $ buitenRol $ "model:User$c" <> (show g) })
    pure $ catMaybes (hush <$> results)

compileCreatingAssignments (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \roleId -> do
    ctxts <- lift2 (roleId ##= contextGetter)
    for ctxts \ctxt -> do
      roleIdentifier <- unsafePartial $ fromJust <$> createAndAddRoleInstance qualifiedRoleIdentifier (unwrap ctxt) (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})
      -- No need to handle retrieval errors as we've just created the role.
      pure (unwrap roleIdentifier)
compileCreatingAssignments qfd = pure \ci -> pure []
