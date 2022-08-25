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
import Data.Array (catMaybes, concat, filter, filterA, head, length, union, unsafeIndex)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..), defaultContextSerializationRecord)
import Perspectives.Assignment.Update (addProperty, deleteProperty, moveRoleInstanceToAnotherContext, removeProperty, setProperty, roleContextualisations)
import Perspectives.CoreTypes (type (~~>), MP, MPT, MonadPerspectivesTransaction, Updater, (###>>), (##=), (##>), (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Error.Boundaries (handlePerspectRolError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionNArgs, lookupHiddenFunction)
import Perspectives.Guid (guid)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (buitenRol, constructUserIdentifier, deconstructModelName)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (allRoleBinders, getFilledRoles) as OG
import Perspectives.Instances.ObjectGetters (binding, context, roleType_)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Arc.PhaseTwo (addNamespace)
import Perspectives.Persistent (getPerspectEntiteit)
import Perspectives.Persistent.PublicStore (mapPublicStore)
import Perspectives.PerspectivesState (addBinding, getVariableBindings)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Query.UnsafeCompiler (compileFunction, getRoleInstances, role2context, role2propertyValue, role2role, role2string, typeTimeOnly)
import Perspectives.Representation.ADT (allLeavesInADT)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (bindingOfRole, contextOfADT)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.SaveUserData (removeBinding, setBinding, setFirstBinding, scheduleRoleRemoval, scheduleContextRemoval)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (computesDatabaseQueryRole, getPublicStore_, hasContextAspect, isDatabaseQueryRole)
import Unsafe.Coerce (unsafeCoerce)

-- Deletes, from all contexts, the role instance.
compileAssignmentFromRole :: QueryFunctionDescription -> MP (Updater RoleInstance)
compileAssignmentFromRole (UQD _ QF.RemoveRole rle _ _ mry) = do
  roleGetter <- role2role rle
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
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
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
    contextsToBeRemoved <- lift (roleId ##= roleGetter >=> binding >=> context)
    case head roles of
      Nothing -> pure unit
      Just ri -> do
        authorizedRole <- lift $ roleType_ ri
        for_ roles scheduleRoleRemoval
        for_ contextsToBeRemoved (scheduleContextRemoval $ Just $ ENR authorizedRole)

-- Deletes, from all contexts, all instances of the role.
compileAssignmentFromRole (UQD _ (QF.DeleteRole qualifiedRoleIdentifier) contextsToDeleteFrom _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextsToDeleteFrom
  roleGetter <- pure $ getRoleInstances (ENR qualifiedRoleIdentifier)
  pure \roleId -> do
    ctxtsToDeleteFrom <- lift (roleId ##= contextGetter)
    for_ ctxtsToDeleteFrom \ctxtToDeleteFrom -> do
      (roles :: Array RoleInstance) <- lift (ctxtToDeleteFrom ##= roleGetter)
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
    ctxts <- lift (roleId ##= contextGetter)
    for_ ctxts \ctxtToDeleteFrom -> do
      -- Remove all role instances and remove all contexts bound to them.
      rolesToBeRemoved <- lift (ctxtToDeleteFrom ##= getRoleInstances qualifiedRoleIdentifier)
      contextsToBeRemoved <- lift (ctxtToDeleteFrom ##= getRoleInstances qualifiedRoleIdentifier >=> binding >=> context)
      for_ rolesToBeRemoved scheduleRoleRemoval
      for_ contextsToBeRemoved (scheduleContextRemoval $ Just qualifiedRoleIdentifier)

-- Create a context. Fill a new context role instance with its external role, unless it is a DBQ role.
compileAssignmentFromRole (UQD _ (QF.CreateContext qualifiedContextTypeIdentifier localName qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \(roleId :: RoleInstance) -> do
    ctxts <- lift (roleId ##= contextGetter)
    for_ ctxts \ctxt -> do
      case qualifiedRoleIdentifier of
        -- calculatedType is guaranteed by the statementcompiler to be a DBQ role.
        -- Create a context without binding contextrole.
        -- Calculated Roles cannot be specialised, and there is no way to model a specialised embedded context type.
        CR calculatedType -> do 
          contextIdentifier <- constructContextIdentifier qualifiedContextTypeIdentifier localName
          void $ runExceptT $ constructContext (Just qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
            { id = contextIdentifier
            , ctype = unwrap qualifiedContextTypeIdentifier
            })

        ENR enumeratedType -> do
          -- Get the contextualised versions of the role type that we should create.
          roleTypesToCreate <- roleContextualisations ctxt enumeratedType
          -- Now, each of these role types may have a more restricted filler.
          for_ roleTypesToCreate \roleTypeToCreate -> do
            -- Get the context types whose external roles may be bound to this role type we're about to create.
            -- Keep only those that are a specialisation of qualifiedContextTypeIdentifier.
            contextTypesToCreate <- lift (bindingOfRole (ENR roleTypeToCreate) >>= contextOfADT >>= pure <<< allLeavesInADT)
              >>= filterA \cType -> lift (cType ###>> hasContextAspect qualifiedContextTypeIdentifier)
            -- If there are specialisations, do not create the aspect type.
            contextTypesToCreate' <- if length contextTypesToCreate > 1 
              then pure $ filter ((notEq) qualifiedContextTypeIdentifier) contextTypesToCreate
              else pure contextTypesToCreate
            for contextTypesToCreate' \contextTypeToCreate -> do 
              contextIdentifier <- constructContextIdentifier qualifiedContextTypeIdentifier localName
              void $ runExceptT $ constructContext (Just $ ENR roleTypeToCreate) (ContextSerialization defaultContextSerializationRecord
                { id = contextIdentifier
                , ctype = unwrap contextTypeToCreate
                })
              void $ createAndAddRoleInstance roleTypeToCreate (unwrap ctxt) (RolSerialization
                { id: Nothing
                , properties: PropertySerialization empty
                , binding: Just $ buitenRol contextIdentifier })

compileAssignmentFromRole (UQD _ (QF.CreateRootContext qualifiedContextTypeIdentifier localName) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure $ withAuthoringRole 
    (ENR $ EnumeratedRoleType sysUser)
    \(roleId :: RoleInstance) -> do
      ctxts <- lift (roleId ##= contextGetter)
      for_ ctxts \ctxt -> do
        contextIdentifier <- constructContextIdentifier qualifiedContextTypeIdentifier localName
        void $ runExceptT $ constructContext Nothing (ContextSerialization defaultContextSerializationRecord
          { id = contextIdentifier
          , ctype = unwrap qualifiedContextTypeIdentifier
          })

compileAssignmentFromRole (UQD _ (QF.CreateContext_ qualifiedContextTypeIdentifier localName) roleGetterDescription _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleGetterDescription
  pure \(roleId :: RoleInstance) -> do
    roles <- lift (roleId ##= roleGetter)
    for_ roles \roleInstance -> do
      roleTypeToFill <- lift $ roleType_ roleInstance
      -- Get the context types whose external roles may be bound to this role type we're about to fill.
      -- Keep only those that are a specialisation of qualifiedContextTypeIdentifier.
      contextTypesToCreate <- lift (bindingOfRole (ENR roleTypeToFill) >>= contextOfADT >>= pure <<< allLeavesInADT)
        >>= filterA \cType -> lift (cType ###>> hasContextAspect qualifiedContextTypeIdentifier)
      -- We can only create one context instance for this roleInstance. 
      -- Arbitrarily, we choose the first context type:
      case head contextTypesToCreate of
        Nothing -> pure unit
        Just contextTypeToCreate -> do 
          contextIdentifier <- constructContextIdentifier qualifiedContextTypeIdentifier localName
          newContext <- runExceptT $ constructContext (Just $ ENR roleTypeToFill) (ContextSerialization defaultContextSerializationRecord
            { id = contextIdentifier
            , ctype = unwrap contextTypeToCreate
            })
          -- now bind it in the role instance.
          void $ setFirstBinding roleInstance (RoleInstance $ buitenRol contextIdentifier) Nothing

compileAssignmentFromRole (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \roleId -> do
    ctxts <- lift (roleId ##= contextGetter)
    for_ ctxts \ctxt -> do
      roleTypesToCreate <- roleContextualisations ctxt qualifiedRoleIdentifier
      for_ roleTypesToCreate \objectType -> unsafePartial $ fromJust <$> createAndAddRoleInstance objectType (unwrap ctxt) (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})

compileAssignmentFromRole (BQD _ QF.Move roleToMove contextToMoveTo _ _ mry) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextToMoveTo
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleToMove
  if (pessimistic mry)
    then pure \roleId -> do
      c <- lift (roleId ##>> contextGetter)
      (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
      case head roles of
        Nothing -> pure unit
        Just role -> try (lift $ getPerspectEntiteit role) >>=
          handlePerspectRolError "compileAssignmentFromRole, Move"
            (\((PerspectRol{context, pspType}) :: PerspectRol) -> for roles (moveRoleInstanceToAnotherContext context c pspType))

    else pure \roleId -> do
      ctxt <- lift (roleId ##> contextGetter)
      case ctxt of
        Nothing -> pure unit
        Just c -> do
          (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
          case head roles of
            Nothing -> pure unit
            Just role -> try (lift $ getPerspectEntiteit role) >>=
              handlePerspectRolError "compileAssignmentFromRole, Move"
                (\((PerspectRol{context, pspType}) :: PerspectRol) -> for roles  (moveRoleInstanceToAnotherContext context c pspType))

compileAssignmentFromRole (BQD _ (QF.Bind qualifiedRoleIdentifier) bindings contextToBindIn _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextToBindIn
  (bindingsGetter :: (RoleInstance ~~> RoleInstance)) <- role2role bindings
  pure \roleId -> do
    ctxts <- lift (roleId ##= contextGetter)
    (bindings' :: Array RoleInstance) <- lift (roleId ##= bindingsGetter)
    for_ ctxts \ctxt -> do
      roleTypesToCreate' <- roleContextualisations ctxt qualifiedRoleIdentifier    
      for_ roleTypesToCreate' \objectType ->
        for_ bindings' \bndg -> createAndAddRoleInstance
          objectType
          (unwrap ctxt)
          (RolSerialization{id: Nothing, properties: PropertySerialization empty, binding: Just (unwrap bndg)})

compileAssignmentFromRole (BQD _ QF.Bind_ binding binder _ _ _) = do
  (bindingGetter :: (RoleInstance ~~> RoleInstance)) <- role2role binding
  (binderGetter :: (RoleInstance ~~> RoleInstance)) <- role2role binder
  pure \roleId -> do
    (binding' :: Maybe RoleInstance) <- lift (roleId ##> bindingGetter)
    (binder' :: Maybe RoleInstance) <- lift (roleId ##> binderGetter)
    -- setBinding caches, saves, sets isMe and me.
    void $ case binding', binder' of
      Just binding'', Just binder'' -> setBinding binder'' binding'' Nothing
      _, _ -> pure []

compileAssignmentFromRole (UQD _ (QF.Unbind mroleType) bindings _ _ _) = do
  (bindingsGetter :: (RoleInstance ~~> RoleInstance)) <- role2role bindings
  case mroleType of
    Nothing -> pure
      \roleId -> do
        binders <- lift (roleId ##= bindingsGetter >=> OG.allRoleBinders)
        for_ binders removeBinding
    Just roleType -> do
      EnumeratedRole role <- getEnumeratedRole roleType
      pure \roleId -> do
        -- We have no information about the ContextType in which these binders of type `roleType` are a member.
        -- Therefore we currently just remove them from the lexical context of roleType.
        -- When we implement the `remove filler` syntax, the modeller can specify the ContextType as well.
        binders <- lift (roleId ##= bindingsGetter >=> OG.getFilledRoles role.context roleType)
        for_ binders removeBinding

compileAssignmentFromRole (BQD _ QF.Unbind_ bindings binders _ _ _) = do
  (bindingsGetter :: (RoleInstance ~~> RoleInstance)) <- role2role bindings
  (bindersGetter :: (RoleInstance ~~> RoleInstance)) <- role2role binders
  pure \roleId -> do
    (binding :: Maybe RoleInstance) <- lift (roleId ##> bindingsGetter)
    (binder :: Maybe RoleInstance) <- lift (roleId ##> bindersGetter)
    -- TODO. As soon as we introduce multiple values for a binding, we have to adapt this so the binding argument
    -- is taken into account, too.
    void $ case binder of
      Nothing -> pure []
      Just binder' -> removeBinding binder'

compileAssignmentFromRole (UQD _ (QF.DeleteProperty qualifiedProperty) roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
    deleteProperty roles qualifiedProperty

compileAssignmentFromRole (BQD _ (QF.RemovePropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  (valueGetter :: (RoleInstance ~~> Value)) <- role2propertyValue valueQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
    (values :: Array Value) <- lift (roleId ##= valueGetter)
    removeProperty roles qualifiedProperty values

compileAssignmentFromRole (BQD _ (QF.AddPropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  (valueGetter :: (RoleInstance ~~> Value)) <- role2propertyValue valueQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
    (values :: Array Value) <- lift (roleId ##= valueGetter)
    addProperty roles qualifiedProperty (flip Tuple Nothing <$> values)

compileAssignmentFromRole (BQD _ (QF.SetPropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  (valueGetter :: (RoleInstance ~~> Value)) <- role2propertyValue valueQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
    (values :: Array Value) <- lift (roleId ##= valueGetter)
    setProperty roles qualifiedProperty values

-- Even though SequenceF is compiled in the QueryCompiler, we need to handle it here, too.
-- In the QueryCompiler, the components will be variable bindings.
-- Here they will be assignments.
compileAssignmentFromRole (BQD _ (BinaryCombinator SequenceF) f1 f2 _ _ _) = do
  if (typeTimeOnly f1)
    -- Skip all VarBindings that were meant for the description compiler only.
    -- These will be bindings that are added by the core in the StateCompilers.
    then if (typeTimeOnly f2)
      then pure \c -> pure unit
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
    old <- lift $ getVariableBindings
    void $ lift $ modify \s@{variableBindings} -> s {variableBindings = (_pushFrame old)}
    r <- f1' c
    void $ lift $ modify \s@{variableBindings} -> s {variableBindings = old}
    pure unit

compileAssignmentFromRole (UQD _ (BindVariable varName) f1 _ _ _) = do
  f1' <- role2string f1
  pure \roleId -> do
    v <- lift (roleId ##= f1')
    lift $ addBinding varName (unsafeCoerce v)
    pure unit

compileAssignmentFromRole (UQD _ (BindResultFromCreatingAssignment varName) f1 _ _ _) = do
  f1' <- compileCreatingAssignments f1
  pure \contextId -> do
    v <- f1' contextId
    lift $ addBinding varName (unsafeCoerce v)
    pure unit

compileAssignmentFromRole (MQD dom (ExternalEffectFullFunction functionName) args _ _ _) = do
  (f :: HiddenFunction) <- pure $ unsafePartial $ fromJust $ lookupHiddenFunction functionName
  (argFunctions :: Array (RoleInstance ~~> String)) <- traverse (unsafeCoerce compileFunction) args
  pure (\c -> do
    (values :: Array (Array String)) <- lift $ traverse (\g -> c ##= g) argFunctions
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
    (values :: Array (Array String)) <- lift $ traverse (\g -> c ##= g) argFunctions
    modify (over Transaction \t@{scheduledAssignments} -> t
      { scheduledAssignments = scheduledAssignments `union` [ExecuteDestructiveEffect functionName (unwrap c) values] }))

-- Catchall, remove when all cases have been covered.
compileAssignmentFromRole otherwise = throwError (error ("Found unknown case for compileAssignmentFromRole: " <> show otherwise))

-- | This is a special way to compile Creating assignment statements, that results in a function that actually returns
-- | something, as opposed to ordinary assignment statements (including Creating statements) that return nothing.
-- | For a context, the roles that bind the external role of the newly created contexts is returned.
compileCreatingAssignments :: QueryFunctionDescription -> MP (RoleInstance -> MonadPerspectivesTransaction (Array String))
compileCreatingAssignments (UQD _ (QF.CreateContext qualifiedContextTypeIdentifier localName qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \(roleId :: RoleInstance) -> do
    ctxts <- lift (roleId ##= contextGetter)
    results <- concat <$> for ctxts \ctxt -> do
      -- TODO. Breid qualifiedRoleIdentifier uit naar RoleType: nu hanteren we alleen EnumeratedRoleType.
      case qualifiedRoleIdentifier of
        -- calculatedType is guaranteed by the statementcompiler to be a DBQ role.
        -- Create a context without binding contextrole.
        CR calculatedType -> do
          contextIdentifier <- constructContextIdentifier qualifiedContextTypeIdentifier localName
          r <- runExceptT $ constructContext (Just qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
            { id = contextIdentifier
            , ctype = unwrap qualifiedContextTypeIdentifier
            })
          case r of
            Left e -> do
              logPerspectivesError e
              pure $ [Left e]
            Right ci -> pure $ [Right $ unwrap ci]

        ENR enumeratedType -> do
          -- Get the contextualised versions of the role type that we should create.
          roleTypesToCreate <- roleContextualisations ctxt enumeratedType
          -- Now, each of these role types may have a more restricted filler.
          concat <$> for roleTypesToCreate \roleTypeToCreate -> do
            -- Get the context types whose external roles may be bound to this role type we're about to create.
            -- Keep only those that are a specialisation of qualifiedContextTypeIdentifier.
            contextTypesToCreate <- lift (bindingOfRole (ENR roleTypeToCreate) >>= contextOfADT >>= pure <<< allLeavesInADT)
              >>= filterA \cType -> lift (cType ###>> hasContextAspect qualifiedContextTypeIdentifier)
            contextTypesToCreate' <- if length contextTypesToCreate > 1 
              then pure $ filter ((notEq) qualifiedContextTypeIdentifier) contextTypesToCreate
              else pure contextTypesToCreate
            for contextTypesToCreate' \contextTypeToCreate -> do
              contextIdentifier <- constructContextIdentifier qualifiedContextTypeIdentifier localName
              r <- runExceptT $ constructContext (Just $ ENR roleTypeToCreate) (ContextSerialization defaultContextSerializationRecord
                { id = contextIdentifier
                , ctype = unwrap contextTypeToCreate
                })
              case r of
                Left e -> do
                  logPerspectivesError e
                  pure $ Left e
                Right _ -> (Right <<< unwrap <<< unsafePartial fromJust) <$> createAndAddRoleInstance
                  roleTypeToCreate 
                  (unwrap ctxt)
                  (RolSerialization
                    { id: Nothing
                    , properties: PropertySerialization empty
                    , binding: Just $ buitenRol contextIdentifier })
    pure $ catMaybes (hush <$> results)

compileCreatingAssignments (UQD _ (QF.CreateRootContext qualifiedContextTypeIdentifier localName) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \(roleId :: RoleInstance) -> do
      originalRole <- gets (_.authoringRole <<< unwrap)
      modify (over Transaction \t -> t {authoringRole = (ENR $ EnumeratedRoleType sysUser)})
      ctxts <- lift (roleId ##= contextGetter)
      r <- for ctxts \ctxt -> do
        contextIdentifier <- constructContextIdentifier qualifiedContextTypeIdentifier localName
        void $ runExceptT $ constructContext Nothing (ContextSerialization defaultContextSerializationRecord
          { id = contextIdentifier
          , ctype = unwrap qualifiedContextTypeIdentifier
          })
        pure $ contextIdentifier
      modify (over Transaction \t -> t {authoringRole = originalRole})
      pure r

compileCreatingAssignments (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  pure \roleId -> do
    ctxts <- lift (roleId ##= contextGetter)
    concat <$> for ctxts \ctxt -> do
      roleTypesToCreate <- roleContextualisations ctxt qualifiedRoleIdentifier
      for roleTypesToCreate
        \roleTypeToCreate -> do 
          roleIdentifier <- unsafePartial $ fromJust <$> createAndAddRoleInstance 
            roleTypeToCreate 
            (unwrap ctxt) 
            (RolSerialization {id: Nothing, properties: PropertySerialization empty, binding: Nothing})
          -- No need to handle retrieval errors as we've just created the role.
          pure (unwrap roleIdentifier)
compileCreatingAssignments qfd = pure \ci -> pure []

constructContextIdentifier :: ContextType -> Maybe String -> MonadPerspectivesTransaction String
constructContextIdentifier ctype@(ContextType cname) mlocalName = do
  localName <- case mlocalName of 
      Nothing -> show <$> liftEffect guid
      Just n -> pure n
  mPStore <- lift $ getPublicStore_ ctype 
  case mPStore of 
    Nothing -> pure $ constructUserIdentifier localName
    Just pStore -> pure $ addNamespace (unsafePartial mapPublicStore pStore (unsafePartial fromJust $ deconstructModelName cname)) localName

withAuthoringRole :: forall a. RoleType -> Updater a -> Updater a
withAuthoringRole aRole updater a = do
  originalRole <- gets (_.authoringRole <<< unwrap)
  modify (over Transaction \t -> t {authoringRole = aRole})
  updater a
  modify (over Transaction \t -> t {authoringRole = originalRole})
