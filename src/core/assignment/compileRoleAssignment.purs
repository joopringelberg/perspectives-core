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
import Data.Array (catMaybes, concat, filter, filterA, head, index, length, nub, singleton, union, unsafeIndex)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Foreign (unsafeToForeign)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..), defaultContextSerializationRecord)
import Perspectives.Assignment.Update (addProperty, deleteProperty, moveRoleInstanceToAnotherContext, removeProperty, roleContextualisations, saveFile, setProperty)
import Perspectives.CoreTypes (type (~~>), MP, MPT, MonadPerspectivesTransaction, Updater, (###>>), (##=), (##>), (##>>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Error.Boundaries (handlePerspectRolError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (lookupHiddenFunctionNArgs, lookupHiddenFunction)
import Perspectives.HiddenFunction (HiddenFunction)
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Builders (constructContext, createAndAddRoleInstance)
import Perspectives.Instances.Environment (_pushFrame)
import Perspectives.Instances.ObjectGetters (allRoleBinders, getFilledRoles) as OG
import Perspectives.Instances.ObjectGetters (binding, context, roleType_)
import Perspectives.Instances.Values (writePerspectivesFile)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.PerspectivesState (addBinding, getVariableBindings)
import Perspectives.Query.QueryTypes (QueryFunctionDescription(..))
import Perspectives.Query.UnsafeCompiler (compileFunction, getRoleInstances, role2context, role2propertyValue, role2role, role2string, typeTimeOnly)
import Perspectives.Representation.ADT (allLeavesInADT)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (bindingOfRole, contextOfADT)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), ResourceType(..), RoleType(..))
import Perspectives.ResourceIdentifiers (createResourceIdentifier, databaseLocation, resourceIdentifier2DocLocator)
import Perspectives.SaveUserData (removeBinding, scheduleContextRemoval, scheduleRoleRemoval, setBinding, setFirstBinding, synchronise)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Types.ObjectGetters (computesDatabaseQueryRole, hasContextAspect, isDatabaseQueryRole)
import Unsafe.Coerce (unsafeCoerce)

-- Deletes, from all contexts, the role instance.
compileAssignmentFromRole :: QueryFunctionDescription -> MP (Updater RoleInstance) 
compileAssignmentFromRole (UQD _ QF.RemoveRole rle _ _ mry) = do
  roleGetter <- role2role rle
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
    for_ roles (scheduleRoleRemoval synchronise)

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
        for_ roles (scheduleRoleRemoval synchronise)
        users <- nub <<< concat <$> for roles (scheduleRoleRemoval synchronise)
        for_ contextsToBeRemoved (scheduleContextRemoval (Just $ ENR authorizedRole) users)

-- Deletes, from all contexts, all instances of the role.
compileAssignmentFromRole (UQD _ (QF.DeleteRole qualifiedRoleIdentifier) contextsToDeleteFrom _ _ _) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextsToDeleteFrom
  roleGetter <- pure $ getRoleInstances (ENR qualifiedRoleIdentifier)
  pure \roleId -> do
    ctxtsToDeleteFrom <- lift (roleId ##= contextGetter)
    for_ ctxtsToDeleteFrom \ctxtToDeleteFrom -> do
      (roles :: Array RoleInstance) <- lift (ctxtToDeleteFrom ##= roleGetter)
      for_ roles (scheduleRoleRemoval synchronise)

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
      users <- nub <<< concat <$> for rolesToBeRemoved (scheduleRoleRemoval synchronise)
      for_ contextsToBeRemoved (scheduleContextRemoval (Just qualifiedRoleIdentifier) users)

-- Create a context. Fill a new context role instance with its external role, unless it is a DBQ role.
compileAssignmentFromRole qfd@(UQD _ (QF.CreateContext _ _) _ _ _ _ ) = unsafePartial compileContextAssignmentFromRole qfd Nothing

compileAssignmentFromRole qfd@(BQD _ (QF.CreateContext _ _) _ nameGetterDescription _ _ _ ) = unsafePartial compileContextAssignmentFromRole 
  (unsafePartial makeUQD qfd)
  (Just nameGetterDescription)

compileAssignmentFromRole qfd@(UQD _ (QF.CreateRootContext _) _ _ _ _) = unsafePartial compileContextAssignmentFromRole qfd Nothing

compileAssignmentFromRole qfd@(BQD _ (QF.CreateRootContext _) _ nameGetterDescription _ _ _ ) = unsafePartial compileContextAssignmentFromRole 
  (unsafePartial makeUQD qfd) 
  (Just nameGetterDescription)

compileAssignmentFromRole qfd@(UQD _ (QF.CreateContext_ _) _ _ _ _) = unsafePartial compileContextAssignmentFromRole qfd Nothing

compileAssignmentFromRole qfd@(BQD _ (QF.CreateContext_ _) _ nameGetterDescription _ _ _) = unsafePartial compileContextAssignmentFromRole 
  (unsafePartial makeUQD qfd) 
  (Just nameGetterDescription)

compileAssignmentFromRole qfd@(UQD _ (QF.CreateRole qualifiedRoleIdentifier) _ _ _ _) = unsafePartial compileRoleAssignment qfd Nothing

compileAssignmentFromRole qfd@(BQD _ (QF.CreateRole qualifiedRoleIdentifier) _ nameGetterDescription _ _ _) = unsafePartial compileRoleAssignment 
  (unsafePartial makeUQD qfd)
  (Just nameGetterDescription)

compileAssignmentFromRole (BQD _ QF.Move roleToMove contextToMoveTo _ _ mry) = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextToMoveTo
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleToMove
  if (pessimistic mry)
    then pure \roleId -> do
      c <- lift (roleId ##>> contextGetter)
      (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
      case head roles of
        Nothing -> pure unit
        Just role -> try (lift $ getPerspectRol role) >>=
          handlePerspectRolError "compileAssignmentFromRole, Move"
            (\((PerspectRol{context, pspType}) :: PerspectRol) -> for roles (moveRoleInstanceToAnotherContext context c pspType Nothing))

    else pure \roleId -> do
      ctxt <- lift (roleId ##> contextGetter)
      case ctxt of
        Nothing -> pure unit
        Just c -> do
          (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
          case head roles of
            Nothing -> pure unit
            Just role -> try (lift $ getPerspectRol role) >>=
              handlePerspectRolError "compileAssignmentFromRole, Move"
                (\((PerspectRol{context, pspType}) :: PerspectRol) -> for roles  (moveRoleInstanceToAnotherContext context c pspType Nothing))

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
    deleteProperty roles qualifiedProperty Nothing

compileAssignmentFromRole (BQD _ (QF.RemovePropertyValue qualifiedProperty) valueQfd roleQfd _ _ _) = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleQfd
  (valueGetter :: (RoleInstance ~~> Value)) <- role2propertyValue valueQfd
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
    (values :: Array Value) <- lift (roleId ##= valueGetter)
    removeProperty roles qualifiedProperty Nothing values

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
    setProperty roles qualifiedProperty Nothing values

compileAssignmentFromRole (MQD _ (QF.CreateFileF mimeType qualifiedProperty) args _ _ _) = do
  -- args = [filenameQfd, contentQfd, roleQfd]
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role (unsafePartial fromJust $ index args 2)
  (contentGetter :: (RoleInstance ~~> Value)) <- role2propertyValue (unsafePartial fromJust $ index args 1)
  (fileNameGetter :: (RoleInstance ~~> Value)) <- role2propertyValue (unsafePartial fromJust $ index args 0)
  pure \roleId -> do
    (roles :: Array RoleInstance) <- lift (roleId ##= roleGetter)
    (contents :: Array Value) <- lift (roleId ##= contentGetter)
    (fileNames :: Array Value) <- lift (roleId ##= fileNameGetter)
    case head roles, head contents, head fileNames of
      -- Notice that the content is a string. It is eventually passed on to toFile as a Foreign value and 
      -- then passed on to the File constructor. This constructor accepts Strings just as well as ArrayBuffers.
      Just roleInstance, Just content, Just (Value fileName) -> do 
        dbLoc <- lift $ databaseLocation $ unwrap roleInstance
        {documentName} <- lift $ resourceIdentifier2DocLocator (unwrap roleInstance)
        setProperty roles qualifiedProperty Nothing [Value $ writePerspectivesFile {fileName, mimeType, propertyType: qualifiedProperty, database: dbLoc, roleFileName: documentName}]
        void $ saveFile roleInstance qualifiedProperty (unsafeToForeign content) mimeType
      Just roleInstance, Nothing, Just (Value fileName) -> do 
        dbLoc <- lift $ databaseLocation $ unwrap roleInstance
        {documentName} <- lift $ resourceIdentifier2DocLocator (unwrap roleInstance)
        setProperty roles qualifiedProperty Nothing [Value $ writePerspectivesFile {fileName, mimeType, propertyType: qualifiedProperty, database: dbLoc, roleFileName: documentName}]
        void $ saveFile roleInstance qualifiedProperty (unsafeToForeign "") mimeType
      Nothing, _, Just (Value fileName) -> throwError (error $ "No role instance found to attach the file '" <> fileName <> "' to.")
      _, _, _ -> throwError (error $ "some of the arguments to create file are missing.")

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
    (nrOfParameters :: Int) <- pure $ unsafePartial (fromJust $ lookupHiddenFunctionNArgs functionName)
    -- Notice that the number of parameters given ignores the default argument (context or role) that the function is applied to anyway.
    -- If we do have an extra argument value, supply it as the last argument instead of r.
    (lastArgument :: RoleInstance) <- case index values nrOfParameters of
      Nothing -> pure c
      Just v -> pure $ RoleInstance (unsafePartial (unsafeIndex v 0))
    case unsafePartial $ fromJust $ lookupHiddenFunctionNArgs functionName of
      0 -> (unsafeCoerce f :: RoleInstance -> MPT Unit) lastArgument
      1 -> (unsafeCoerce f :: (Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        lastArgument
      2 -> (unsafeCoerce f :: (Array String -> Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        lastArgument
      3 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        lastArgument
      4 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        lastArgument
      5 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        lastArgument
      6 -> (unsafeCoerce f :: (Array String -> Array String -> Array String -> Array String -> Array String -> Array String -> RoleInstance -> MPT Unit))
        (unsafePartial (unsafeIndex values 0))
        (unsafePartial (unsafeIndex values 1))
        (unsafePartial (unsafeIndex values 2))
        (unsafePartial (unsafeIndex values 3))
        (unsafePartial (unsafeIndex values 4))
        (unsafePartial (unsafeIndex values 5))
        c
      _ -> throwError (error "Too many arguments for external core module: maximum is 6")
    )

compileAssignmentFromRole (MQD dom (ExternalDestructiveFunction functionName) args _ _ _) = do
  (argFunctions :: Array (RoleInstance ~~> String)) <- traverse (unsafeCoerce compileFunction) args
  pure (\c -> do
    (values :: Array (Array String)) <- lift $ traverse (\g -> c ##= g) argFunctions
    modify (over Transaction \t@{scheduledAssignments} -> t
      { scheduledAssignments = scheduledAssignments `union` [ExecuteDestructiveEffect functionName (unwrap c) values] }))

-- Catchall, remove when all cases have been covered.
compileAssignmentFromRole otherwise = throwError (error ("Found unknown case for compileAssignmentFromRole: " <> show otherwise))

compileRoleAssignment :: Partial => QueryFunctionDescription -> Maybe QueryFunctionDescription -> MP (Updater RoleInstance)
compileRoleAssignment (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) mnameGetterDescription = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  (mNameGetter :: Maybe (RoleInstance ~~> String)) <- case mnameGetterDescription of
    Just nameGetterDescription -> Just <$> role2string nameGetterDescription
    Nothing -> pure Nothing
  pure \roleId -> do
    ctxts <- lift (roleId ##= contextGetter)
    localName <- case mNameGetter of
      Just nameGetter -> lift $ Just <$> (roleId ##>> nameGetter)
      Nothing -> pure Nothing
    for_ ctxts \ctxt -> do
      roleTypesToCreate <- roleContextualisations ctxt qualifiedRoleIdentifier
      -- If the role type is indexed, adds the created instance to the indexed roles in PerspectivesState.
      for_ roleTypesToCreate \qualifiedRoleIdentifier' -> unwrap <<< unsafePartial fromJust <$> 
        createAndAddRoleInstance qualifiedRoleIdentifier' (unwrap ctxt) 
          (RolSerialization {id: localName, properties: PropertySerialization empty, binding: Nothing})


compileContextAssignmentFromRole :: Partial => QueryFunctionDescription -> Maybe QueryFunctionDescription -> MP (Updater RoleInstance)
compileContextAssignmentFromRole (UQD _ (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) contextGetterDescription _ _ _) mnameGetterDescription = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  (mNameGetter :: Maybe (RoleInstance ~~> String)) <- case mnameGetterDescription of
    Just nameGetterDescription -> Just <$> role2string nameGetterDescription
    Nothing -> pure Nothing
  pure \(roleId :: RoleInstance) -> do
    ctxts <- lift (roleId ##= contextGetter)
    localName <- case mNameGetter of
      Just nameGetter -> lift $ Just <$> (roleId ##>> nameGetter)
      Nothing -> pure Nothing
    for_ ctxts \ctxt -> do
      case qualifiedRoleIdentifier of
        -- calculatedType is guaranteed by the statementcompiler to be a DBQ role.
        -- Create a context without binding contextrole.
        -- Calculated Roles cannot be specialised, and there is no way to model a specialised embedded context type.
        CR calculatedType -> void do 
            contextCreationResult <- runExceptT $ constructContext (Just qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
              { ctype = unwrap qualifiedContextTypeIdentifier
              })
            case contextCreationResult of
              Left e -> do 
                logPerspectivesError e 
                pure $ Left e
              Right (ContextInstance newContext) -> pure $ Right newContext

        ENR enumeratedType -> do
          -- Get the contextualised versions of the role type that we should create.
          roleTypesToCreate <- roleContextualisations ctxt enumeratedType
          -- Now, each of these role types may have a more restricted filler.
          for_ roleTypesToCreate \roleTypeToCreate -> do
            -- Get the context types whose external roles may be bound to this role type we're about to create.
            -- Keep only those that are a specialisation of qualifiedContextTypeIdentifier.
            contextTypesToCreate <- lift (bindingOfRole (ENR roleTypeToCreate) 
              >>= pure <<< (map contextOfADT) 
              >>= pure <<< (map allLeavesInADT))
              >>= maybe (pure []) (filterA \cType -> lift (cType ###>> hasContextAspect qualifiedContextTypeIdentifier))
            -- If there are specialisations, do not create the aspect type.
            contextTypesToCreate' <- if length contextTypesToCreate > 1 
              then pure $ filter ((notEq) qualifiedContextTypeIdentifier) contextTypesToCreate
              else pure contextTypesToCreate
            for contextTypesToCreate' \contextTypeToCreate -> void do 
              contextIdentifier <- createResourceIdentifier (CType contextTypeToCreate)
              r <- runExceptT $ constructContext (Just $ ENR roleTypeToCreate) (ContextSerialization defaultContextSerializationRecord
                { id = Just contextIdentifier
                , ctype = unwrap contextTypeToCreate
                })
              case r of
                Left e -> do
                  logPerspectivesError e
                  pure $ Left e
                Right (ContextInstance newContext) -> do 
                  void $ createAndAddRoleInstance roleTypeToCreate (unwrap ctxt) (RolSerialization
                    { id: Nothing
                    , properties: PropertySerialization empty
                    , binding: Just $ buitenRol newContext })
                  pure $ Right contextIdentifier

compileContextAssignmentFromRole (UQD _ (QF.CreateRootContext qualifiedContextTypeIdentifier) contextGetterDescription _ _ _) mnameGetterDescription = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  (mNameGetter :: Maybe (RoleInstance ~~> String)) <- case mnameGetterDescription of
    Just nameGetterDescription -> Just <$> role2string nameGetterDescription
    Nothing -> pure Nothing
  pure $ withAuthoringRole 
    (ENR $ EnumeratedRoleType sysUser)
    \(roleId :: RoleInstance) -> do
      ctxts <- lift (roleId ##= contextGetter)
      localName <- case mNameGetter of
        Just nameGetter -> lift $ Just <$> (roleId ##>> nameGetter)
        Nothing -> pure Nothing
      for_ ctxts \ctxt -> do
        r <- runExceptT $ constructContext Nothing (ContextSerialization defaultContextSerializationRecord
          { ctype = unwrap qualifiedContextTypeIdentifier
          , id = localName
          })
        case r of
          Left e -> do 
            logPerspectivesError e
            pure $ Left e
          Right (ContextInstance newContext) -> pure $ Right newContext

compileContextAssignmentFromRole (UQD _ (QF.CreateContext_ qualifiedContextTypeIdentifier) roleGetterDescription _ _ _) mnameGetterDescription = do
  (roleGetter :: (RoleInstance ~~> RoleInstance)) <- role2role roleGetterDescription
  (mNameGetter :: Maybe (RoleInstance ~~> String)) <- case mnameGetterDescription of
    Just nameGetterDescription -> Just <$> role2string nameGetterDescription
    Nothing -> pure Nothing
  pure \(roleId :: RoleInstance) -> do
    roles <- lift (roleId ##= roleGetter)
    localName <- case mNameGetter of
      Just nameGetter -> lift $ Just <$> (roleId ##>> nameGetter)
      Nothing -> pure Nothing
    for_ roles \roleInstance -> do
      roleTypeToFill <- lift $ roleType_ roleInstance
      -- Get the context types whose external roles may be bound to this role type we're about to fill.
      -- Keep only those that are a specialisation of qualifiedContextTypeIdentifier.
      contextTypesToCreate <- lift (bindingOfRole (ENR roleTypeToFill) 
        >>= pure <<< (map contextOfADT) 
        >>= pure <<< (map allLeavesInADT))
        >>= maybe (pure []) (filterA \cType -> lift (cType ###>> hasContextAspect qualifiedContextTypeIdentifier))
      -- We can only create one context instance for this roleInstance. 
      -- Arbitrarily, we choose the first context type:
      case head contextTypesToCreate of
        Nothing -> pure unit
        Just contextTypeToCreate -> void do 
          newContext <- runExceptT $ constructContext (Just $ ENR roleTypeToFill) (ContextSerialization defaultContextSerializationRecord
            { ctype = unwrap contextTypeToCreate
            , id = localName
            })
          -- now bind it in the role instance.
          case newContext of 
            Left e -> do
              logPerspectivesError e
              pure $ Left e
            Right (ContextInstance ctxtId) -> do 
              -- now bind it in the role instance.
              void $ setFirstBinding roleInstance (RoleInstance $ buitenRol ctxtId) Nothing
              pure $ Right ctxtId

makeUQD :: Partial => QueryFunctionDescription -> QueryFunctionDescription
makeUQD (BQD dom qf contextGetterDescription _ ran fun man) =
  (UQD dom qf contextGetterDescription ran fun man)

-- | This is a special way to compile Creating assignment statements, that results in a function that actually returns
-- | something, as opposed to ordinary assignment statements (including Creating statements) that return nothing.
-- | For a context, the roles that bind the external role of the newly created contexts is returned.
compileCreatingAssignments :: QueryFunctionDescription -> MP (RoleInstance -> MonadPerspectivesTransaction (Array String))
compileCreatingAssignments qfd@(UQD _ (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) contextGetterDescription _ _ _) = 
  unsafePartial compileContextCreatingAssignments qfd Nothing

compileCreatingAssignments qfd@(BQD _ (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) contextGetterDescription mnameGetterDescription _ _ _) = 
  unsafePartial compileContextCreatingAssignments (unsafePartial makeUQD qfd) (Just mnameGetterDescription)

compileCreatingAssignments qfd@(UQD _ (QF.CreateRootContext qualifiedContextTypeIdentifier) contextGetterDescription _ _ _) =
  unsafePartial compileContextCreatingAssignments qfd Nothing

compileCreatingAssignments qfd@(BQD _ (QF.CreateRootContext qualifiedContextTypeIdentifier) contextGetterDescription mnameGetterDescription _ _ _) =
  unsafePartial compileContextCreatingAssignments (unsafePartial makeUQD qfd) (Just mnameGetterDescription)

compileCreatingAssignments qfd@(UQD _ (QF.CreateRole qualifiedRoleIdentifier) _ _ _ _) = 
  unsafePartial compileRoleCreatingAssignments qfd Nothing

compileCreatingAssignments qfd@(BQD _ (QF.CreateRole qualifiedRoleIdentifier) _ nameGetterDescription _ _ _) = 
  unsafePartial compileRoleCreatingAssignments 
    (unsafePartial makeUQD qfd) 
    (Just nameGetterDescription)

compileCreatingAssignments qfd = pure \ci -> pure []

compileRoleCreatingAssignments:: Partial => QueryFunctionDescription -> Maybe QueryFunctionDescription -> MP (RoleInstance -> MonadPerspectivesTransaction (Array String))
compileRoleCreatingAssignments (UQD _ (QF.CreateRole qualifiedRoleIdentifier) contextGetterDescription _ _ _) mnameGetterDescription = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  (mNameGetter :: Maybe (RoleInstance ~~> String)) <- case mnameGetterDescription of
    Just nameGetterDescription -> Just <$> role2string nameGetterDescription
    Nothing -> pure Nothing
  pure \roleId -> do
    ctxts <- lift (roleId ##= contextGetter)
    concat <$> for ctxts \ctxt -> do
      roleTypesToCreate <- roleContextualisations ctxt qualifiedRoleIdentifier
      localName <- case mNameGetter of
        Just nameGetter -> lift $ Just <$> (roleId ##>> nameGetter)
        Nothing -> pure Nothing
      for roleTypesToCreate
        \roleTypeToCreate -> do 
          roleIdentifier <- unsafePartial $ fromJust <$> createAndAddRoleInstance 
            roleTypeToCreate 
            (unwrap ctxt) 
            (RolSerialization {id: localName, properties: PropertySerialization empty, binding: Nothing})
          -- No need to handle retrieval errors as we've just created the role.
          pure (unwrap roleIdentifier)


compileContextCreatingAssignments :: Partial => QueryFunctionDescription -> Maybe QueryFunctionDescription -> MP (RoleInstance -> MonadPerspectivesTransaction (Array String))
compileContextCreatingAssignments (UQD _ (QF.CreateContext qualifiedContextTypeIdentifier qualifiedRoleIdentifier) contextGetterDescription _ _ _) mnameGetterDescription = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  (mNameGetter :: Maybe (RoleInstance ~~> String)) <- case mnameGetterDescription of
    Just nameGetterDescription -> Just <$> role2string nameGetterDescription
    Nothing -> pure Nothing
  pure \(roleId :: RoleInstance) -> do
    ctxts <- lift (roleId ##= contextGetter)
    localName <- case mNameGetter of
      Just nameGetter -> lift $ Just <$> (roleId ##>> nameGetter)
      Nothing -> pure Nothing
    results <- concat <$> for ctxts \ctxt -> do
      -- TODO. Breid qualifiedRoleIdentifier uit naar RoleType: nu hanteren we alleen EnumeratedRoleType.
      case qualifiedRoleIdentifier of
        -- calculatedType is guaranteed by the statementcompiler to be a DBQ role.
        -- Create a context without binding contextrole.
        CR calculatedType -> singleton <$> do
          r <- runExceptT $ constructContext (Just qualifiedRoleIdentifier) (ContextSerialization defaultContextSerializationRecord
            { id = localName
            , ctype = unwrap qualifiedContextTypeIdentifier
            })
          case r of
            Left e -> do
              logPerspectivesError e
              pure $ Left e
            Right ci -> pure $ Right $ unwrap ci

        ENR enumeratedType -> do
          -- Get the contextualised versions of the role type that we should create.
          roleTypesToCreate <- roleContextualisations ctxt enumeratedType
          -- Now, each of these role types may have a more restricted filler.
          concat <$> for roleTypesToCreate \roleTypeToCreate -> do
            -- Get the context types whose external roles may be bound to this role type we're about to create.
            -- Keep only those that are a specialisation of qualifiedContextTypeIdentifier.
            contextTypesToCreate <- lift (bindingOfRole (ENR roleTypeToCreate) 
              >>= pure <<< (map contextOfADT) 
              >>= pure <<< (map allLeavesInADT))
              >>= maybe (pure []) (filterA \cType -> lift (cType ###>> hasContextAspect qualifiedContextTypeIdentifier))
            contextTypesToCreate' <- if length contextTypesToCreate > 1 
              then pure $ filter ((notEq) qualifiedContextTypeIdentifier) contextTypesToCreate
              else pure contextTypesToCreate
            for contextTypesToCreate' \contextTypeToCreate -> do
              r <- runExceptT $ constructContext (Just $ ENR roleTypeToCreate) (ContextSerialization defaultContextSerializationRecord
                { ctype = unwrap contextTypeToCreate
                })
              case r of
                Left e -> do
                  logPerspectivesError e
                  pure $ Left e
                Right (ContextInstance contextIdentifier) -> (Right <<< unwrap <<< unsafePartial fromJust) <$> createAndAddRoleInstance
                  roleTypeToCreate 
                  (unwrap ctxt)
                  (RolSerialization
                    { id: Nothing
                    , properties: PropertySerialization empty
                    , binding: Just $ buitenRol contextIdentifier })
    pure $ catMaybes (hush <$> results)

compileContextCreatingAssignments (UQD _ (QF.CreateRootContext qualifiedContextTypeIdentifier) contextGetterDescription _ _ _) mnameGetterDescription = do
  (contextGetter :: (RoleInstance ~~> ContextInstance)) <- role2context contextGetterDescription
  (mNameGetter :: Maybe (RoleInstance ~~> String)) <- case mnameGetterDescription of
    Just nameGetterDescription -> Just <$> role2string nameGetterDescription
    Nothing -> pure Nothing
  pure \(roleId :: RoleInstance) -> do
      originalRole <- gets (_.authoringRole <<< unwrap)
      modify (over Transaction \t -> t {authoringRole = (ENR $ EnumeratedRoleType sysUser)})
      ctxts <- lift (roleId ##= contextGetter)
      localName <- case mNameGetter of
        Just nameGetter -> lift $ Just <$> (roleId ##>> nameGetter)
        Nothing -> pure Nothing
      r <- for ctxts \ctxt -> do
        contextCreationResult <- runExceptT $ constructContext Nothing (ContextSerialization defaultContextSerializationRecord
          { ctype = unwrap qualifiedContextTypeIdentifier
          , id = localName
          })
        case contextCreationResult of
          Left e -> do
            logPerspectivesError e
            pure $ Left e
          Right (ContextInstance ctxtId) -> pure $ Right ctxtId
      modify (over Transaction \t -> t {authoringRole = originalRole})
      pure $ catMaybes (hush <$> r)

withAuthoringRole :: forall a. RoleType -> Updater a -> Updater a
withAuthoringRole aRole updater a = do
  originalRole <- gets (_.authoringRole <<< unwrap)
  modify (over Transaction \t -> t {authoringRole = aRole})
  updater a
  modify (over Transaction \t -> t {authoringRole = originalRole})
