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

module Perspectives.Assignment.SerialiseAsDeltas
( serialisedAsDeltasFor
, getPropertyValues
, serialiseDependencies
, serialiseRoleInstancesAndProperties
, serialisedAsDeltasForUserType
)
where

import Control.Monad.AvarMonadAsk (get) as AMA
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (StateT, gets, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons) as ARR
import Data.Array (elemIndex)
import Data.Array.NonEmpty (NonEmptyArray, singleton) as NA
import Data.Array.NonEmpty (toArray)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmptyList, foldM, head)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Aff.AVar (new)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction, (###=), (##=))
import Perspectives.Deltas (addDelta)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.ModelDependencies (idProperty, sysUser)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Query.Interpreter (interpret)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, allPaths, singletonPath)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Representation.Class.Property (getProperty, getCalculation) as PClass
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.Transaction (Transaction(..), createTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.Types.ObjectGetters (perspectivesClosure_, propertiesInPerspective)
import Prelude (Unit, bind, discard, join, pure, show, unit, void, ($), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=))
import Simple.JSON (unsafeStringify, write)

serialisedAsDeltasFor :: ContextInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor cid userId = do
  userType <- lift $  roleType_ userId
  serialisedAsDeltasFor_ cid userId (ENR userType)

-- | Construct a Transaction that represents a context for a particular user role.
-- | Serialise the Transaction as a string.
serialisedAsDeltasForUserType :: ContextInstance -> RoleType -> MonadPerspectives Value
serialisedAsDeltasForUserType cid userType = do
  me <- getUserIdentifier
  (Transaction{author, timeStamp, deltas}) <- execMonadPerspectivesTransaction
    -- The authoringRole is used on *constructing* deltas. However, here we merely *read* deltas from the
    -- context- and role representations. So this value is in effect ignored.
    (ENR $ EnumeratedRoleType sysUser)
    -- NOTE: we provide serialisedAsDeltasFor_ with the value (RoleInstance me) as
    -- identifier for the user for whom we serialise. As we don't know the real
    -- user identifier (we serialise for a type!) we use it as a stand in.
    (serialisedAsDeltasFor_ cid (RoleInstance me) userType)
  tfp <- pure $ TransactionForPeer
    { author
    , timeStamp
    , deltas: _.delta <<< unwrap <$> deltas
  }
  pure $ Value $ unsafeStringify $ write tfp
  where
    -- | Execute a value in MonadPerspectivesTransaction, discard the result and return the transaction.
    execMonadPerspectivesTransaction :: forall o.
      RoleType ->
      MonadPerspectivesTransaction o ->
      MonadPerspectives Transaction
    execMonadPerspectivesTransaction authoringRole a =
      getUserIdentifier
      >>= lift <<< createTransaction authoringRole
      >>= lift <<< new
      >>= runReaderT run
        where
          run :: MonadPerspectivesTransaction Transaction
          run = do
            void a
            AMA.get


liftToMPT :: forall a. MonadPerspectives a -> MonadPerspectivesTransaction a
liftToMPT = lift

-- | The `userId` must be an instance of the `userType`, otherwise we cannot establish whether
-- | a perspective is a self-perspective.
serialisedAsDeltasFor_:: ContextInstance -> RoleInstance -> RoleType -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor_ cid userId userType =
  -- All Roletypes the user may see in this context, expressed as Perspectives.
   (liftToMPT (userType ###= perspectivesClosure_) >>= traverse_ (serialisePerspectiveForUser cid (NA.singleton userId) userType))

-- | Add Deltas to the transaction of the peers ultimately filling the given user roles, to provide
-- | them with a complete account of the perspective on the context instance.
serialisePerspectiveForUser ::
  ContextInstance ->
  NA.NonEmptyArray RoleInstance ->
  RoleType ->
  Perspective ->
  MonadPerspectivesTransaction Unit
serialisePerspectiveForUser cid users userRoleType p@(Perspective{object, propertyVerbs, selfOnly, isSelfPerspective}) = do
  (visiblePropertyTypes :: Array PropertyType) <- liftToMPT $ propertiesInPerspective p
  serialiseRoleInstancesAndProperties cid users object visiblePropertyTypes selfOnly isSelfPerspective

-- | MODEL DEPENDENCY IN THIS FUNCTION. The correct operation of this function depends on
-- | model:System. The role model:System$PerspectivesSystem$User should have a property with
-- | local name "Id". Instances need not have a value for that property.
serialiseRoleInstancesAndProperties ::
  ContextInstance ->                   -- The context instance for which we serialise roles and properties.
  NA.NonEmptyArray RoleInstance ->     -- User Role instances to serialise for. These have a single type.
  QueryFunctionDescription ->          -- Find object role instances with this description.
  Array PropertyType ->                 -- PropertyTypes whose values on the role instances should be serialised.
  Boolean ->                           -- true iff the perspective is selfonly.
  Boolean ->                           -- true iff the object of the perspective equals its subject.
  MonadPerspectivesTransaction Unit
serialiseRoleInstancesAndProperties cid users object properties selfOnly isPerspectiveOnSelf = do
  -- We know that object has a role range.
  properties' <- if isPerspectiveOnSelf
    then pure $ ARR.cons (ENP $ EnumeratedPropertyType idProperty) properties
    else pure properties
  -- All instances of this RoleType (object) the user may see in this context.
  -- In general, these may be instances of several role types, as the perspective object is expressed as a query.
  (rinstances :: Array (DependencyPath)) <- liftToMPT ((singletonPath (C cid)) ##= interpret object)
  -- Serialise all the dependencies.
  -- If the perspective is selfOnly, then each of the users can only receive his own role and properties.
  -- This means that the users and (the role instances in) rinstances should be the same collection.
  -- in that case, call serialiseDependency on a pair consisting of the instance (one of rinstances, each represented by a Dependency) and a user that is that same instance.
  if selfOnly
    then do
      for_ (join (allPaths <$> rinstances))
        \(dependencies :: NonEmptyList Dependency) -> do
          -- The head will be a user role because of selfOnly.
          oneUserOnly <- unsafePartial case head dependencies of
            R r -> pure [r]
          serialiseDependencies oneUserOnly dependencies
          for_ properties'
            \pt -> for_ (_.head <$> rinstances)
              \(dep ::Dependency) -> do
                (vals :: Array DependencyPath) <- liftToMPT ((singletonPath dep) ##= getPropertyValues pt)
                for_ (join (allPaths <$> vals)) (serialiseDependencies oneUserOnly)
    else do
      for_ (join (allPaths <$> rinstances)) (serialiseDependencies (toArray users))
      for_ properties'
        \pt -> for_ (_.head <$> rinstances)
          \(dep :: Dependency) -> do
            (vals :: Array DependencyPath) <- liftToMPT ((singletonPath dep) ##= getPropertyValues pt)
            for_ (join (allPaths <$> vals)) (serialiseDependencies (toArray users))

getPropertyValues :: PropertyType -> DependencyPath ~~> DependencyPath
getPropertyValues pt dep = do
  calc <- lift $ lift $ (PClass.getProperty >=> PClass.getCalculation) pt
  interpret calc dep

serialiseDependencies :: Array RoleInstance -> NonEmptyList Dependency -> MonadPerspectivesTransaction Unit
serialiseDependencies users deps = void $ runStateT (serialiseDependencies_ users deps) []

serialiseDependencies_ ::
  Array RoleInstance ->
  NonEmptyList Dependency ->
  StateT (Array Dependency) MonadPerspectivesTransaction Unit
serialiseDependencies_ users deps = void $ foldM
  (serialiseDependency users)
  Nothing
  deps

-- Always returns the second argument in Maybe.
-- | `users` will not always be model:System$PerspectivesSystem$User instances.
serialiseDependency ::
  Array RoleInstance ->
  Maybe Dependency ->
  Dependency ->
  StateT (Array Dependency) MonadPerspectivesTransaction (Maybe Dependency)
serialiseDependency users mpreviousDependency currentDependency = do
  -- We serialise a role dependency as soon as we see it, hence we analyse the currentDependency.
  case currentDependency of
    (R roleId) -> do
      seenBefore <- gets \depsSeenBefore -> isJust $ elemIndex currentDependency depsSeenBefore
      if seenBefore
        then pure unit
        else lift $ addDeltasForRole roleId
    otherwise -> pure unit

  case mpreviousDependency, currentDependency of
    Just first@(R roleId1), (R roleId2) -> do
      lift $ addBindingDelta roleId1 roleId2 >>= if _
        then pure unit
        else addBindingDelta roleId2 roleId1 >>= if _
          then pure unit
          else log ("serialiseDependency finds two role dependencies without binding: " <> show mpreviousDependency <> ", " <> show currentDependency)
    Just (V ptypeString (Value val)), (R roleId) -> lift $ addPropertyDelta roleId ptypeString val
    _, _ -> pure unit
  pure $ Just currentDependency

  where
    -- | Returns true iff the binding of the first argument equals the second argument.
    addBindingDelta :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Boolean
    addBindingDelta roleId1 roleId2 = (liftToMPT $ try $ getPerspectRol roleId1) >>= handlePerspectRolError' "addBindingDelta" false
      \(PerspectRol{binding, bindingDelta}) -> case binding of
        Just b -> if b == roleId2
          then traverse_ (\bd -> addDelta $ DeltaInTransaction {users, delta: bd}) bindingDelta *> pure true
          else pure false
        Nothing -> pure false

    addPropertyDelta :: RoleInstance -> PropertyName -> String -> MonadPerspectivesTransaction Unit
    addPropertyDelta roleId ptypeString val = (liftToMPT $ try $ getPerspectRol roleId) >>=
      handlePerspectRolError "addPropertyDelta"
        \(PerspectRol{propertyDeltas}) -> case lookup ptypeString propertyDeltas of
          Nothing -> pure unit
          Just x -> case lookup val x of
            Nothing -> throwError (error $ "No propertyDelta for value " <> val <> " on " <> show roleId)
            Just pdelta -> addDelta $ DeltaInTransaction {users, delta: pdelta}

    addDeltasForRole :: RoleInstance -> MonadPerspectivesTransaction Unit
    addDeltasForRole roleId = do
      -- Todo: serialise the external role.
      (liftToMPT $ try $ getPerspectRol roleId) >>=
        handlePerspectRolError "addDeltasForRole"
          \(PerspectRol{context, universeRoleDelta, contextDelta}) -> do
            (liftToMPT $ try $ getPerspectContext context) >>=
              handlePerspectContextError "addDeltasForRole"
                \(PerspectContext{universeContextDelta, buitenRol}) -> do
                  (liftToMPT $ try $ getPerspectRol buitenRol) >>=
                    handlePerspectRolError "addDeltasForRole"
                      \(PerspectRol{universeRoleDelta: eRoleDelta}) -> do
                        -- ORDER IS OF THE ESSENCE, HERE!!
                        addDelta $ DeltaInTransaction {users, delta: eRoleDelta}
                        addDelta $ DeltaInTransaction {users, delta: universeContextDelta}
                        addDelta $ DeltaInTransaction {users, delta: universeRoleDelta}
                        addDelta $ DeltaInTransaction {users, delta: contextDelta}

    withContext :: Boolean
    withContext = true

    withoutContext :: Boolean
    withoutContext = false

type PropertyName = String
