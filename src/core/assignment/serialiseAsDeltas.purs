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

module Perspectives.Assignment.SerialiseAsDeltas where

import Control.Monad.AvarMonadAsk (get) as AMA
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array.Partial (head) as PA
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (foldM)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.AVar (new)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Class (encode)
import Foreign.Object (lookup)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectives, (##>>), (###=), type (~~>), (##=))
import Perspectives.Deltas (addDelta)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (bottom, roleType_)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Query.Interpreter (interpret)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, allPaths, singletonPath)
import Perspectives.Representation.Class.Property (getProperty, getCalculation) as PClass
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), PropertyType, RoleType(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.Transaction (Transaction(..), createTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.Types.ObjectGetters (perspectivesClosure_, propertiesInPerspective)
import Prelude (Unit, bind, discard, join, pure, show, unit, void, ($), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=))

serialisedAsDeltasFor :: ContextInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor cid userId = do
  userType <- lift $ lift $  roleType_ userId
  systemUser <- lift $ lift $ (userId ##>> bottom)
  serialisedAsDeltasFor_ cid systemUser (ENR userType)

-- | Construct a Transaction that represents a context for a particular user role.
-- | Serialise the Transaction as a string.
serialisedAsDeltasForUserType :: ContextInstance -> RoleType -> MonadPerspectives Value
serialisedAsDeltasForUserType cid userType = do
  me <- getUserIdentifier
  (Transaction{author, timeStamp, deltas}) <- execMonadPerspectivesTransaction
    -- The authoringRole is used on *constructing* deltas. However, here we merely *read* deltas from the
    -- context- and role representations. So this value is in effect ignored.
    (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
    -- NOTE: we provide serialisedAsDeltasFor_ with the value (RoleInstance me) as
    -- identifier for the user for whom we serialise. As we don't know the real
    -- user identifier (we serialise for a type!) we use it as a stand in.
    (serialisedAsDeltasFor_ cid (RoleInstance me) userType)
  tfp <- pure $ TransactionForPeer
    { author
    , timeStamp
    , deltas: _.delta <<< unwrap <$> deltas
  }
  pure $ Value $ unsafeStringify $ encode tfp
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
      >>= runReaderT (runArrayT run)
      >>= pure <<< unsafePartial PA.head
        where
          run :: MonadPerspectivesTransaction Transaction
          run = do
            void a
            lift AMA.get


liftToMPT :: forall a. MonadPerspectives a -> MonadPerspectivesTransaction a
liftToMPT = lift <<< lift

serialisedAsDeltasFor_:: ContextInstance -> RoleInstance -> RoleType -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor_ cid userId userType =
  -- All Roletypes the user may see in this context, expressed as Perspectives.
   (liftToMPT (userType ###= perspectivesClosure_) >>= traverse_ (serialisePerspectiveForUser cid [userId]))

-- | Add Deltas to the transaction for the given users, to provide them with a complete
-- | account of the perspective on the context instance.
serialisePerspectiveForUser :: ContextInstance -> Array RoleInstance -> Perspective -> MonadPerspectivesTransaction Unit
serialisePerspectiveForUser cid users p@(Perspective{object, propertyVerbs}) = do
  -- All instances of this RoleType the user may see in this context.
  (rinstances :: Array (DependencyPath)) <- liftToMPT ((singletonPath (C cid)) ##= interpret object)
  -- Serialise all the dependencies.
  for_ (join (allPaths <$> rinstances)) (foldM (serialiseDependency users) Nothing)
  -- All PropertyTypes on this RoleType the user may see.
  (visiblePropertyTypes :: Array PropertyType) <- liftToMPT $ propertiesInPerspective p
  -- Compute all values and serialise the dependencies.
  for_ visiblePropertyTypes
    \pt -> do
      for_ (_.head <$> rinstances)
        \(dep :: Dependency) -> do
          (vals :: Array DependencyPath) <- liftToMPT ((singletonPath dep) ##= getPropertyValues pt)
          for_ (join (allPaths <$> vals)) (foldM (serialiseDependency users) Nothing)
  pure unit

getPropertyValues :: PropertyType -> DependencyPath ~~> DependencyPath
getPropertyValues pt dep = do
  calc <- lift $ lift $ (PClass.getProperty >=> PClass.getCalculation) pt
  interpret calc dep

-- Always returns the second argument in Maybe.
serialiseDependency :: Array RoleInstance ->  Maybe Dependency -> Dependency -> MonadPerspectivesTransaction (Maybe Dependency)
serialiseDependency users mprevious d = do
  case mprevious, d of

    -- For the first member we have the following 4 possibilities: Nothing + Context, Role and Val.
    -- For the second member we have just Context, Role and Val.
    -- These combine to 12 possible pairs.
    -- The four cases that end on Role 4 are handled below.
    -- None of the 4 cases that have Val as second member need to be handled; they would represent
    -- queries that continue from a value and these do not occur.
    --
    -- That leaves us with 4 cases whose second member is context. We can rule out (Nothing, Context), because
    -- it would represent a query that results in a context and these do not occur.
    -- But the other (first) member *must* be Role, because the only way we can hit upon a Context is through one
    -- of its Roles. So we have only one case left to cover: (Role, Context).
    -- Obviously, we've already handled Role (when it was the second member and we handle all 4 such cases).
    -- As we handle a role, we serialise its context, too.
    -- So the conclusion is that we need not do anything for these other 4 cases.

    Just (R roleId1), (R roleId2) -> do
      addDeltasForRole roleId2
      addBindingDelta roleId1 roleId2 >>= if _
        then pure unit
        else addBindingDelta roleId2 roleId1 >>= if _
          then pure unit
          else log ("serialiseDependency finds two role dependencies without binding: " <> show mprevious <> ", " <> show d)

    Just (V ptypeString (Value val)), (R roleId) -> do
      addDeltasForRole roleId
      addPropertyDelta roleId ptypeString val

    -- Two cases: Nothing and Context.
    _, (R roleId) -> addDeltasForRole roleId

    -- All cases we can ignore (see above).
    _, _ -> pure unit
  pure $ Just d

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
