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

module Perspectives.Assignment.SerialiseAsDeltas where

import Control.Monad.AvarMonadAsk (get) as AMA
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (StateT, evalStateT, modify, get)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, elemIndex)
import Data.Array.NonEmpty (fromArray)
import Data.Array.Partial (head) as PA
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Effect.Aff.AVar (new)
import Foreign.Class (encode)
import Foreign.Object (lookup)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectives, (###>>), (##>>))
import Perspectives.Deltas (addDelta)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (bottom, context, roleType, roleType_)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..), externalRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), externalRoleType)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.Transaction (Transaction(..), createTransactie)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.Types.ObjectGetters (propertyIsInPerspectiveOf, roleIsInPerspectiveOf)
import Prelude (Unit, bind, discard, pure, unit, void, when, ($), (>>=), (<<<), (<$>))

serialisedAsDeltasFor :: ContextInstance -> RoleInstance -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor cid userId = do
  userType <- lift2 $ roleType_ userId
  systemUser <- lift2 (userId ##>> bottom)
  runSerialiser $ serialisedAsDeltasFor_ cid systemUser (ENR userType)

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
    (runSerialiser $ serialisedAsDeltasFor_ cid (RoleInstance me) userType)
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
      >>= lift <<< createTransactie authoringRole
      >>= lift <<< new
      >>= runReaderT (runArrayT run)
      >>= pure <<< unsafePartial PA.head
        where
          run :: MonadPerspectivesTransaction Transaction
          run = do
            void a
            lift AMA.get

type Serializer = StateT (Array ContextInstance) MonadPerspectivesTransaction

liftToSerialiser :: forall a. MonadPerspectives a -> Serializer a
liftToSerialiser = lift <<< lift <<< lift

runSerialiser :: forall a. Serializer a -> MonadPerspectivesTransaction a
runSerialiser x = evalStateT x []

serialisedAsDeltasFor_:: ContextInstance -> RoleInstance -> RoleType -> Serializer Unit
serialisedAsDeltasFor_ cid userId userType = do
  seenBefore <- get
  when (isNothing $ elemIndex cid seenBefore)
    do
      void $ modify (cons cid)
      PerspectContext{universeContextDelta, pspType, rolInContext} <- liftToSerialiser $ getPerspectContext cid
      lift $ addDelta $ DeltaInTransaction {users: [userId], delta: universeContextDelta}
      -- Serialise the external role.
      serialiseRoleInstance cid (externalRoleType pspType) false (externalRole cid)
      -- Now for each role, decide if the user may see it.
      -- If so, add a UniverseRoleDelta and a ContextDelta.
      forWithIndex_ rolInContext \roleTypeId roleInstances' -> do
        allowed <- liftToSerialiser (userType ###>> roleIsInPerspectiveOf (ENR $ EnumeratedRoleType roleTypeId))
        if allowed
          then do
            case fromArray roleInstances' of
              Nothing -> pure unit
              Just roleInstances -> do
                for_ roleInstances (serialiseRoleInstance cid (EnumeratedRoleType roleTypeId) true)
          else pure unit

  where
  serialiseRoleInstance :: ContextInstance -> EnumeratedRoleType -> Boolean -> RoleInstance -> Serializer Unit
  serialiseRoleInstance cid' roleTypeId isNotExternal roleInstance = do
    PerspectRol{binding, properties, universeRoleDelta, contextDelta, bindingDelta, propertyDeltas} <- liftToSerialiser $ getPerspectRol roleInstance
    lift $ addDelta $ DeltaInTransaction { users: [userId], delta: universeRoleDelta}
    -- Not for external roles!
    when isNotExternal (lift $ addDelta  $ DeltaInTransaction { users: [userId], delta: contextDelta })
    case binding of
      Nothing -> pure unit
      Just b -> do
        c <- liftToSerialiser (b ##>> context)
        typeOfBinding <- liftToSerialiser (b ##>> roleType)
        shouldBeSent <- liftToSerialiser (userType ###>> roleIsInPerspectiveOf (ENR typeOfBinding))
        when shouldBeSent do
          -- TODO. Serialiseer de context niet als de ander er al een rol bij speelt!
          serialisedAsDeltasFor_ c userId userType
          case bindingDelta of
            Nothing -> pure unit
            Just bd -> lift $ addDelta $ DeltaInTransaction {users: [userId], delta: bd }
    -- For each set of Property Values, add a RolePropertyDelta if the user may see it.
    forWithIndex_ properties \propertyTypeId values -> do
      propAllowed <- liftToSerialiser (userType ###>> propertyIsInPerspectiveOf (ENP (EnumeratedPropertyType propertyTypeId)))
      if propAllowed
        then case lookup propertyTypeId propertyDeltas of
          Nothing -> pure unit
          Just deltas -> for_ deltas \pd -> lift $ addDelta $ DeltaInTransaction {users: [userId], delta: pd}
        else pure unit
