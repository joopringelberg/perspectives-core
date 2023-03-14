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

module Perspectives.Deltas where

import Control.Monad.AvarMonadAsk (modify, gets) as AA
import Control.Monad.State.Trans (StateT, execStateT, get, lift, put)
import Data.Array (catMaybes, elemIndex, filter, filterA, foldl, insertAt, length, nub, snoc, union)
import Data.DateTime.Instant (toDateTime)
import Data.Map (insert, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (over)
import Data.Traversable (for, for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Now (now)
import Foreign.Generic (encodeJSON)
import Foreign.Object (Object, empty, insert, lookup)
import Foreign.Object (filter) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.AMQP.Stomp (sendToTopic)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>))
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.ObjectGetters (bottom_, getProperty, roleType_)
import Perspectives.ModelDependencies (connectedToAMQPBroker, userChannel, sysUser) as DEP
import Perspectives.Names (getMySystem)
import Perspectives.Persistence.API (Url, addDocument)
import Perspectives.Persistent (postDatabaseName)
import Perspectives.PerspectivesState (nextTransactionNumber, stompClient)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), addToTransactionForPeer, transactieID)
import Perspectives.Types.ObjectGetters (isPublicRole)
import Prelude (Unit, bind, discard, eq, flip, map, not, notEq, pure, show, unit, void, ($), (*>), (<$>), (<<<), (<>), (==), (>>>), (>>=), (<*>), (>=>), (||))

-- | Splits the transaction in versions specific for each peer and sends them.
-- | If a public roles are involved, will return their TransactionForPeer instances.
-- IMPLEMENTATION NOTICE: we cannot handle these instances here by calling executeTransactionForPublicRole
-- because that would need cyclic model imports.
distributeTransaction :: Transaction -> MonadPerspectives TransactionPerUser
distributeTransaction t@(Transaction{changedDomeinFiles}) = do
  for_ changedDomeinFiles (DomeinFileId >>> saveCachedDomeinFile)
  -- Send the Transaction to all involved.
  distributeTransactie' t
  
distributeTransactie' :: Transaction -> MonadPerspectives TransactionPerUser
distributeTransactie' t = do
  (customizedTransacties :: TransactionPerUser) <- transactieForEachUser t
  map (unsafePartial fromJust) <<< OBJ.filter isJust <$> forWithIndex customizedTransacties sendTransactie

-- | If we have the visitor user, handle it by augmenting resources in the public store with the deltas.
sendTransactie :: String -> TransactionForPeer -> MonadPerspectives (Maybe TransactionForPeer)
sendTransactie userId t = do 
  userType <- roleType_ (RoleInstance userId)
  if userType == (EnumeratedRoleType DEP.sysUser)
    then sendTransactieToUserUsingAMQP userId t *> pure Nothing
    else pure $ Just t

-- | Send a transaction using the Couchdb Channel.
sendTransactieToUserUsingCouchdb :: Url -> String -> TransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingCouchdb cdbUrl userId t = do
  -- TODO controleer of hier authentication nodig is!
  userType <- roleType_ (RoleInstance userId)
  getChannel <- getDynamicPropertyGetter DEP.userChannel (ST $ userType)
  mchannel <- (RoleInstance userId) ##> getChannel
  case mchannel of
    Nothing -> pure unit
    Just (Value channel) -> do
      transactionNumber <- nextTransactionNumber
      void $ addDocument (cdbUrl <> channel) t (transactieID t <> "_" <> show transactionNumber)

-- | `userId` WILL be either 
-- |   * a model://perspectives.domains#System$PerspectivesSystem$User instance, or
-- |   * an instance of the Visitor role.
sendTransactieToUserUsingAMQP :: String -> TransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingAMQP userId t = do
  connected <- connectedToAMQPBroker
  n <- liftEffect $ now
  dt <- pure $ SerializableDateTime (toDateTime n)
  messageId <- pure $ userId <> show dt
  if connected
    then do
      mstompClient <- stompClient
      case mstompClient of
        Just stompClient -> do
          saveTransactionInOutgoingPost userId messageId t
          liftEffect $ sendToTopic stompClient userId messageId (encodeJSON t)
        otherwise -> saveTransactionInOutgoingPost userId messageId t
    else saveTransactionInOutgoingPost userId messageId t

  where
    connectedToAMQPBroker :: MonadPerspectives Boolean
    connectedToAMQPBroker = do
      mySystem <- getMySystem
      mConnected <- (RoleInstance $ buitenRol mySystem) ##> getProperty (EnumeratedPropertyType DEP.connectedToAMQPBroker)
      pure $ mConnected == (Just $ Value "true")

saveTransactionInOutgoingPost :: String -> String -> TransactionForPeer -> MonadPerspectives Unit
saveTransactionInOutgoingPost userId messageId t = do
  postDB <- postDatabaseName
  void $ addDocument postDB (OutgoingTransaction{_id: messageId, receiver: userId, transaction: t}) messageId

-- | An object of TransactionForPeer where the keys are the string value of RoleInstances, invariably identifying user roles.
-- | Must be either an instance of sys:PerspectivesSystem$User, or of a RoleInstance of a type with RoleKind Visitor.
type TransactionPerUser = Object TransactionForPeer

-- | The Transaction holds Deltas and each Delta names user instances who should receive that Delta.
-- | This function builds a custom version of the Transaction for each such user.
-- | `users` in DeltaInTransaction will not always be model://perspectives.domains#System$PerspectivesSystem$User instances.
transactieForEachUser :: Transaction -> MonadPerspectives TransactionPerUser
transactieForEachUser t@(Transaction tr@{author, timeStamp, deltas, userRoleBottoms}) = do
  execStateT (for_ deltas \(DeltaInTransaction{users, delta}) -> do
    -- Lookup the ultimate filler for all users in the delta.
    sysUsers <- pure $ catMaybes (flip Map.lookup userRoleBottoms <$> users)
    addDeltaToCustomisedTransactie delta (nub sysUsers))
    empty
  where
    -- `sysUsers` WILL be either 
    --    * model://perspectives.domains#System$PerspectivesSystem$User instances, or
    --    * instances of the Visitor role.
    addDeltaToCustomisedTransactie :: SignedDelta -> (Array RoleInstance) -> StateT TransactionPerUser (MonadPerspectives) Unit
    addDeltaToCustomisedTransactie d@(SignedDelta {author: deltaAuthor}) sysUsers = for_
      sysUsers
      (\(RoleInstance sysUser) -> if not $ eq sysUser deltaAuthor
        then do
          trs <- get
          case lookup sysUser trs of
            Nothing -> put $ insert sysUser (TransactionForPeer {author, timeStamp, deltas: [d]}) trs
            Just trans -> put $ insert sysUser (addToTransactionForPeer d trans) trs
        else pure unit
      )

addDomeinFileToTransactie :: ID -> MonadPerspectivesTransaction Unit
addDomeinFileToTransactie dfId = AA.modify (over Transaction \(t@{changedDomeinFiles}) ->
  t {changedDomeinFiles = union changedDomeinFiles [dfId]})

-- | Add the delta at the end of the array, unless it is already in the transaction!
addDelta :: DeltaInTransaction -> MonadPerspectivesTransaction Unit
addDelta dt@(DeltaInTransaction{users}) = do
  newUserBottoms <- (for users \user -> Tuple user <$> lift (bottom_ user)) >>= filterA isUserBottomOrPublic
  AA.modify (over Transaction \t@{deltas, userRoleBottoms} -> t 
    { deltas =
      if isJust $ elemIndex dt deltas
        then deltas
        else snoc deltas dt
    , userRoleBottoms = foldl (\userBottoms' (Tuple role user) -> Map.insert role user userBottoms') userRoleBottoms newUserBottoms
    })

-- bottom_ can be idempotent if a role has no binding. Hence we filter away the tuples where fst == snd.
-- However, we should keep roles that are a public role proxy.
isUserBottomOrPublic :: Tuple RoleInstance RoleInstance -> MonadPerspectivesTransaction Boolean
isUserBottomOrPublic (Tuple r b) = (||) <$> (pure (r `notEq` b)) <*> (lift $ (roleType_ >=> isPublicRole) r)

-- | Insert the delta at the index, unless it is already in the transaction.
insertDelta :: DeltaInTransaction -> Int -> MonadPerspectivesTransaction Unit
insertDelta dt@(DeltaInTransaction{users}) i = do
  newUserBottoms <- lift $ filter (\(Tuple r b) -> r `notEq` b) <$> (for users \user -> Tuple user <$> bottom_ user)  
  AA.modify (over Transaction \t@{deltas, userRoleBottoms} -> t 
    { deltas =
      if isJust $ elemIndex dt deltas
        then deltas
        else unsafePartial $ fromJust $ insertAt i dt deltas
    , userRoleBottoms = foldl (\userBottoms' (Tuple role user) -> Map.insert role user userBottoms') userRoleBottoms newUserBottoms})

-- | Instrumental for QUERY UPDATES.
addCorrelationIdentifiersToTransactie :: Array CorrelationIdentifier -> MonadPerspectivesTransaction Unit
addCorrelationIdentifiersToTransactie corrIds = AA.modify (over Transaction \t@{correlationIdentifiers} -> t {correlationIdentifiers = union correlationIdentifiers corrIds})

-- | Give the number of SignedDeltas in the Transaction.
deltaIndex :: MonadPerspectivesTransaction Int
deltaIndex = AA.gets \(Transaction{deltas}) -> length deltas

addCreatedContextToTransaction :: ContextInstance -> MonadPerspectivesTransaction Unit
addCreatedContextToTransaction cid =
  AA.modify (over Transaction \t@{createdContexts} -> t {createdContexts =
    if isJust $ elemIndex cid createdContexts
      then createdContexts
      else snoc createdContexts cid})

addCreatedRoleToTransaction :: RoleInstance -> MonadPerspectivesTransaction Unit
addCreatedRoleToTransaction rid =
  AA.modify (over Transaction \t@{createdRoles} -> t {createdRoles =
    if isJust $ elemIndex rid createdRoles
      then createdRoles
      else snoc createdRoles rid})
