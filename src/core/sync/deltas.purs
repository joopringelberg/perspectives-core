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
import Data.Array (elemIndex, insertAt, length, nub, snoc, union)
import Data.DateTime.Instant (toDateTime)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (over)
import Data.Traversable (for_)
import Data.TraversableWithIndex (forWithIndex)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Foreign.Generic (encodeJSON)
import Foreign.Object (Object, empty, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.AMQP.Stomp (sendToTopic)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>), (##=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.DomeinFile (DomeinFileId(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.ObjectGetters (bottom, getProperty, roleType_)
import Perspectives.Names (getMySystem)
import Perspectives.Persistence.API (addDocument)
import Perspectives.Persistent (postDatabaseName)
import Perspectives.PerspectivesState (nextTransactionNumber, stompClient)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), addToTransactionForPeer, transactieID)
import Perspectives.User (getCouchdbBaseURL)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (<>), (>=>), (>>>), not, eq, (==))

distributeTransaction :: Transaction -> MonadPerspectives Unit
distributeTransaction t@(Transaction{changedDomeinFiles}) = do
  for_ changedDomeinFiles (DomeinFileId >>> saveCachedDomeinFile)
  -- Send the Transaction to all involved.
  distributeTransactie' t
  pure unit

distributeTransactie' :: Transaction -> MonadPerspectives Unit
distributeTransactie' t = do
  (customizedTransacties :: TransactionPerUser) <- transactieForEachUser t
  _ <- forWithIndex customizedTransacties sendTransactieToUserUsingAMQP
  pure unit

-- | Send a transaction using the Couchdb Channel.
sendTransactieToUserUsingCouchdb :: String -> TransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingCouchdb userId t = do
  -- TODO controleer of hier authentication nodig is!
  userType <- roleType_ (RoleInstance userId)
  getChannel <- getDynamicPropertyGetter "model:System$PerspectivesSystem$User$Channel" (ST $ userType)
  mchannel <- (RoleInstance userId) ##> getChannel
  case mchannel of
    Nothing -> pure unit
    Just (Value channel) -> do
      cdbUrl <- getCouchdbBaseURL
      transactionNumber <- nextTransactionNumber
      void $ addDocument (cdbUrl <> channel) t (transactieID t <> "_" <> show transactionNumber)

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
      mConnected <- (RoleInstance $ buitenRol mySystem) ##> getProperty (EnumeratedPropertyType "model:System$PerspectivesSystem$External$ConnectedToAMQPBroker")
      pure $ mConnected == (Just $ Value "true")

saveTransactionInOutgoingPost :: String -> String -> TransactionForPeer -> MonadPerspectives Unit
saveTransactionInOutgoingPost userId messageId t = do
  postDB <- postDatabaseName
  void $ addDocument postDB (OutgoingTransaction{ receiver: userId, transaction: t}) messageId

type TransactionPerUser = Object TransactionForPeer

-- | The Transaction holds Deltas and each Delta names user instances who should receive that Delta.
-- | This function builds a custom version of the Transaction for each such user.
transactieForEachUser :: Transaction -> MonadPerspectives TransactionPerUser
transactieForEachUser t@(Transaction tr@{author, timeStamp, deltas}) = do
  execStateT (for_ deltas \(DeltaInTransaction{users, delta}) -> do
      sysUsers <- lift (unit ##= (\_ -> ArrayT (pure users)) >=> bottom)
      addDeltaToCustomisedTransactie delta (nub sysUsers))
    empty
  where
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
addDomeinFileToTransactie dfId = lift $ AA.modify (over Transaction \(t@{changedDomeinFiles}) ->
  t {changedDomeinFiles = union changedDomeinFiles [dfId]})

-- | Add the delta at the end of the array, unless it is already in the transaction!
addDelta :: DeltaInTransaction -> MonadPerspectivesTransaction Unit
addDelta dt =
  lift $ AA.modify (over Transaction \t@{deltas} -> t {deltas =
    if isJust $ elemIndex dt deltas
      then deltas
      else snoc deltas dt})

-- | Insert the delta at the index, unless it is already in the transaction.
insertDelta :: DeltaInTransaction -> Int -> MonadPerspectivesTransaction Unit
insertDelta dt i =
  lift $ AA.modify (over Transaction \t@{deltas} -> t {deltas =
    if isJust $ elemIndex dt deltas
      then deltas
      else unsafePartial $ fromJust $ insertAt i dt deltas})

-- | Instrumental for QUERY UPDATES.
addCorrelationIdentifiersToTransactie :: Array CorrelationIdentifier -> MonadPerspectivesTransaction Unit
addCorrelationIdentifiersToTransactie corrIds = lift $ AA.modify (over Transaction \t@{correlationIdentifiers} -> t {correlationIdentifiers = union correlationIdentifiers corrIds})

-- | Give the number of SignedDeltas in the Transaction.
deltaIndex :: MonadPerspectivesTransaction Int
deltaIndex = lift $ AA.gets \(Transaction{deltas}) -> length deltas

addCreatedContextToTransaction :: ContextInstance -> MonadPerspectivesTransaction Unit
addCreatedContextToTransaction cid =
  lift $ AA.modify (over Transaction \t@{createdContexts} -> t {createdContexts =
    if isJust $ elemIndex cid createdContexts
      then createdContexts
      else snoc createdContexts cid})

-- Procedure om Delta's zuinig toe te voegen.
-- 2. Bepaal of de rol functioneel is.
-- ZO JA:
-- 3. Zoek een delta waarvan de id, rolName en DeltaType gelijk zijn aan die van de nieuwe.
-- 	Indien gevonden: vervang die door de nieuwe.
-- 	Indien niet gevonden: zoek een delta waarvan id en rolName overeenkomen.
-- 		Indien gevonden, als geldt:
-- 			het ene DeltaType is Add en het andere Remove, verwijder dan de oude.
-- 			het oude DeltaType is Change en het nieuwe Remove, vervang de oude dan door de nieuwe
-- 			het oude DeltaType is Add en het nieuwe is Change, vervang dan in de oude de rolID door die van de nieuwe.
-- 		Indien niet gevonden: voeg de nieuwe toe.
-- ZO NEE:
-- 4. zoek een delta waarvan id, rolName en rolID gelijk zijn aan die van de nieuwe en het ene DeltaType Add is en het andere Remove.
-- 	Indien gevonden: verwijder de oude.
-- 	Anders: voeg de nieuwe toe.
-- | Add a Delta to the Transaction. Tries to keep the Transaction as small as possible, by eliminating and integrating
-- | Delta's that affect the same Role or Property.
-- | Modify a Triple that represents a basic fact in the TripleAdministration.
-- | Add that Triple to the TripleQueue.
-- TODO. Dit werkt voor de generieke Delta, maar niet voor de specifieke ContextDelta, RoleBindingDelta, enz.
-- addDelta :: Delta -> MonadPerspectives Unit
-- addDelta newCD@(Delta{id: id', memberName, deltaType, value, isContext}) = do
--   t@(Transaction tf@{deltas}) <- transactie
--   case elemIndex newCD deltas of
--     (Just _) -> pure unit
--     Nothing -> do
--       -- TODO. Maak de DeltaMonad en push hier een delta? Of in de functies die addDelta-functies aanroepen?
--       -- Ergens moet de veranderde assumptie geregistreerd worden.
--       maybeM (pure unit) addTripleToQueue (lift $ liftEffect $ modifyTriple newCD)
--       (isfunc :: Boolean) <- case isContext of
--         true -> getPerspectType (EnumeratedRoleType memberName) >>= \(r :: EnumeratedRole) -> R.functional r
--         false -> getPerspectType (EnumeratedPropertyType memberName) >>= \(p :: EnumeratedProperty) -> P.functional p
--       -- isfunc <- isFunctionalComputer memberName -- hier komt ie niet uit.
--       -- (isfunc :: Boolean) <- evalMonadPerspectivesQuery memberName (toBoolean (if isContext then rolIsFunctioneelM else propertyIsFunctioneelM ))
--       if (isfunc)
--         then do
--           x <- pure $ findIndex equalExceptRolID deltas
--           case x of
--             (Just oldCD) -> setTransactie (replace oldCD newCD t)
--             Nothing -> do
--               mCdelta <- pure $ find equalIdRolName deltas
--               case mCdelta of
--                 Nothing -> setTransactie (add newCD t)
--                 (Just oldCD@(Delta oldF@{deltaType: d})) -> do
--                   indexOld <- pure (unsafePartial (fromJust (elemIndex oldCD deltas)))
--                   case d of
--                     Add | (deltaType == Remove) -> setTransactie (remove oldCD t)
--                     Remove | (deltaType == Add) -> setTransactie (remove oldCD t)
--                     Change | (deltaType == Remove) -> setTransactie (replace indexOld newCD t)
--                     Add | (deltaType == Change) -> setTransactie (replace indexOld (Delta oldF {value = value}) t)
--                     otherwise -> setTransactie (add newCD t)
--         else do
--           x <- pure $ findIndex equalExceptDeltaType deltas
--           case x of
--             Nothing -> setTransactie (add newCD t)
--             (Just i) -> setTransactie (replace i newCD t)
--   pure unit
--   where
--     equalExceptDeltaType :: Delta  -> Boolean
--     equalExceptDeltaType
--       (Delta{id: i, memberName: r, deltaType: d, value: ri}) =
--         id' == i &&
--         memberName == r &&
--         value == ri &&
--         ((deltaType == Add && d == Remove) || (deltaType == Remove && d == Add))
--     equalExceptRolID :: Delta -> Boolean
--     equalExceptRolID
--       (Delta{id: i, memberName: r, deltaType: d}) =
--         id' == i &&
--         memberName == r &&
--         deltaType == d
--     equalIdRolName :: Delta -> Boolean
--     equalIdRolName (Delta{id: i, memberName: r}) =
--       id' == i &&
--       memberName == r
--     add :: Delta -> Transaction -> Transaction
--     add delta (Transaction tf@{deltas}) = Transaction tf {deltas = cons delta deltas}
--     replace :: Int -> Delta -> Transaction -> Transaction
--     replace i delta (Transaction tf@{deltas}) = Transaction tf {deltas = cons newCD (maybe deltas identity (deleteAt i deltas))}
--     remove :: Delta -> Transaction -> Transaction
--     remove i (Transaction tf@{deltas}) = Transaction tf {deltas = (delete i deltas)}
