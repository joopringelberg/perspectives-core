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
import Data.Array (catMaybes, elemIndex, filter, foldl, insertAt, length, nub, snoc, union)
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
import Partial.Unsafe (unsafePartial)
import Perspectives.AMQP.Stomp (sendToTopic)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>))
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.ObjectGetters (bottom_, getProperty, roleType_)
import Perspectives.Names (getMySystem)
import Perspectives.Persistence.API (Url, addDocument)
import Perspectives.Persistent (postDatabaseName)
import Perspectives.PerspectivesState (nextTransactionNumber, stompClient)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedPropertyType(..))
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), addToTransactionForPeer, transactieID)
import Prelude (Unit, bind, discard, eq, flip, not, notEq, pure, show, unit, void, ($), (<$>), (<>), (==), (>>>))

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
sendTransactieToUserUsingCouchdb :: Url -> String -> TransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingCouchdb cdbUrl userId t = do
  -- TODO controleer of hier authentication nodig is!
  userType <- roleType_ (RoleInstance userId)
  getChannel <- getDynamicPropertyGetter "model:System$PerspectivesSystem$User$Channel" (ST $ userType)
  mchannel <- (RoleInstance userId) ##> getChannel
  case mchannel of
    Nothing -> pure unit
    Just (Value channel) -> do
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
  void $ addDocument postDB (OutgoingTransaction{_id: messageId, receiver: userId, transaction: t}) messageId

type TransactionPerUser = Object TransactionForPeer

-- | The Transaction holds Deltas and each Delta names user instances who should receive that Delta.
-- | This function builds a custom version of the Transaction for each such user.
-- | `users` in DeltaInTransaction will not always be model:System$PerspectivesSystem$User instances.
transactieForEachUser :: Transaction -> MonadPerspectives TransactionPerUser
transactieForEachUser t@(Transaction tr@{author, timeStamp, deltas, userRoleBottoms}) = do
  execStateT (for_ deltas \(DeltaInTransaction{users, delta}) -> do
    sysUsers <- pure $ catMaybes (flip Map.lookup userRoleBottoms <$> users)
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
addDomeinFileToTransactie dfId = AA.modify (over Transaction \(t@{changedDomeinFiles}) ->
  t {changedDomeinFiles = union changedDomeinFiles [dfId]})

-- | Add the delta at the end of the array, unless it is already in the transaction!
addDelta :: DeltaInTransaction -> MonadPerspectivesTransaction Unit
addDelta dt@(DeltaInTransaction{users}) = do
  -- bottom_ can be idempotent if a role has no binding. Hence we filter away the tuples where fst == snd.
  newUserBottoms <- lift $ filter (\(Tuple r b) -> r `notEq` b) <$> (for users \user -> Tuple user <$> bottom_ user)  
  AA.modify (over Transaction \t@{deltas, userRoleBottoms} -> t 
    { deltas =
      if isJust $ elemIndex dt deltas
        then deltas
        else snoc deltas dt
    , userRoleBottoms = foldl (\userBottoms' (Tuple role user) -> Map.insert role user userBottoms') userRoleBottoms newUserBottoms
    })

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

-- Procedure om Delta's zuinig toe te voegen.
-- 2. Bepaal of de rol functioneel is.
-- ZO JA:
-- 3. Zoek een delta waarvan de id, rolName en DeltaType gelijk zijn aan die van de nieuwe.
--   Indien gevonden: vervang die door de nieuwe.
--   Indien niet gevonden: zoek een delta waarvan id en rolName overeenkomen.
--     Indien gevonden, als geldt:
--       het ene DeltaType is Add en het andere Remove, verwijder dan de oude.
--       het oude DeltaType is Change en het nieuwe Remove, vervang de oude dan door de nieuwe
--       het oude DeltaType is Add en het nieuwe is Change, vervang dan in de oude de rolID door die van de nieuwe.
--     Indien niet gevonden: voeg de nieuwe toe.
-- ZO NEE:
-- 4. zoek een delta waarvan id, rolName en rolID gelijk zijn aan die van de nieuwe en het ene DeltaType Add is en het andere Remove.
--   Indien gevonden: verwijder de oude.
--   Anders: voeg de nieuwe toe.
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
