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

module Perspectives.Deltas where

import Affjax (Request, request)
import Affjax.RequestBody as RequestBody
import Control.Monad.AvarMonadAsk (modify) as AA
import Control.Monad.State.Trans (StateT, execStateT, get, lift, put)
import Data.Array (cons, deleteAt, findIndex, union)
import Data.Array.NonEmpty (singleton)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (Map, lookup, insert, empty)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (over, unwrap)
import Data.Traversable (for_)
import Data.TraversableWithIndex (forWithIndex)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign.Generic (defaultOptions, genericEncodeJSON)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##=))
import Perspectives.Couchdb (PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (defaultPerspectRequest)
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (roleType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Sync.AffectedContext (AffectedContext(..))
import Perspectives.Sync.Class.DeltaUsers (class DeltaUsers, addToTransaction, transactionCloneWithDelta, users)
import Perspectives.Sync.Transaction (Transaction(..), transactieID)
import Perspectives.TypesForDeltas (RoleBindingDelta, RolePropertyDelta, ContextDelta)
import Perspectives.User (getCouchdbBaseURL)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (<>), (==))

runTransactie :: Transaction -> MonadPerspectives Unit
runTransactie t@(Transaction{changedDomeinFiles}) = do
  for_ changedDomeinFiles saveCachedDomeinFile
  -- Send the Transaction to all involved.
  distributeTransactie t
  pure unit

distributeTransactie :: Transaction -> MonadPerspectives Unit
distributeTransactie t = do
  (customizedTransacties :: TransactionPerUser) <- transactieForEachUser t
  _ <- forWithIndex customizedTransacties sendTransactieToUser
  pure unit

sendTransactieToUser :: RoleInstance -> Transaction -> MonadPerspectives Unit
sendTransactieToUser userId t = do
  log ("In sendTransactieToUser with " <> show userId <> " and transaction " <> show t)
  -- The connection database that we should put the transaction in, is identified by the userId.
  -- TODO controleer of hier authentication nodig is!
  cdbUrl <- getCouchdbBaseURL
  (rq :: (Request String)) <- defaultPerspectRequest
  res <- liftAff $ request $ rq {method = Left PUT, url = (cdbUrl <> "/" <> (unwrap userId) <> "/" <> transactieID t), content = Just $ RequestBody.string (genericEncodeJSON defaultOptions t)}
  void $ onAccepted res.status [200, 201] "sendTransactieToUser"
    (onCorrectCallAndResponse "sendTransactieToUser" res.body (\(a :: PutCouchdbDocument) -> pure unit))

type TransactionPerUser = Map RoleInstance Transaction

-- | The Transaction holds Deltas and each Delta names user instances who should receive that Delta.
-- | This function builds a custom version of the Transaction for each such user.
transactieForEachUser :: Transaction -> MonadPerspectives TransactionPerUser
transactieForEachUser t@(Transaction{contextDeltas, roleDeltas, propertyDeltas}) = do
  execStateT (do
    (for_ contextDeltas \d -> (transactieForEachUser' d (users d)))
    (for_ roleDeltas \d -> (transactieForEachUser' d (users d)))
    (for_ propertyDeltas \d -> (transactieForEachUser' d (users d)))
    )
    empty
  where
    transactieForEachUser' :: forall d. DeltaUsers d => d -> (Array RoleInstance) -> StateT TransactionPerUser (MonadPerspectives) Unit
    transactieForEachUser' d users = do
      trs <- get
      for_
        users
        (\(user :: RoleInstance) -> case lookup user trs of
          Nothing -> put $ insert user (transactionCloneWithDelta d t) trs
          Just tr -> put $ insert user (addToTransaction d tr) trs
        )

addContextToTransactie :: PerspectContext ->
  MonadPerspectivesTransaction Unit
addContextToTransactie c@(PerspectContext{_id, me}) = do
  case me of
    Nothing -> lift $ AA.modify (over Transaction \(t@{createdContexts}) -> t {createdContexts = cons c createdContexts})
    Just me' -> do
      myType <- lift $ lift $ (me' ##= roleType)
      lift $ AA.modify (over Transaction \(t@{createdContexts, affectedContexts}) -> t {createdContexts = cons c createdContexts, affectedContexts = cons (AffectedContext {contextInstances: singleton _id, userTypes: myType}) affectedContexts})

addRolToTransactie :: PerspectRol -> MonadPerspectivesTransaction Unit
addRolToTransactie c = lift $ AA.modify (over Transaction \(t@{createdRoles}) -> t {createdRoles = cons c createdRoles})

deleteContextFromTransactie :: ContextInstance -> MonadPerspectivesTransaction Unit
deleteContextFromTransactie contextId = lift $ AA.modify (over Transaction \(t@{createdContexts, deletedContexts}) ->
  case findIndex (\(PerspectContext{_id: i}) -> contextId == i) createdContexts of
    Nothing -> t {deletedContexts = cons contextId deletedContexts}
    (Just i) -> t {createdContexts = unsafePartial $ fromJust $ deleteAt i createdContexts})

deleteRolFromTransactie :: RoleInstance -> MonadPerspectivesTransaction Unit
deleteRolFromTransactie rolId = lift $ AA.modify (over Transaction \(t@{createdRoles, deletedRoles}) ->
  case findIndex (\(PerspectRol{_id: i}) -> rolId == i) createdRoles of
    Nothing -> t {deletedRoles = cons rolId deletedRoles}
    (Just i) -> t {createdRoles = unsafePartial $ fromJust $ deleteAt i createdRoles})

addDomeinFileToTransactie :: ID -> MonadPerspectivesTransaction Unit
addDomeinFileToTransactie dfId = lift $ AA.modify (over Transaction \(t@{changedDomeinFiles}) ->
  t {changedDomeinFiles = union changedDomeinFiles [dfId]})

addContextDelta :: ContextDelta -> MonadPerspectivesTransaction Unit
addContextDelta d = lift $ AA.modify (over Transaction \t@{contextDeltas} -> t {contextDeltas = cons d contextDeltas})

addRoleDelta :: RoleBindingDelta -> MonadPerspectivesTransaction Unit
addRoleDelta d = lift $ AA.modify (over Transaction \t@{roleDeltas} -> t {roleDeltas = cons d roleDeltas})

addPropertyDelta :: RolePropertyDelta -> MonadPerspectivesTransaction Unit
addPropertyDelta d = lift $ AA.modify (over Transaction \t@{propertyDeltas} -> t {propertyDeltas = cons d propertyDeltas})

addCorrelationIdentifiersToTransactie :: Array CorrelationIdentifier -> MonadPerspectivesTransaction Unit
addCorrelationIdentifiersToTransactie corrIds = lift $ AA.modify (over Transaction \t@{correlationIdentifiers} -> t {correlationIdentifiers = union correlationIdentifiers corrIds})

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
