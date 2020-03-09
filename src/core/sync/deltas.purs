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
import Control.Monad.AvarMonadAsk (modify, gets) as AA
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (StateT, execStateT, get, lift, put)
import Data.Array (cons, union)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (Map, lookup, insert, empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Traversable (for_)
import Data.TraversableWithIndex (forWithIndex)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Foreign.Generic (defaultOptions, genericEncodeJSON)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>))
import Perspectives.Couchdb (PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (defaultPerspectRequest)
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Instances.GetPropertyOnRoleGraph (getPropertyGetter)
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance, Value(..))
import Perspectives.Sync.Class.DeltaClass (setIndex)
import Perspectives.Sync.Class.DeltaUsers (class DeltaUsers, addToTransaction, transactionCloneWithDelta, users)
import Perspectives.Sync.Transaction (Transaction(..), transactieID)
import Perspectives.TypesForDeltas (ContextDelta, RoleBindingDelta, RolePropertyDelta, UniverseContextDelta, UniverseRoleDelta)
import Perspectives.User (getCouchdbBaseURL)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<>), (+))

distributeTransaction :: Transaction -> MonadPerspectives Unit
distributeTransaction t@(Transaction{changedDomeinFiles}) = do
  for_ changedDomeinFiles saveCachedDomeinFile
  -- Send the Transaction to all involved.
  distributeTransactie' t
  pure unit

distributeTransactie' :: Transaction -> MonadPerspectives Unit
distributeTransactie' t = do
  (customizedTransacties :: TransactionPerUser) <- transactieForEachUser t
  _ <- forWithIndex customizedTransacties sendTransactieToUser
  pure unit

sendTransactieToUser :: RoleInstance -> Transaction -> MonadPerspectives Unit
sendTransactieToUser userId t = do
  -- TODO controleer of hier authentication nodig is!
  userType <- roleType_ userId
  getChannel <- getPropertyGetter "model:System$PerspectivesSystem$User$Channel" userType
  mchannel <- userId ##> getChannel
  case mchannel of
    Nothing -> void $ throwError (error ("sendTransactieToUser: cannot find channel for user " <> (unwrap userId)))
    Just (Value channel) -> do
      cdbUrl <- getCouchdbBaseURL
      (rq :: (Request String)) <- defaultPerspectRequest
      res <- liftAff $ request $ rq {method = Left PUT, url = (cdbUrl <> channel <> "/" <> transactieID t), content = Just $ RequestBody.string (genericEncodeJSON defaultOptions t)}
      void $ onAccepted res.status [200, 201] "sendTransactieToUser"
        (onCorrectCallAndResponse "sendTransactieToUser" res.body (\(a :: PutCouchdbDocument) -> pure unit))

type TransactionPerUser = Map RoleInstance Transaction

-- | The Transaction holds Deltas and each Delta names user instances who should receive that Delta.
-- | This function builds a custom version of the Transaction for each such user.
transactieForEachUser :: Transaction -> MonadPerspectives TransactionPerUser
transactieForEachUser t@(Transaction{contextDeltas, roleDeltas, propertyDeltas, universeRoleDeltas, universeContextDeltas}) = do
  execStateT (do
    for_ contextDeltas \d -> addDeltaToCustomisedTransactie d (users d)
    for_ roleDeltas \d -> addDeltaToCustomisedTransactie d (users d)
    for_ propertyDeltas \d -> addDeltaToCustomisedTransactie d (users d)
    for_ universeRoleDeltas \d -> addDeltaToCustomisedTransactie d (users d)
    for_ universeContextDeltas \d -> addDeltaToCustomisedTransactie d (users d)
    )
    empty
  where
    addDeltaToCustomisedTransactie :: forall d. DeltaUsers d => d -> (Array RoleInstance) -> StateT TransactionPerUser (MonadPerspectives) Unit
    addDeltaToCustomisedTransactie d users = do
      trs <- get
      for_
        users
        (\(user :: RoleInstance) -> case lookup user trs of
          Nothing -> put $ insert user (transactionCloneWithDelta d t) trs
          Just tr -> put $ insert user (addToTransaction d tr) trs
        )

addDomeinFileToTransactie :: ID -> MonadPerspectivesTransaction Unit
addDomeinFileToTransactie dfId = lift $ AA.modify (over Transaction \(t@{changedDomeinFiles}) ->
  t {changedDomeinFiles = union changedDomeinFiles [dfId]})

addContextDelta :: ContextDelta -> MonadPerspectivesTransaction Unit
addContextDelta d = lift $ AA.modify (over Transaction \t@{contextDeltas, nextDeltaIndex} -> t {contextDeltas = cons (setIndex nextDeltaIndex d) contextDeltas, nextDeltaIndex = nextDeltaIndex + 1})

addRoleDelta :: RoleBindingDelta -> MonadPerspectivesTransaction Unit
addRoleDelta d = lift $ AA.modify (over Transaction \t@{roleDeltas, nextDeltaIndex} -> t {roleDeltas = cons (setIndex nextDeltaIndex d) roleDeltas, nextDeltaIndex = nextDeltaIndex + 1})

addPropertyDelta :: RolePropertyDelta -> MonadPerspectivesTransaction Unit
addPropertyDelta d = lift $ AA.modify (over Transaction \t@{propertyDeltas, nextDeltaIndex} -> t {propertyDeltas = cons (setIndex nextDeltaIndex d) propertyDeltas, nextDeltaIndex = nextDeltaIndex + 1})

addUniverseContextDelta :: UniverseContextDelta -> MonadPerspectivesTransaction Unit
addUniverseContextDelta d = lift $ AA.modify (over Transaction \t@{universeContextDeltas, nextDeltaIndex} -> t {universeContextDeltas = cons (setIndex nextDeltaIndex d) universeContextDeltas, nextDeltaIndex = nextDeltaIndex + 1})

addUniverseRoleDelta :: UniverseRoleDelta -> MonadPerspectivesTransaction Unit
addUniverseRoleDelta d = lift $ AA.modify (over Transaction \t@{universeRoleDeltas, nextDeltaIndex} -> t {universeRoleDeltas = cons (setIndex nextDeltaIndex d) universeRoleDeltas, nextDeltaIndex = nextDeltaIndex + 1})

addCorrelationIdentifiersToTransactie :: Array CorrelationIdentifier -> MonadPerspectivesTransaction Unit
addCorrelationIdentifiersToTransactie corrIds = lift $ AA.modify (over Transaction \t@{correlationIdentifiers} -> t {correlationIdentifiers = union correlationIdentifiers corrIds})

increaseDeltaIndex :: MonadPerspectivesTransaction Int
increaseDeltaIndex = do
  i <- lift $  AA.gets (\(Transaction{nextDeltaIndex}) -> nextDeltaIndex)
  lift $ AA.modify (over Transaction \t@{nextDeltaIndex} -> t {nextDeltaIndex = nextDeltaIndex + 1})
  pure i

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
