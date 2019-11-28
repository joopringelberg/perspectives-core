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

import Affjax (printResponseFormatError)
import Affjax (put) as AJ
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.AvarMonadAsk (modify, get) as AA
import Control.Monad.State.Trans (StateT, execStateT, get, lift, put)
import Data.Array (cons, delete, deleteAt, elemIndex, find, findIndex, head, union)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (over, unwrap)
import Data.Traversable (for_)
import Data.TraversableWithIndex (forWithIndex)
import Effect.Aff (error, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign.Object (Object, empty, insert, lookup) as FO
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (context)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Class.Property (functional) as P
import Perspectives.Representation.Class.Role (functional) as R
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypesForDeltas (RoleDelta(..), DeltaType(..), PropertyDelta(..), ContextDelta(..))
import Perspectives.User (getUser)
import Perspectives.Utilities (maybeM, onNothing')
import Prelude (Unit, bind, discard, identity, pure, show, unit, ($), (&&), (<<<), (<>), (==), (>>=), (||))

-- TODO: doe ook wat met de andere modificaties in de transactie?
-- Waarschijnlijk niet. De update functies (Perspectives.Assignment.Update) slaan op in cache en in couchdb.
runTransactie :: Transaction -> MonadPerspectivesTransaction Unit
runTransactie t@(Transaction{changedDomeinFiles}) = do
  lift $ lift $ for_ changedDomeinFiles saveCachedDomeinFile
  -- Send the Transaction to all involved.
  -- distributeTransactie t
  pure unit

distributeTransactie :: Transaction -> MonadPerspectives Unit
distributeTransactie t = do
  -- TODO: SLUIT DIT AAN NA REFACTOREN VAN transactieForEachUser
  -- (customizedTransacties :: FO.Object Transaction) <- transactieForEachUser t
  -- (customizedTransacties :: FO.Object Transaction) <- pure []
  -- _ <- forWithIndex customizedTransacties sendTransactieToUser
  pure unit

-- TODO: Pas addContextToTransactie toe, ergens!
addContextToTransactie :: PerspectContext ->
  MonadPerspectivesTransaction Unit
addContextToTransactie c = lift $ AA.modify (over Transaction \(t@{createdContexts}) -> t {createdContexts = cons c createdContexts})

-- TODO: Pas addRolToTransactie toe, ergens!
addRolToTransactie :: PerspectRol -> MonadPerspectivesTransaction Unit
addRolToTransactie c = lift $ AA.modify (over Transaction \(t@{createdRoles}) -> t {createdRoles = cons c createdRoles})

-- TODO: Pas deleteContextFromTransactie toe, ergens!
deleteContextFromTransactie :: PerspectContext -> MonadPerspectivesTransaction Unit
deleteContextFromTransactie c@(PerspectContext{_id}) = lift $ AA.modify (over Transaction \(t@{createdContexts, deletedContexts}) ->
  case findIndex (\(PerspectContext{_id: i}) -> _id == i) createdContexts of
    Nothing -> t {deletedContexts = cons _id deletedContexts}
    (Just i) -> t {createdContexts = unsafePartial $ fromJust $ deleteAt i createdContexts})

-- TODO: Pas deleteRolFromTransactie toe, ergens!
deleteRolFromTransactie :: PerspectRol -> MonadPerspectivesTransaction Unit
deleteRolFromTransactie c@(PerspectRol{_id}) = lift $ AA.modify (over Transaction \(t@{createdRoles, deletedRoles}) ->
  case findIndex (\(PerspectRol{_id: i}) -> _id == i) createdRoles of
    Nothing -> t {deletedRoles = cons _id deletedRoles}
    (Just i) -> t {createdRoles = unsafePartial $ fromJust $ deleteAt i createdRoles})

addDomeinFileToTransactie :: ID -> MonadPerspectivesTransaction Unit
addDomeinFileToTransactie dfId = lift $ AA.modify (over Transaction \(t@{changedDomeinFiles}) ->
  t {changedDomeinFiles = union changedDomeinFiles [dfId]})

addContextDelta :: ContextDelta -> MonadPerspectivesTransaction Unit
addContextDelta d = lift $ AA.modify (over Transaction \t@{contextDeltas} -> t {contextDeltas = cons d contextDeltas})

addRoleDelta :: RoleDelta -> MonadPerspectivesTransaction Unit
addRoleDelta d = lift $ AA.modify (over Transaction \t@{roleDeltas} -> t {roleDeltas = cons d roleDeltas})

addPropertyDelta :: PropertyDelta -> MonadPerspectivesTransaction Unit
addPropertyDelta d = lift $ AA.modify (over Transaction \t@{propertyDeltas} -> t {propertyDeltas = cons d propertyDeltas})

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
-- TODO. Dit werkt voor de generieke Delta, maar niet voor de specifieke ContextDelta, RoleDelta, enz.
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


{-
sendTransactieToUser :: ID -> Transaction -> MonadPerspectives Unit
sendTransactieToUser userId t = do
  tripleUserIP <- userId ##> DTG.identity -- TODO. Het lijkt erop dat hier een getter toegepast moet worden die het IP adres van de user oplevert!
  (userIP :: String) <- (onNothing' <<< error) ("sendTransactieToUser: user has no IP: " <> userId) tripleUserIP
  -- TODO controleer of hier authentication nodig is!
  res  <- liftAff $ AJ.put ResponseFormat.string (userIP <> "/" <> userId <> "_post/" <> transactieID t) (RequestBody.string (writeJSON t))
  (StatusCode n) <- pure res.status
  case n == 200 || n == 201 of
    true -> pure unit
    false -> case res.body of
      (Left e) -> throwError $ error ("sendTransactieToUser " <> transactieID t <> " fails: " <> (show res.status) <> "(" <> printResponseFormatError e <> ")")
      _ -> pure unit
  pure unit

-- TODO. De verbinding tussen Actie en Rol is omgekeerd en is niet
-- langer geregistreerd als een rol van de Actie, maar als rol van de Rol (objectRol en subjectRol).
-- | The (IDs of the) users that play a role in, and have a relevant perspective on, the Context that is modified;
-- | or the users that play a role in the context of the Role that is modified and have a relevant perspective on that Role.
usersInvolvedInDelta :: Delta -> MonadPerspectives (Array RolInContext)
usersInvolvedInDelta dlt@(Delta{isContext}) = if isContext then usersInvolvedInContext dlt else usersInvolvedInRol dlt
  where

  -- From the instance of a Rol, retrieve the instances of users that play another Rol
  -- in the same context, such that they have an Actie with an objectView that has the
  -- changed property (as identified by memberName).
  usersInvolvedInRol :: Delta -> MonadPerspectives (Array RolInContext)
  usersInvolvedInRol (Delta{id, memberName}) =
    do
      (contextId :: AnyContext) <- (RolInContext id) %%>> context
      ((Triple {object}) :: Triple RolInContext RolInContext) <-
        (RolInContext id) ## DTG.rolType >-> subjectsOfRelevantActies >-> (rolesOf contextId) >-> rolUser
      pure $ object
    where
      -- roles in context that play the subjectRol in the relevant acties
      subjectsOfRelevantActies :: (RolDef **> UserRolDef)
      subjectsOfRelevantActies = unwrap `before` filter (notEmpty (intersect actiesOfRol relevantActies)) (enclosingDefinition `followedBy` ContextDef >-> searchUnqualifiedRolDefinition "gebruikerRol")

      -- acties that have an objectView with the memberName
      relevantActies :: (RolDef **> ActieDef)
      relevantActies = unwrap `before` filter (hasRelevantView (PropertyDef memberName)) (enclosingDefinition `followedBy` ContextDef >-> actiesInContextDef)

  -- From the instance of the context, retrieve the instances of the users that play
  -- a Rol in this context that have a subjectRol bound to an Actie that is bound as the
  -- objectRol of the Rol of which memberName is an instance.

  -- From the instance of the context, retrieve the instances of the users that play
  -- a Rol in this context who fill the $subject Rol of an Actie that has its $object Rol filled
  -- with the Rol of which memberName is an instance.
  usersInvolvedInContext :: Delta -> MonadPerspectives (Array RolInContext)
  usersInvolvedInContext (Delta{id, memberName}) =
    do
      (Triple {object}) <- (RolInContext id) ## DTG.rolType >-> actorsForObject >-> (rolesOf id) >-> rolUser
      pure object
    where
      -- All Rollen that are the subject of Acties that have the Rol as object.
      actorsForObject :: (RolDef **> UserRolDef)
      actorsForObject = objectRollenDef >-> inverse_subjectRollenDef

  -- Tests an Actie for having memberName in the view that is its objectView.
  -- psp:Actie -> psp:Boolean
  hasRelevantView :: PropertyDef -> (ActieDef **> PBool)
  hasRelevantView id = id `containedIn` (objectViewDef >-> propertyReferenties >-> DTG.rolBindingDef `followedBy` PropertyDef)


-- Bouw een transactie eerst op, splits hem dan in versies voor elke gebruiker.
-- Doorloop de verzameling deltas en bepaal per delta welke gebruikers betrokken zijn.
-- Bouw al doende een FO.Object van userId en gespecialiseerde transacties op, waarbij je een transactie toevoegt voor een gebruiker die nog niet in de FO.Object voorkomt.
transactieForEachUser :: Transaction -> MonadPerspectives(FO.Object Transaction)
transactieForEachUser t@(Transaction{deltas}) = do
  execStateT
    (for_ deltas (\d -> (lift $ usersInvolvedInDelta d) >>= (\(users :: Array RolInContext) -> transactieForEachUser' d users)))
    FO.empty
  where
    transactieForEachUser' :: Delta -> (Array RolInContext) -> StateT (FO.Object Transaction) (MonadPerspectives) Unit
    transactieForEachUser' d users = do
      trs <- get
      for_
        users
        (\(user :: RolInContext) -> case FO.lookup (unwrap user) trs of
          Nothing -> put $ FO.insert (unwrap user) (transactieCloneWithJustDelta t d) trs
          (Just (Transaction tr@{deltas: ds})) -> put $ FO.insert (unwrap user) (Transaction tr {deltas = cons d ds}) trs)
    transactieCloneWithJustDelta :: Transaction -> Delta -> Transaction
    transactieCloneWithJustDelta (Transaction tr) d = Transaction tr {deltas = [d]}
-}
