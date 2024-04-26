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
import Control.Monad.State.Trans (StateT, execStateT, get, lift, modify, put)
import Data.Array (catMaybes, concat, elemIndex, filterA, foldl, head, length, nub, null, snoc, union)
import Data.DateTime.Instant (toDateTime)
import Data.Map (Map, empty, filter, insert, lookup) as Map
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (over, unwrap)
import Data.Traversable (for, for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Now (now)
import Partial.Unsafe (unsafePartial)
import Perspectives.AMQP.Stomp (sendToTopic)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.ContextAndRole (context_buitenRol, context_universeContextDelta, rol_context, rol_property, rol_propertyDelta, rol_contextDelta, rol_universeRoleDelta)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>), (##=))
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.ObjectGetters (getPerspectivesSystemUsers, getProperty, notIsMe, perspectivesUsersRole_, roleType_)
import Perspectives.ModelDependencies (connectedToAMQPBroker, userChannel) as DEP
import Perspectives.ModelDependencies (perspectivesUsersCancelled, perspectivesUsersPublicKey)
import Perspectives.Names (getMySystem)
import Perspectives.Persistence.API (Url, addDocument)
import Perspectives.Persistent (getPerspectContext, getPerspectRol, postDatabaseName)
import Perspectives.PerspectivesState (nextTransactionNumber, stompClient)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesSystemUser, PerspectivesUser, RoleInstance(..), Value(..), perspectivesUser2RoleInstance)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedPropertyType(..), RoleType(..))
import Perspectives.ResourceIdentifiers (deltaAuthor2ResourceIdentifier)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (PublicKeyInfo, Transaction(..), TransactionDestination(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), addToTransactionForPeer, transactieID)
import Perspectives.Types.ObjectGetters (isPublicRole)
import Prelude (Unit, bind, discard, eq, flip, map, not, pure, show, unit, void, ($), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>))
import Simple.JSON (writeJSON)

-- | Splits the transaction in versions specific for each peer and sends them.
-- | If a public roles are involved, will return their TransactionForPeer instances.
-- IMPLEMENTATION NOTICE: we cannot handle these instances here by calling executeTransactionForPublicRole
-- because that would need cyclic model imports.
distributeTransaction :: Transaction -> MonadPerspectives TransactionPerUser
distributeTransaction t@(Transaction{changedDomeinFiles}) = do
  for_ changedDomeinFiles (DomeinFileId >>> saveCachedDomeinFile)
  -- Send the Transaction to all involved.
  addPublicKeysToTransaction t >>= distributeTransactie'
  
distributeTransactie' :: Transaction -> MonadPerspectives TransactionPerUser
distributeTransactie' t = do
  (customizedTransacties :: TransactionPerUser) <- transactieForEachUser t
  map (unsafePartial fromJust) <<< Map.filter isJust <$> forWithIndex customizedTransacties sendTransactie

-- | If we have the visitor user, handle it by augmenting resources in the public store with the deltas.
sendTransactie :: TransactionDestination -> TransactionForPeer -> MonadPerspectives (Maybe TransactionForPeer)
sendTransactie (Peer perspectivesUser) t = sendTransactieToUserUsingAMQP perspectivesUser t *> pure Nothing
sendTransactie (PublicDestination r) t = pure $ Just t

-- | Send a transaction using the Couchdb Channel.
sendTransactieToUserUsingCouchdb :: Url -> String -> TransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingCouchdb cdbUrl userId t = do
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
sendTransactieToUserUsingAMQP :: PerspectivesUser -> TransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingAMQP perspectivesUser t = do
  connected <- connectedToAMQPBroker
  n <- liftEffect $ now
  dt <- pure $ SerializableDateTime (toDateTime n)
  messageId <- pure $ show perspectivesUser <> show dt
  if connected
    then do
      mstompClient <- stompClient
      case mstompClient of
        Just stompClient -> do
          saveTransactionInOutgoingPost perspectivesUser messageId t
          -- Get all PerspectivesSystemUsers and send the transaction to them. This is how we deal with multiple installations.
          (allInstallations :: Array PerspectivesSystemUser) <- (perspectivesUser2RoleInstance perspectivesUser ##= getPerspectivesSystemUsers) 
          for_ allInstallations 
            \(installation :: PerspectivesSystemUser) -> liftEffect $ sendToTopic stompClient (unwrap installation) messageId (writeJSON t)
        otherwise -> saveTransactionInOutgoingPost perspectivesUser messageId t
    else saveTransactionInOutgoingPost perspectivesUser messageId t

  where
    connectedToAMQPBroker :: MonadPerspectives Boolean
    connectedToAMQPBroker = do
      mySystem <- getMySystem
      mConnected <- (RoleInstance $ buitenRol mySystem) ##> getProperty (EnumeratedPropertyType DEP.connectedToAMQPBroker)
      pure $ mConnected == (Just $ Value "true")

saveTransactionInOutgoingPost :: PerspectivesUser -> String -> TransactionForPeer -> MonadPerspectives Unit
saveTransactionInOutgoingPost userId messageId t = do
  postDB <- postDatabaseName
  void $ addDocument postDB (OutgoingTransaction{_id: messageId, receiver: userId, transaction: t}) messageId

-- | An object of TransactionForPeer where the keys are the string value of RoleInstances, invariably identifying user roles.
-- | Must be either an instance of sys:PerspectivesSystem$User, or of a RoleInstance of a type with RoleKind Visitor.
type TransactionPerUser = Map.Map TransactionDestination TransactionForPeer

-- | The Transaction holds Deltas and each Delta names user instances who should receive that Delta.
-- | This function builds a custom version of the Transaction for each such user.
-- | `users` in DeltaInTransaction will not always be model://perspectives.domains#System$PerspectivesSystem$User instances.
transactieForEachUser :: Transaction -> MonadPerspectives TransactionPerUser
transactieForEachUser t@(Transaction tr@{timeStamp, deltas, userRoleBottoms, publicKeys}) = do
  execStateT (for_ deltas \(DeltaInTransaction{users, delta}) -> do
    system <- lift getMySystem
    -- Lookup the ultimate filler for all users in the delta.
    (sysUsers :: Array TransactionDestination) <- pure $ catMaybes (flip Map.lookup userRoleBottoms <$> users)
    addDeltaToCustomisedTransactie delta (nub sysUsers) (ContextInstance system))
    Map.empty
  where
    -- `destinations` WILL be either 
    --    * Peer model://perspectives.domains#System$TheWorld$PerspectivesUsers instances, or
    --    * PublicDestination roleInstance, where the latter is an instance of the Visitor role.
    addDeltaToCustomisedTransactie :: SignedDelta -> (Array TransactionDestination) -> ContextInstance -> StateT TransactionPerUser (MonadPerspectives) Unit
    addDeltaToCustomisedTransactie d@(SignedDelta {author}) destinations perspectivesSystem = for_
      destinations
      (\destination -> case destination of 
        peer@(Peer perspectivesUser) -> if not $ eq perspectivesUser author
          then do
            trs <- get
            case Map.lookup peer trs of 
              -- TODO: verwijder public key info voor deltas die er niet toe doen voor deze user.
              Nothing -> put $ Map.insert peer (TransactionForPeer {author, perspectivesSystem, timeStamp, deltas: [d], publicKeys}) trs
              Just trans -> put $ Map.insert peer (addToTransactionForPeer d trans) trs
          else pure unit
        publicRole@(PublicDestination _) -> do
            trs <- get
            case Map.lookup publicRole trs of 
              Nothing -> put $ Map.insert publicRole (TransactionForPeer {author, perspectivesSystem, timeStamp, deltas: [d], publicKeys}) trs
              Just trans -> put $ Map.insert publicRole (addToTransactionForPeer d trans) trs
      )

addDomeinFileToTransactie :: ID -> MonadPerspectivesTransaction Unit
addDomeinFileToTransactie dfId = AA.modify (over Transaction \(t@{changedDomeinFiles}) ->
  t {changedDomeinFiles = union changedDomeinFiles [dfId]})

-- | If we have a public role instance, return [Tuple rid []].
-- | If the role chain bottoms out in an instance of TheWorld$PerspectivesUsers, return Tuple rid <<the other PerspectivesSystem$Users>>.
-- | Otherwise return an empty array.
computeUserRoleBottom :: RoleInstance -> MonadPerspectives (Array (Tuple RoleInstance TransactionDestination))
computeUserRoleBottom rid = ((map ENR <<< roleType_ >=> isPublicRole) rid) >>= if _ 
  then pure [Tuple rid (PublicDestination rid)]
  else perspectivesUsersRole_ rid >>= case _ of 
    Nothing -> pure []
    Just perspectivesUser -> ((perspectivesUser2RoleInstance perspectivesUser) ##> getProperty (EnumeratedPropertyType perspectivesUsersCancelled)) >>= case _ of 
      Just (Value "true") -> pure []
      _ -> pure [Tuple rid (Peer perspectivesUser)]

-- | Add the delta at the end of the array, unless it is already in the transaction or there are no users (and ignore the own user).
-- | Only include 
-- |    * public roles
-- |    * instances of sys:PerspectivesSystem$User, but not the one that equals the local sys:Me.
addDelta :: DeltaInTransaction -> MonadPerspectivesTransaction Unit
addDelta dt@(DeltaInTransaction{users}) = do 
  -- NOTE. Even though we try not to create deltas with roles that represent me, on system installation this can go wrong.
  users' <- lift $ filterA notIsMe users
  if null users'
    then pure unit
    else do
      newUserBottoms <- lift (concat <$> for users' computeUserRoleBottom)
      AA.modify (over Transaction \t@{deltas, userRoleBottoms} -> t 
        { deltas =
          if isJust $ elemIndex dt deltas
            then deltas
            else snoc deltas dt
        , userRoleBottoms = foldl (\userBottoms' (Tuple role user) -> Map.insert role user userBottoms') userRoleBottoms newUserBottoms
        })

-- | Insert the delta at the index, unless it is already in the transaction or there are no users (and ignore the own user).
insertDelta :: DeltaInTransaction -> Int -> MonadPerspectivesTransaction Unit
insertDelta dt@(DeltaInTransaction{users}) i = do
  -- NOTE. Even though we try not to create deltas with roles that represent me, on system installation this can go wrong.
  users' <- lift $ filterA notIsMe users
  if null users'
    then pure unit
    else do
      (newUserBottoms :: Array (Tuple RoleInstance TransactionDestination)) <- lift (concat <$> for users' computeUserRoleBottom)
      AA.modify (over Transaction \t@{deltas, userRoleBottoms} -> t 
        { deltas =
          if isJust $ elemIndex dt deltas
            then deltas
            else snoc deltas dt
        , userRoleBottoms = foldl (\userBottoms' (Tuple role user) -> Map.insert role user userBottoms') userRoleBottoms newUserBottoms
        })

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

addPublicKeysToTransaction :: Transaction -> MonadPerspectives Transaction
addPublicKeysToTransaction (Transaction tr@{deltas}) = do 
  publicKeys :: Map.Map PerspectivesUser PublicKeyInfo <- execStateT (for_ deltas addPublicKeyInfo) Map.empty
  pure $ Transaction tr { publicKeys = ENCMAP.EncodableMap publicKeys}

  where
    addPublicKeyInfo :: DeltaInTransaction -> StateT (Map.Map PerspectivesUser PublicKeyInfo) MonadPerspectives Unit
    addPublicKeyInfo (DeltaInTransaction{delta}) = case delta of 
      SignedDelta {author} -> do
        keys <- get
        case Map.lookup author keys of 
          Just _ -> pure unit
          Nothing -> do 
            pkInfo <- lift $ getPkInfo author
            void $ modify (\keys' -> Map.insert author pkInfo keys')
    
    -- This is built on the assumption that the argument is the string value of the RoleInstance of type TheWorld$PerspectivesUser
    -- that fills SocialEnvironment$Me
    getPkInfo :: PerspectivesUser -> MonadPerspectives PublicKeyInfo
    getPkInfo perspectivesUser = do 
      authorRole <- getPerspectRol (deltaAuthor2ResourceIdentifier (perspectivesUser2RoleInstance perspectivesUser))
      theworld <- getPerspectContext $ rol_context authorRole
      theWorldExternal <- getPerspectRol (context_buitenRol theworld)
      pure let 
        k@(Value key) = unsafePartial fromJust $ head $ rol_property authorRole (EnumeratedPropertyType perspectivesUsersPublicKey)
        propertyDelta = unsafePartial fromJust $ rol_propertyDelta authorRole (EnumeratedPropertyType perspectivesUsersPublicKey) k
      in
        { key, deltas: 
          [ rol_universeRoleDelta theWorldExternal
          , context_universeContextDelta theworld
          , rol_contextDelta theWorldExternal
          , rol_universeRoleDelta authorRole
          , rol_contextDelta authorRole
          , propertyDelta
          ]}
