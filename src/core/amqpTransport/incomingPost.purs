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

module Perspectives.AMQP.IncomingPost
  ( incomingPost
  , retrieveBrokerService
  )
  where

import Control.Coroutine (Consumer, Producer, await, runProcess, ($$))
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Array (nub, sort)
import Data.Either (Either(..))
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (ForeignError(..), MultipleErrors)
import Perspectives.AMQP.Stomp (StructuredMessage, acknowledge, createStompClient, messageProducer, sendToTopic)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes (BrokerService, MonadPerspectives, MonadPerspectivesQuery, (##>))
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.ObjectGetters (context, externalRole, getProperty)
import Perspectives.ModelDependencies (accountHolder, accountHolderName, accountHolderPassword, accountHolderQueueName, brokerEndpoint, brokerServiceContractInUse, brokerServiceExchange, connectedToAMQPBroker, myBrokers, sysUser)
import Perspectives.Names (getMySystem, lookupIndexedContext)
import Perspectives.Persistence.API (deleteDocument, documentsInDatabase, excludeDocs, getDocument_)
import Perspectives.Persistent (postDatabaseName)
import Perspectives.PerspectivesState (getBrokerService, getPerspectivesUser, setBrokerService, setStompClient, stompClient, transactionLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction, getRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.RunMonadPerspectivesTransaction (detectPublicStateChanges, runMonadPerspectivesTransaction')
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Prelude (Unit, bind, pure, show, unit, void, ($), (>=>), (>>=), discard, (*>), (<>), (>>>), map, (<$>), (<<<))
import Simple.JSON (writeJSON)

incomingPost :: MonadPerspectives Unit
incomingPost = do
  setConnectionState false
  {topic, queueId, login, passcode, vhost, url} <- getBrokerService
  -- Create a Stomp Client: url
  stpClient <- liftEffect $ createStompClient( url )
  -- Save the client in state.
  setStompClient stpClient
  -- Create a messageProducer: ConnectAndSubscriptionParameters
  (transactionProducer :: Producer (Either MultipleErrors (StructuredMessage TransactionForPeer)) MonadPerspectives Unit) <- pure $ messageProducer stpClient
    { topic -- the PerspectivesSystem identifier.
    , queueId
    , login
    , passcode
    , vhost
    }
  void $ runProcess $ transactionProducer $$ transactionConsumer

  where
    transactionConsumer :: Consumer (Either MultipleErrors (StructuredMessage TransactionForPeer)) MonadPerspectives Unit
    transactionConsumer = do
      postDB <- lift $ postDatabaseName
      forever do
        change <- await
        case change of
          Left me -> case head me of
            ForeignError "noConnection" -> lift $ setConnectionState false
            ForeignError "connection" -> lift $ setConnectionState true *> sendOutgoingPost
            TypeMismatch "receipt" docId -> void $ lift $ deleteDocument postDB docId Nothing
            _ -> log ("Perspectives.AMQP.IncomingPost.transactionConsumer: " <> show me)
          Right {body, ack} -> do
            -- NOTE. Transaction execution seems to be so slow, that the connection can be lOst before we acknowledge.
            -- In that case, the broker resends the message.
            -- That is why we acknowledge first.
            -- The risk is that the PDR may not handle the message fully and then it is lost.
            lift $ acknowledge ack
            lift do 
              padding <- transactionLevel
              log $ padding <> "Executing incoming post transaction"
              runMonadPerspectivesTransaction'
                false
                (ENR $ EnumeratedRoleType sysUser)
                (executeTransaction body)
              detectPublicStateChanges

    setConnectionState :: Boolean -> MonadPerspectives Unit
    setConnectionState c = do
      mySystem <- getMySystem
      padding <- transactionLevel
      log $ padding <> "Setting connection state to " <> show c
      void $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser) (setProperty [RoleInstance $ buitenRol mySystem] (EnumeratedPropertyType connectedToAMQPBroker) Nothing [Value $ show c])
      pure unit

    -- | Send all transactions that have accumulated in the post database while we had no connection to the Broker.
    sendOutgoingPost :: MonadPerspectives Unit
    sendOutgoingPost = do
      postDB <- postDatabaseName
      (waitingTransactions :: Array String) <- documentsInDatabase postDB excludeDocs >>= _.rows >>> map _.id >>> pure
      mstompClient <- stompClient
      case mstompClient of
        -- TODO. Order transactions to increasing timestamps (TransactionForPeer, timeStamp)
        Just stompClient -> do
          (transactions :: Array OutgoingTransaction) <- sort <<< nub <$> traverse (getDocument_ postDB) waitingTransactions
          -- We do not delete here; only when we receive the receipt.
          void $ for transactions \(OutgoingTransaction{_id, receiver, transaction}) -> liftEffect $ sendToTopic stompClient receiver _id (writeJSON transaction)
        _ -> pure unit

-- | Construct the BrokerService from the database, if possible, and set it in PerspectivesState.
retrieveBrokerService :: MonadPerspectives Unit
retrieveBrokerService = lookupIndexedContext myBrokers >>= (\mbrokers -> case mbrokers of
  Nothing -> pure Nothing
  Just brokers -> brokers ##> 
    getRoleInstances (CR $ CalculatedRoleType brokerServiceContractInUse) 
    >=> context
    >=> getRoleInstances (ENR $ EnumeratedRoleType accountHolder) 
    >=> constructBrokerServiceForUser) >>= setBrokerService


-- | Construct a BrokerService object for a particular AccountHolder.
constructBrokerServiceForUser :: RoleInstance -> MonadPerspectivesQuery BrokerService
constructBrokerServiceForUser accountHolder = do
  (Value login) <- getProperty (EnumeratedPropertyType accountHolderName) accountHolder
  (Value passcode) <- getProperty (EnumeratedPropertyType accountHolderPassword) accountHolder
  
  brokerContractExternal <- (context >=> externalRole) accountHolder
  endpointGetter <- lift $ lift $ getPropertyFunction brokerEndpoint
  (Value url) <- endpointGetter brokerContractExternal
  exchangeGetter <- lift $ lift $ getPropertyFunction brokerServiceExchange
  (Value vhost) <- exchangeGetter brokerContractExternal
  queueNameGetter <- lift $ lift $ getPropertyFunction accountHolderQueueName
  (Value queueId) <- queueNameGetter brokerContractExternal
  -- The topic is the unique identifier of this peer (his PerspectivesUsers instance).
  -- RabbitMQ will forward the messages sent to this topic to the various queues the user has, 
  -- one for each PerspectivesSystem (i.e. one for each installation).
  perspectivesUser <- lift $ lift $ getPerspectivesUser
  pure $
    { topic : (takeGuid $ unwrap perspectivesUser)
    , queueId
    , login
    , passcode
    , vhost
    , url
    }
