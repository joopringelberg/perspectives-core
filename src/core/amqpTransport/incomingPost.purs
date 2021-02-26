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

module Perspectives.AMQP.IncomingPost where

import Control.Coroutine (Consumer, Producer, await, runProcess, ($$))
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (ForeignError(..), MultipleErrors)
import Foreign.Generic (encodeJSON)
import Perspectives.AMQP.Stomp (StructuredMessage, acknowledge, createStompClient, messageProducer, sendToTopic)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, BrokerService, (##>))
import Perspectives.Couchdb.Databases (documentNamesInDatabase)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.ObjectGetters (context, externalRole, getProperty, getRoleBinders)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Persistence.API (deleteDocument, getDocument_)
import Perspectives.Persistent (postDatabaseName)
import Perspectives.PerspectivesState (brokerService, setBrokerService, setStompClient, stompClient)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.Sync.Channel (postDbName)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Prelude (Unit, bind, pure, show, unit, void, ($), (>=>), (>>=), discard, (*>), (<>))

incomingPost :: MonadPerspectives Unit
incomingPost = do
  -- get the post database
  post <- postDbName
  mbrokerService <- brokerService
  case mbrokerService of
    Just {topic, queueId, login, passcode, vhost, url} -> do
      -- Create a Stomp Client: url
      stpClient <- liftEffect $ createStompClient( url )
      -- Save the client in state.
      setStompClient stpClient
      -- Create a messageProducer: ConnectAndSubscriptionParameters
      (transactionProducer :: Producer (Either MultipleErrors (StructuredMessage TransactionForPeer)) MonadPerspectives Unit) <- pure $ messageProducer stpClient
        { topic
        , queueId
        , login
        , passcode
        , vhost
        }
      void $ runProcess $ transactionProducer $$ transactionConsumer
    Nothing -> pure unit

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
            otherwise -> log ("Perspectives.AMQP.IncomingPost.transactionConsumer: " <> show me)
          Right {body, ack} -> do
            lift $ executeTransaction body
            lift $ acknowledge ack

    setConnectionState :: Boolean -> MonadPerspectives Unit
    setConnectionState c = do
      mySystem <- getMySystem
      void $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User") (setProperty [RoleInstance $ buitenRol mySystem] (EnumeratedPropertyType "model:System$PerspectivesSystem$External$ConnectedToAMQPBroker") [Value $ show c])
      pure unit

    -- | Send all transactions that have accumulated in the post database while we had no connection to the Broker.
    sendOutgoingPost :: MonadPerspectives Unit
    sendOutgoingPost = do
      postDB <- postDatabaseName
      (waitingTransactions :: Array String) <- documentNamesInDatabase postDB
      mstompClient <- stompClient
      case mstompClient of
        Just stompClient -> for_ waitingTransactions \tname -> do
          mdoc <- getDocument_ postDB tname
          case mdoc of
            Just (OutgoingTransaction{receiver, transaction}) -> do
              liftEffect $ sendToTopic stompClient receiver tname (encodeJSON transaction)
              -- We do not delete here; only when we receive the receipt.
            otherwise -> pure unit
        otherwise -> pure unit

-- | Construct the BrokerService from the database, if possible, and set it in PerspectivesState.
retrieveBrokerService :: MonadPerspectives Unit
retrieveBrokerService = getUserIdentifier >>= (\u -> (RoleInstance u) ##> constructBrokerServiceForUser) >>= setBrokerService

-- | Construct a BrokerService object for a particular user by querying the database.
constructBrokerServiceForUser :: RoleInstance -> MonadPerspectivesQuery BrokerService
constructBrokerServiceForUser userId = do
  -- LET OP: gaat misschien fout als model:BrokerServices nog niet beschikbaar is.
  accountHolder <- getRoleBinders (EnumeratedRoleType "model:BrokerServices$BrokerContract$AccountHolder") userId
  (Value login) <- getProperty (EnumeratedPropertyType "model:BrokerServices$BrokerContract$AccountHolder$AccountName") accountHolder
  (Value passcode) <- getProperty (EnumeratedPropertyType "model:BrokerServices$BrokerContract$AccountHolder$AccountPassword") accountHolder
  (Value queueId) <- getProperty (EnumeratedPropertyType "model:BrokerServices$BrokerContract$AccountHolder$QueueName") accountHolder

  brokerContractExternal <- (context >=> externalRole >=> getRoleBinders (EnumeratedRoleType "model:BrokerServices$BrokerService$Accounts") >=> context >=> externalRole) accountHolder
  (Value url) <- getProperty (EnumeratedPropertyType "model:BrokerServices$BrokerService$External$Url") brokerContractExternal
  (Value vhost) <- getProperty (EnumeratedPropertyType "model:BrokerServices$BrokerService$External$Exchange") brokerContractExternal

  pure $
    { topic : (unwrap userId)
    , queueId
    , login
    , passcode
    , vhost
    , url
    }
