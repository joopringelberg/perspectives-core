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

module Perspectives.AMQP.IncomingPost where

import Control.Coroutine (Consumer, Producer, await, runProcess, ($$))
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Array (nub, sort)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (ForeignError(..), MultipleErrors)
import Foreign.Generic (encodeJSON)
import Perspectives.AMQP.Stomp (StructuredMessage, acknowledge, createStompClient, messageProducer, sendToTopic)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, BrokerService, (##>))
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.ObjectGetters (context, externalRole, getProperty, getFilledRoles)
import Perspectives.ModelDependencies (accountHolder, accountHolderName, accountHolderPassword, accountHolderQueueName, brokerContract, brokerServiceAccounts, brokerServiceExchange, brokerServiceUrl, connectedToAMQPBroker, sysUser)
import Perspectives.ModelDependencies (brokerService) as DEP
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Persistence.API (deleteDocument, documentsInDatabase, excludeDocs, getDocument_)
import Perspectives.Persistent (postDatabaseName)
import Perspectives.PerspectivesState (brokerService, setBrokerService, setStompClient, stompClient)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Prelude (Unit, bind, pure, show, unit, void, ($), (>=>), (>>=), discard, (*>), (<>), (>>>), map, (<$>), (<<<))

incomingPost :: MonadPerspectives Unit
incomingPost = do
  -- get the post database
  post <- postDatabaseName
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
            -- NOTE. Transaction execution seems to be so slow, that the connection can be last before we acknowledge.
            -- In that case, the broker resends the message.
            -- That is why we acknowledge first.
            -- The risk is that the PDR may not handle the message fully and then it is lost.
            lift $ acknowledge ack
            lift $ executeTransaction body

    setConnectionState :: Boolean -> MonadPerspectives Unit
    setConnectionState c = do
      mySystem <- getMySystem
      void $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser) (setProperty [RoleInstance $ buitenRol mySystem] (EnumeratedPropertyType connectedToAMQPBroker) [Value $ show c])
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
          for_ transactions \t@(OutgoingTransaction{_id, receiver, transaction}) -> liftEffect $ sendToTopic stompClient receiver _id (encodeJSON transaction)
        otherwise -> pure unit

-- | Construct the BrokerService from the database, if possible, and set it in PerspectivesState.
retrieveBrokerService :: MonadPerspectives Unit
retrieveBrokerService = getUserIdentifier >>= (\u -> (RoleInstance u) ##> constructBrokerServiceForUser) >>= setBrokerService

-- | Construct a BrokerService object for a particular user by querying the database.
constructBrokerServiceForUser :: RoleInstance -> MonadPerspectivesQuery BrokerService
constructBrokerServiceForUser userId = do
  -- LET OP: gaat misschien fout als model:BrokerServices nog niet beschikbaar is.
  accountHolder <- getFilledRoles
    (ContextType brokerContract)
    (EnumeratedRoleType accountHolder)
    userId
  (Value login) <- getProperty (EnumeratedPropertyType accountHolderName) accountHolder
  (Value passcode) <- getProperty (EnumeratedPropertyType accountHolderPassword) accountHolder
  (Value queueId) <- getProperty (EnumeratedPropertyType accountHolderQueueName) accountHolder

  brokerContractExternal <- (context >=> externalRole >=> getFilledRoles (ContextType DEP.brokerService) (EnumeratedRoleType brokerServiceAccounts) >=> context >=> externalRole) accountHolder
  (Value url) <- getProperty (EnumeratedPropertyType brokerServiceUrl) brokerContractExternal
  (Value vhost) <- getProperty (EnumeratedPropertyType brokerServiceExchange) brokerContractExternal

  pure $
    { topic : (unwrap userId)
    , queueId
    , login
    , passcode
    , vhost
    , url
    }
