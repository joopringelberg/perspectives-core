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
import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Perspectives.AMQP.Stomp (StructuredMessage, createStompClient, messageProducer)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Sync.Channel (postDbName)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Perspectives.User (getCouchdbBaseURLWithCredentials)
import Prelude (Unit, bind, pure, unit, void, ($))

incomingPost :: MonadPerspectives Unit
incomingPost = do
  -- get host and port
  base <- getCouchdbBaseURLWithCredentials
  -- get the post database
  post <- postDbName
  -- Create a Stomp Client: url
  stpClient <- liftEffect $ createStompClient( "ws://94.237.110.95:15674/ws" )
  -- Save the client in state.
  -- Create a messageProducer: ConnectAndSubscriptionParameters
  (transactionProducer :: Producer (Either MultipleErrors (StructuredMessage TransactionForPeer)) MonadPerspectives Unit) <- pure $ messageProducer stpClient
    { topic : "cor"
    , queueId : "corsecret"
    , login : "cor"
    , passcode : "gEnieisleuk5r"
    , vhost : "inplace"
    }

  void $ runProcess $ transactionProducer $$ transactionConsumer
  where
    transactionConsumer :: Consumer (Either MultipleErrors (StructuredMessage TransactionForPeer)) MonadPerspectives Unit
    transactionConsumer = forever do
      change <- await
      case change of
        -- TODO: iets met deze errors doen? Loggen, naar support sturen...
        Left me -> pure unit
        Right {body, ack} -> do
          lift $ executeTransaction body
