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
-- along with this program.  If not, see <https:--www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.AMQP.Stomp -- 3

  ( messageProducer
  , StompClient
  , createStompClient
  , Message
  , AcknowledgeFunction
  , unsubscribe
  , send
  )

where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (Emitter, Step(..), produce)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import Foreign (Foreign)

-----------------------------------------------------------
-- STOMPURL
-----------------------------------------------------------
-- The Stomp server we want to subscribe to.
type StompUrl = String

-----------------------------------------------------------
-- STOMPCLIENT
-----------------------------------------------------------
-- | Represents the type of instances of the (foreign) Javascript Stomp Client object.
-- | Use a Stomp Client to create a Producer of Messages.
foreign import data StompClient :: Type

foreign import createStompClientImpl :: EffectFn1 StompUrl StompClient

createStompClient :: StompUrl -> Effect StompClient
createStompClient stompUrl = runEffectFn1 createStompClientImpl stompUrl

-----------------------------------------------------------
-- CONNECTANDSUBSCRIBE
-----------------------------------------------------------
type QueueId = String

type ConnectAndSubscriptionParameters =
  { topic :: String       -- the routing / binding key
  , queueId :: QueueId    -- the (secret) identification of the queue that we subscribe to.
  , login :: String      -- RabbitMQ login name
  , passcode :: String   -- RabbitMQ password
  , vhost :: String       -- vhost name on the RabbitMQ server that the user credentials belong to.
  }

-- | We model a parameterless acknowledgement function, as we have no use case for parameters (yet).
type AcknowledgeFunction = Unit -> Unit

type Message =
  { body :: Foreign
  , ack :: AcknowledgeFunction}

foreign import connectAndSubscribeImpl :: EffectFn5
  StompClient
  ConnectAndSubscriptionParameters
  (Message -> Step Message Unit)
  (Unit -> Step Message Unit)
  (Emitter Effect Message Unit)
  Unit

-- | Takes an EventSource and produces a function that takes an Emitter.
-- | Apply `produce` or `produce'` to it to create a Producer of Foreign.
connectAndSubscribe ::
  StompClient ->
  ConnectAndSubscriptionParameters ->
  (Emitter Effect Message Unit) ->
  Effect Unit
connectAndSubscribe stompClient params = runEffectFn5 connectAndSubscribeImpl
  stompClient
  params
  Emit
  Finish

-----------------------------------------------------------
-- MESSAGEPRODUCER
-----------------------------------------------------------
-- | A Producer of Messages.
messageProducer :: StompClient -> ConnectAndSubscriptionParameters -> Producer Message Aff Unit
messageProducer stompClient params = produce (connectAndSubscribe stompClient params)

-----------------------------------------------------------
-- UNSUBSCRIBE
-----------------------------------------------------------

foreign import unsubscribeImpl :: EffectFn2 StompClient QueueId Unit

-- | Unsubscribe from the queue.
unsubscribe :: StompClient -> QueueId -> Effect Unit
unsubscribe stompClient queueId = runEffectFn2 unsubscribeImpl stompClient queueId

-----------------------------------------------------------
-- SEND
-----------------------------------------------------------
type Destination = String

foreign import sendImpl :: EffectFn3 StompClient Destination String Unit

-- | Send a string to a destination.
send :: StompClient -> Destination -> String -> Effect Unit
send stompClient destination messageString = runEffectFn3 sendImpl stompClient destination messageString
