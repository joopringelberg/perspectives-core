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
-- along with this program.  If not, see <https:--www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | This module wraps part of Stompjs.
-- | See:
-- |  * https://github.com/stomp-js/stompjs
-- |  * https://stomp-js.github.io/guide/stompjs/using-stompjs-v5.html
-- |  * https://stomp-js.github.io/api-docs/latest/classes/Client.html

module Perspectives.AMQP.Stomp

  ( messageProducer
  , StompClient
  , createStompClient
  , Message
  , AcknowledgeFunction
  , AcknowledgementHeaders
  , acknowledge
  , unsubscribe
  , send
  , sendToTopic
  , sendToQueue
  , StructuredMessage
  , ConnectAndSubscriptionParameters
  , QueueId
  )

where

import Prelude

import Control.Coroutine (Producer, transform, ($~))
import Control.Coroutine.Aff (Emitter, Step(..), produce')
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.List.NonEmpty (cons, singleton)
import Data.Maybe (Maybe, fromJust, isJust)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn4, runEffectFn5)
import Foreign (ForeignError(..), MultipleErrors)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (getFirstMatch)
import Perspectives.Persistence.Types (MonadPouchdb)
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier(..))
import Simple.JSON (class ReadForeign, readJSON')

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

type ConnectAndSubscriptionParameters p =
  { topic :: String       -- the routing / binding key
  , queueId :: QueueId    -- the (secret) identification of the queue that we subscribe to.
  , login :: String       -- RabbitMQ login name
  , passcode :: String    -- RabbitMQ password
  , vhost :: String       -- vhost name on the RabbitMQ server that the user credentials belong to.
  | p
  }

-- -- | We model a parameterless acknowledgement function, as we have no use case for parameters (yet).
-- type AcknowledgeFunction = Unit -> Unit
type AcknowledgementHeaders = {}
type AcknowledgeFunction = EffectFn1 AcknowledgementHeaders Unit

type Message =
  { body :: String
  , ack :: AcknowledgeFunction}

foreign import connectAndSubscribeImpl :: forall p. EffectFn5
  StompClient
  (ConnectAndSubscriptionParameters p)
  (Message -> Step Message Unit)
  (Unit -> Step Message Unit)
  (Emitter Effect Message Unit)
  Unit

-- | Takes an EventSource and produces a function that takes an Emitter.
-- | Apply `produce` or `produce'` to it to create a Producer of Foreign.
connectAndSubscribe :: forall p.
  StompClient ->
  (ConnectAndSubscriptionParameters p) ->
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
messageProducer' :: forall f p. StompClient -> ConnectAndSubscriptionParameters p -> Producer Message (MonadPouchdb f) Unit
messageProducer' stompClient params = produce' (connectAndSubscribe stompClient params)

type StructuredMessage f =
  { body :: f
  , ack :: AcknowledgeFunction}

messageProducer :: forall t f p. ReadForeign t => StompClient -> ConnectAndSubscriptionParameters p -> Producer (Either MultipleErrors (StructuredMessage t)) (MonadPouchdb f) Unit
messageProducer stompClient params = (messageProducer' stompClient params) $~ (forever (transform decodeMessage))
  where
    decodeMessage :: Message -> Either MultipleErrors (StructuredMessage t)
    decodeMessage {body, ack} = case runExcept $ readJSON' body of
      Left e -> case body of
            "noConnection" -> Left $ singleton $ ForeignError "noConnection"
            "connection" -> Left $ singleton $ ForeignError "connection"
            -- NOTICE that we misuse / overload the TypeMismatch constructor here for our purposes.
            s | isAReceipt s -> Left $ singleton (TypeMismatch "receipt" (unsafePartial $ fromJust $ getReceipt s))
            otherwise -> Left $ cons (ForeignError body) e
      Right m -> Right {body: m, ack}

    isAReceipt :: String -> Boolean
    isAReceipt = isJust <<< getReceipt

    getReceipt :: String -> Maybe String
    getReceipt = getFirstMatch (unsafeRegex "receipt:(.+)" noFlags)

-----------------------------------------------------------
-- ACKNOWLEDGE
-----------------------------------------------------------

acknowledge :: forall f. AcknowledgeFunction -> MonadPouchdb f Unit
acknowledge ack = liftEffect $ runEffectFn1 ack {}

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
type Queue = String
type MessageId = String

foreign import sendImpl :: EffectFn4 StompClient Destination MessageId String Unit

-- | Send a string to a destination.
-- | Destinations start either with "/topic/" or with "/queue/".
send :: StompClient -> Destination -> MessageId -> String -> Effect Unit
send stompClient destination messageId messageString = runEffectFn4 sendImpl stompClient destination messageId messageString

sendToTopic :: StompClient -> UnschemedResourceIdentifier -> MessageId -> String -> Effect Unit
sendToTopic stompClient (UnschemedResourceIdentifier topic) messageId messageString = runEffectFn4 sendImpl stompClient ("/topic/" <> topic) messageId messageString

sendToQueue :: StompClient -> Queue -> MessageId -> String -> Effect Unit
sendToQueue stompClient queue messageId messageString = runEffectFn4 sendImpl stompClient ("/queue/" <> queue) messageId messageString
