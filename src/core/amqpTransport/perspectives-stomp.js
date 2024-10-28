// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE directory in the projects root.

// END LICENSE

import * as Stomp from "@stomp/stompjs";


export function createStompClientImpl ( url )
{
  var client = new Stomp.Client({ brokerURL: url});

  // Disable heartbeating: we don't need it because we acknowledge explicitly.
  // Commented this out because RabbitMQ seems to hang up pretty quick.
  // client.heartbeat = {incoming: 0, outgoing: 0};

  return client;
}

// This function will be called to create a Producer of Messages.
// emitStep will be bound to the constructor Emit, finishStep will be the constructor Finish.
// login and passcode must be the RabbitMQ credentials.
// vhost must identify a virtual host on the url that the stompClient was created with.
// topic is the binding key associated with the queue we subscribe to.
// queueId is the secret identification of that queue.
// If the queue does not exist, it will be created as a durable queue that will not be deleted
// on server shutdown.
export function connectAndSubscribeImpl (stompClient, params, emitStep, finishStep, emit)
{
  stompClient.connectHeaders =
    { login: params.login
    , passcode: params.passcode
    , host: params.vhost
  };
  stompClient.debug = function (str)
    {
    // console.log(str);
  };

  stompClient.emitToPurescript = function(message)
    {
      // Emit the change to Purescript. `emit` is in Effect:
      // emit :: forall m a r. Emitter m a r -> a -> m Unit
      // so we have to apply the result we get from emit to sort the effect.
      emit( emitStep( message ) )();
    };

  stompClient.onConnect =
    function()
    {
      // result = {id, unsubscribe}
      const result = stompClient.subscribe(
        // publish to amq.topic
        "/topic/" + params.topic,
        // publish to the default exchange.
        // "/queue/" + params.queueId,
        stompClient.emitToPurescript,
        { durable: true
        , "auto-delete": false
        // This will be the id that we identify the STOMP-subscription with.
        , id: params.queueId // As soon as we create more than one subscription within a connection, we'll have to generate ids.
        , ack: "client"
        , "x-queue-name": params.queueId
        });
        emit( emitStep( {body: "connection"} ) )();
      };
    stompClient.onStompError = function (frame) {
        // Will be invoked in case of error encountered at Broker
        // Bad login/passcode typically will cause an error
        // Complaint brokers will set `message` header with a brief message. Body may contain details.
        // Compliant brokers will terminate the connection after any error
        console.log('Broker reported error: ' + frame.headers['message']);
        console.log('Additional details: ' + frame.body);
      };
    stompClient.onDisconnect = function ()
      {
        emit( emitStep( {body: "noConnection"} ) )();
      };
    stompClient.onWebSocketClose = function ()
      {
        emit( emitStep( {body: "noConnection"} ) )();
      };
    stompClient.onUnhandledMessage = function(message)
    {
      console.log( "UNHANDLED: " + message );
    };
    stompClient.activate();
  };

// Unsubscribe from a queue.
export function unsubscribeImpl( stompClient, queueId )
{
  stompClient.unsubscribe( queueId );
}

// Send a message. We do not support additional headers.
export function sendImpl( stompClient, destination, receiptId, messageString )
{
  stompClient.watchForReceipt( receiptId,
    function()
    {
      stompClient.emitToPurescript( {body: "receipt:" + receiptId} );
    })
  stompClient.publish(
    { destination: destination
    , body: messageString
    , headers: {receipt: receiptId}
    // , skipContentLengthHeader: true
    });
}
