// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
// Full text of this license can be found in the LICENSE file in the projects root.

// END LICENSE

var Stomp = require( "stompjs/lib/stomp.js" ).Stomp;


function createStompClientImpl ( url )
{
  var client = Stomp.client(url);

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
function connectAndSubscribeImpl (stompClient, params, emitStep, finishStep, emit)
{
  var headers = {
        login: params.login,
        passcode: params.passcode,
        host: params.vhost
      };
  stompClient.connect
    ( headers
    , function()
      {
        // result = {id, unsubscribe}
        const result = stompClient.subscribe(
          "/topic/" + params.topic,
          function(message)
          {
            // Emit the change to Purescript. `emit` is in Effect:
            // emit :: forall m a r. Emitter m a r -> a -> m Unit
            // so we have to apply the result we get from emit to sort the effect.
            emit( emitStep( message ) )();
          },
          { durable: true
          , "auto-delete": false
          , id: params.queueId
          , ack: "client"
          });
      }
    , function (error)
      {

      }
    );
  };

// Unsubscribe from a queue.
function unsubscribeImpl( stompClient, queueId )
{
  stompClient.unsubscribe( queueId );
}

// Send a message. We do not support additional headers.
function sendImpl( stompClient, destination, messageString )
{
  stompClient.send( destination, {}, messageString );
}

module.exports =
  { createStompClientImpl: createStompClientImpl
  , connectAndSubscribeImpl: connectAndSubscribeImpl
  , unsubscribeImpl: unsubscribeImpl
  , sendImpl: sendImpl
  }
