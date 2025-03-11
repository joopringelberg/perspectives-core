
// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later


let internalChannel, internalChannelResolver, internalChannelRejecter;

// This promise will resolve to an instance of the InternalChannel.
// It is used by function / module handleClientRequest that runs in the same execution context as the core.
const internalChannelPromise = new Promise(
  function (resolve, reject)
  {
    internalChannelResolver = resolve;
    internalChannelRejecter = reject;
  });

////////////////////////////////////////////////////////////////////////////////
//// SERVER SIDE RESOLVER TO INTERNAL CHANNEL
////////////////////////////////////////////////////////////////////////////////

// This function will be called from Perspectives Core if it want to set up an internal channel to a GUI.
// emitStep will be bound to the constructor Emit, finishStep will be the constructor Finish.
// Notice that it can only be called once with an actual effect on the value of the Promise
// (promises can only be resolved once).
export function createRequestEmitterImpl (emitStep, finishStep, emit)
{
  try
  {
    // Resolve internalChannelPromise made above.
    internalChannel = new InternalChannel(emitStep, finishStep, emit);
    internalChannelResolver (internalChannel);
  }
  catch(e)
  {
    internalChannelRejecter(e);
  }
}

export function retrieveRequestEmitterImpl (emit)
{
  internalChannel.setEmit( emit );
}


////////////////////////////////////////////////////////////////////////////////
//// INTERNAL CHANNEL
////////////////////////////////////////////////////////////////////////////////
class InternalChannel
{
  // emitStep will be bound to the constructor Emit, finishStep will be the constructor Finish.
  // emit must be bound to an Effect producing function.
  constructor (emitStep, finishStep, emit)
  {
    this.emitStep = emitStep;
    this.finishStep = finishStep;
    this.emit = emit;
    this.requestId = -1;
  }

  setEmit (emit)
  {
    this.emit = emit;
  }

  nextRequestId ()
  {
    this.requestId = this.requestId + 1;
    return this.requestId;
  }

  // Inform the server that this client shuts down.
  // No other requests may follow this message.
  close()
  {
    this.emit( this.finishStep({}) )();
    this.emit = function()
    {
      throw( "This client has shut down!");
    };
  }

  // Returns a promise for unsubscriber information of the form: {subject: req.subject, corrId: req.corrId}
  send ( req )
  {
    const proxy = this;
    const setter = req.reactStateSetter;
    // Create a correlation identifier and store it in the request.
    if ( !req.corrId )
    {
      req.corrId = this.nextRequestId();
    }
    // console.log( req );

    // this.emit has Purescript type:
    //    newtype Emitter m a r = Emitter (Step a r -> m Unit)
    // where m is Effect.
    // The Step a r is constructed by this.emitStep (which comes from Purescript as well).
    // Hence, calling this.emit returns a Unit result (that we are not interested in here)
    // in Effect. To actually compute that, we have to apply it (to zero arguments).
    this.emit( this.emitStep(req) )();
    // return a promise for the elementary data for unsubscribing.
    return new Promise( function( resolver /*.rejecter*/)
      {
        resolver( {subject: req.subject, corrId: req.corrId} );
      } );

  }

  unsubscribe(req)
  {
    this.send(
      {request: "Unsubscribe", subject: req.subject, predicate: req.predicate, setterId: req.setterId}
    );
  }

}

function corrId2ChannelId (corrId)
{
  return Math.floor(corrId / 1000000);
}

////////////////////////////////////////////////////////////////////////////////
//// HANDLE REQUESTS COMING IN THROUGH CHANNELS FROM CLIENTS
////////////////////////////////////////////////////////////////////////////////
let pdrResolver, pdrRejecter;
let pdrStartedIsResolved = false;
const pdrStarted = new Promise(function( resolver, rejecter)
  {
    pdrResolver = resolver;
    pdrRejecter = rejecter;
  }
  );

// These calls are implemented in accordance with the types of the functions in the core.
// The callbacks are declared as Effects, there, hence we treat them here that way.
// We could cheat and provide callbacks that do not return an Effect.
// NOTE that requests are received through the Channel Messaging API calls from clients. 
// `handleClientRequest` deals with them by using the InternalChannel's send function, 
// that has been connected by the PDR with the stream of requests the PerspectivesAPI handles.
// channels is an array of MessagePort objects. See: https://developer.mozilla.org/en-US/docs/Web/API/MessagePort
export function handleClientRequest( pdr, channels, request )
{
  const req = request.data;
  if (req.proxyRequest)
  {
    // The request can be handled right here in the SharedWorker itself.
    switch (req.proxyRequest)
    {
      case "pdrStarted":
        // This will always return an answer: it is not dependent on whether the PDR has actually been started.
        channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "pdrStarted", pdrStarted: pdrStartedIsResolved});
        break;
      case "isUserLoggedIn":
        //{proxyRequest: "isUserLoggedIn", channelId: proxy.channelId}
        internalChannelPromise.then( function ()
          {
            // We return true because the sharedworker is active.
            pdrStarted
              .then(() => channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "isUserLoggedIn", isUserLoggedIn: true}))
              .catch(() => channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "isUserLoggedIn", isUserLoggedIn: false}));
          });
        break;
      case "resetAccount":
        pdr.resetAccount( req.username) (req.pouchdbuser) (req.options)
          // eslint-disable-next-line no-unexpected-multiline
          (function(success) // (Boolean -> Effect Unit)
            {
              return function() //  This function is the result of the call to resetAccount: the Effect.
              {
                channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "resetAccount", resetSuccesful: success });
              };
            })(); // The core resetAccount function results in an Effect, hence we apply it to return the (boolean) result.
        break;
      case "reCreateInstances":
        pdr.reCreateInstances (req.pouchdbuser) (req.options) 
          // eslint-disable-next-line no-unexpected-multiline
          (function(success) // (Boolean -> Effect Unit)
            {
              return function() //  This function is the result of the call to reCreateInstances: the Effect.
              {
                channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "reCreateInstances", reCreateSuccesful: success });
              };
            })(); // The core reCreateInstances function results in an Effect, hence we apply it to return the (boolean) result.
        break;
      case "recompileLocalModels":
        pdr.recompileLocalModels(req.pouchdbuser) 
          // eslint-disable-next-line no-unexpected-multiline
          (function(success) // (Boolean -> Effect Unit)
            {
              return function() //  This function is the result of the call to recompileLocalModels: the Effect.
              {
                channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "recompileLocalModels", recompileSuccesful: success });
              };
            })(); // The core recompileLocalModels function results in an Effect, hence we apply it to return the (boolean) result.
        break;
      case "createAccount":
        pdr.createAccount( req.username) (req.pouchdbuser) (req.runtimeOptions) (req.identityDocument)
          // eslint-disable-next-line no-unexpected-multiline
          (function({success, reason}) // ({success :: Boolean, reason :: Nullable String} -> Effect Unit)
            {
              return function() //  This function is the result of the call to createAccount: the Effect.
              {
                channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "createAccount", createSuccesful: {success, reason} });
              };
            })(); // The core createAccount function results in an Effect, hence we apply it to return the (boolean) result.
        break;
      case "removeAccount":
        pdr.removeAccount( req.username) (req.pouchdbuser) 
          // eslint-disable-next-line no-unexpected-multiline
          (function(success) // (Boolean -> Effect Unit)
            {
              return function() //  This function is the result of the call to removeAccount: the Effect.
              {
                channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "removeAccount", removeSuccesful: success });
              };
            })(); // The core removeAccount function results in an Effect, hence we apply it to return the (boolean) result.
        break;
      case "runPDR":
        // runPDR :: UserName -> PouchdbUser RuntimeOptions -> Effect Unit
        try
          {
            pdr.runPDR( req.username) (req.pouchdbuser) (req.options)
              // eslint-disable-next-line no-unexpected-multiline
              (function(success) // (Boolean -> Effect Unit), the callback.
              {
                return function() // This function is the Effect that is returned.
                {
                  if (success)
                  {
                    pdrStartedIsResolved = true;
                    pdrResolver(true);
                  }
                  else
                  {
                    pdrRejecter(false);
                  }
                  channels[corrId2ChannelId(req.channelId)].postMessage({responseType: "WorkerResponse", serviceWorkerMessage: "runPDR", startSuccesful: success });
                  return {};
                };
              })();
            break;
          }
          catch (e)
          {
            // Return the error message to the client.
            channels[corrId2ChannelId(req.channelId)].postMessage({serviceWorkerMessage: "runPDR", error: e });
          }
        break;
      case "close":
        internalChannelPromise.then( ic => ic.close() );
        break;
      case "unsubscribe":
        internalChannelPromise.then( ic => ic.unsubscribe( req.request ) );
        break;
    }
  }
  else
  {
    // The request represents a call to the PDR.
    // The original client callback was saved in the SharedWorkerChannel (on the other side, i.e. the client side) and associated with the corrId.
    // Replace the callback with a function that passes on the response to the right channel.
    // The SharedWorkerChannel will apply the original client callback.
    req.reactStateSetter = function( result )
      {
        return function()
        {
          channels[corrId2ChannelId(result.corrId)].postMessage( result );
        };
      };
    // Now call the PDR.
    internalChannelPromise.then( ic => ic.send( req ) );
  }
}
