let resolver, rejecter;

const Perspectives = new Promise(
  function (resolve, reject)
  {
    resolver = resolve;
    rejecter = reject;
  });

// This function will be called from Perspectives Core if it want to set up an internal channel to a GUI.
function createRequestEmitterImpl (left, right, emit)
{
  // Resolve the Perspectives promise made above for the proxy.
  resolver(new PerspectivesProxy(new InternalChannel(left, right, emit)));
}

// Top level entry function to set up a TCP channel with a Perspectives Core endpoint.
// From module Control.Aff.Sockets:
// type TCPOptions opts = {port :: Port, host :: Host, allowHalfOpen :: Boolean | opts}
// type Port = Int
// type Host = String
function createTcpConnectionToPerspectives(options)
{
  // Resolve the Perspectives promise made above for the proxy.
  resolver(new PerspectivesProxy( new TcpChannel(options)));
}

class TcpChannel
{
  constructor (options)
  {
    const valueReceivers = {};
    this.connection = require("net").createConnection(
      options,
      function (message)
      {
        const {setterId, objects} = JSON.parse(message);
        valueReceivers[setterId](objects);
      });
    this.valueReceivers = valueReceivers;
  }

  nextRequestId ()
  {
    this.requestId = this.requestId + 1;
    return this.requestId;
  }

  close()
  {}

  send(req, receiveValues)
  {
    req.setterId = this.nextRequestId();
    this.valueReceivers[ req.setterId ] = receiveValues;
    this.connection.write(JSON.stringify(req));
  }

  unsubscribe(req)
  {
    delete this.valueReceivers[req.setterId];
    this.connection.write(
      {request: "Unsubscribe", subject: req.subject, predicate: req.predicate, setterId: req.setterId}
    );
  }
}

class InternalChannel
{
  constructor (left, right, emit)
  {
    this.left = left;
    this.right = right;
    this.emit = emit;
    this.requestId = -1;
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
    this.emit( this.right({}) )();
    this.emit = function()
    {
      throw( "This client has shut down!");
    };
  }

  send (req, receiveValues)
  {
    const proxy = this;
    // Create a correlation identifier and store 'receiveValues' with it.
    // Send the correlation identifier instead of reactStateSetter.
    req.reactStateSetter = function (arrString)
    {
      receiveValues(arrString);
      return function () {};
    };
    req.setterId = this.nextRequestId();
    this.emit( this.left(req) )();
    // return the unsubscriber.
    return function()
    {
      proxy.unsubscribe( req );
    };
  }

  unsubscribe(req)
  {
    this.send(
      {request: "Unsubscribe", subject: req.subject, predicate: req.predicate, setterId: req.setterId}
    );
  }

}

class PerspectivesProxy
{
  constructor (channel)
  {
    this.channel = channel;
  }

  // Inform the server that this client shuts down.
  // No other requests may follow this message.
  close()
  {
    this.channel.close();
  }

  send (req, receiveValues)
  {
    this.channel.send( req, receiveValues );
  }

  unsubscribe (req)
  {
    this.channel.unsubscribe(req);
  }

  getRolBinding (contextID, rolName, receiveValues)
  {
    this.send(
      {request: "GetRolBinding", subject: contextID, predicate: rolName},
      receiveValues);
  }

  getRol (contextID, rolName, receiveValues)
  {
    this.send(
      {request: "GetRol", subject: contextID, predicate: rolName},
      receiveValues);
  }

  getProperty (rolID, propertyName, receiveValues)
  {
    this.send(
      {request: "GetProperty", subject: rolID, predicate: propertyName},
      receiveValues);
  }

  getBinding (rolID, receiveValues)
  {
    this.send(
      {request: "GetBinding", subject: rolID, predicate: ""},
      receiveValues);
  }

  getBindingType (rolID, receiveValues)
  {
    this.send(
      {request: "GetBindingType", subject: rolID, predicate: ""},
      receiveValues);
  }

  getViewProperties (viewName, receiveValues)
  {
    this.send(
      {request: "GetViewProperties", subject: viewName, predicate: ""},
      receiveValues);
  }

  getRolContext (rolID, receiveValues)
  {
    this.send(
      {request: "GetRolContext", subject: rolID, predicate: ""},
      receiveValues);
  }

  getContextType (contextID, receiveValues)
  {
    this.send(
      {request: "GetContextType", subject: contextID, predicate: ""},
      receiveValues);
  }
}

module.exports = {
  Perspectives: Perspectives,
  createRequestEmitterImpl: createRequestEmitterImpl,
  createTcpConnectionToPerspectives: createTcpConnectionToPerspectives
};
