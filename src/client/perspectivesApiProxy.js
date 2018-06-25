let resolver, rejecter;

const Perspectives = new Promise(
  function (resolve, reject)
  {
    resolver = resolve;
    rejecter = reject;
  });

function connect ({request, response, getter, setter})
{
  return function ()
  {
    window.pproxy = new PerspectivesProxy(request, response, getter, setter);
    // Now resolve the promise made above for the proxy.
    resolver(window.pproxy);
  };
}

function createRequestEmitterImpl (left, right, emit)
{
  window.pproxy = new PerspectivesProxy(left, right, emit);
  // Now resolve the promise made above for the proxy.
  resolver(window.pproxy);
}


class PerspectivesProxy
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
  connect: connect,
  createRequestEmitterImpl: createRequestEmitterImpl
};
