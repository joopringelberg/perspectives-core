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

class PerspectivesProxy
{
  constructor (request, response, getter, setter)
  {
    this.request = request;
    this.response = response;
    this.getter = getter;
    this.setter = setter;
  }

  send (req, receiveValues, handleUnsubscriber)
  {
    req.reactStateSetter = function (arrString)
    {
      receiveValues(arrString);
      return function () {};
    };
    this.setter(req)(this.request)();
    this.getter(this.response)().then(handleChannelError(handleUnsubscriber)); // doe iets met Error.
  }

  getRolBinding (contextID, rolName, receiveValues, handleUnsubscriber)
  {
    this.send(
      {request: "GetRolBinding", subject: contextID, predicate: rolName},
      receiveValues,
      handleUnsubscriber);
  }

  getRol (contextID, rolName, receiveValues, handleUnsubscriber)
  {
    this.send(
      {request: "GetRol", subject: contextID, predicate: rolName},
      receiveValues,
      handleUnsubscriber);
  }

  getProperty (rolID, propertyName, receiveValues, handleUnsubscriber)
  {
    this.send(
      {request: "GetProperty", subject: rolID, predicate: propertyName},
      receiveValues,
      handleUnsubscriber);
  }

  getBinding (rolID, receiveValues, handleUnsubscriber)
  {
    this.send(
      {request: "GetBinding", subject: rolID, predicate: ""},
      receiveValues,
      handleUnsubscriber);
  }

  getBindingType (rolID, receiveValues, handleUnsubscriber)
  {
    this.send(
      {request: "GetBindingType", subject: rolID, predicate: ""},
      receiveValues,
      handleUnsubscriber);
  }

  getViewProperties (viewName, receiveValues, handleUnsubscriber)
  {
    this.send(
      {request: "GetViewProperties", subject: viewName, predicate: ""},
      receiveValues,
      handleUnsubscriber);
  }

  getRolContext (rolID, receiveValues, handleUnsubscriber)
  {
    this.send(
      {request: "GetRolContext", subject: rolID, predicate: ""},
      receiveValues,
      handleUnsubscriber);
  }

  getContextType (contextID, receiveValues, handleUnsubscriber)
  {
    this.send(
      {request: "GetContextType", subject: contextID, predicate: ""},
      receiveValues,
      handleUnsubscriber);
  }
}

// Capture Error responses.
function handleChannelError (handleUnsubscriber)
{
  return function (message)
  {
    if (isError(message))
    {
      console.error(message.value0);
    }
    else
    {
      handleUnsubscriber(message.value0);
    }
  };
}

// A proxy testing whether o is constructed with the ApiResponse Error data constructor.
function isError (o)
{
  return o.constructor.toString().match(/function \$\$Error/);
}

window.test = function (contextID, rolName)
{
  window.pproxy.getRolBinding(
    contextID, rolName,
    function (rolIds)
    {
      console.log("The binding of " + rolName + " van context " + contextID + " is: " + rolIds);
    },
    function (unsubscriber)
    {
      // Save for later so you can unsubscribe.
      console.log("Received an unsubscriber");
    });
};

module.exports = {
  Perspectives: Perspectives,
  connect: connect
};
