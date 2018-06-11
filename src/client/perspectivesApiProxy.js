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

  getRolBinding (contextID, rolName, receiveValues, handleUnsubscriber)
  {
    const req = {
      request: "GetRolBinding",
      subject: contextID,
      predicate: rolName,
      // receiveValues must have type: Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit
      reactStateSetter: function (arrString)
      {
        receiveValues(arrString);
        return function () {};
      }
    };
    this.setter(req)(this.request)();
    this.getter(this.response)().then(handleChannelError(handleUnsubscriber)); // doe iets met Error.
  }

  getRol (contextID, rolName, receiveValues, handleUnsubscriber)
  {
    const req = {
      request: "GetRol",
      subject: contextID,
      predicate: rolName,
      // receiveValues must have type: Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit
      reactStateSetter: function (arrString)
      {
        receiveValues(arrString);
        return function () {};
      }
    };
    this.setter(req)(this.request)();
    this.getter(this.response)().then(handleChannelError(handleUnsubscriber));
  }

  getProperty (rolID, propertyName, receiveValues, handleUnsubscriber)
  {
    const req = {
      request: "GetProperty",
      subject: rolID,
      predicate: propertyName,
      // receiveValues must have type: Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit
      reactStateSetter: function (arrString)
      {
        receiveValues(arrString);
        return function () {};
      }
    };
    this.setter(req)(this.request)();
    this.getter(this.response)().then(handleChannelError(handleUnsubscriber));
  }

  getBinding (rolID, receiveValues, handleUnsubscriber)
  {
    const req = {
      request: "GetBinding",
      subject: rolID,
      predicate: "",
      // receiveValues must have type: Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit
      reactStateSetter: function (arrString)
      {
        receiveValues(arrString);
        return function () {};
      }
    };
    this.setter(req)(this.request)();
    this.getter(this.response)().then(handleChannelError(handleUnsubscriber));
  }

  getBindingType (rolID, receiveValues, handleUnsubscriber)
  {
    const req = {
      request: "GetBindingType",
      subject: rolID,
      predicate: "",
      // receiveValues must have type: Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit
      reactStateSetter: function (arrString)
      {
        receiveValues(arrString);
        return function () {};
      }
    };
    this.setter(req)(this.request)();
    this.getter(this.response)().then(handleChannelError(handleUnsubscriber));
  }

}
// Capture Error responses.
function handleChannelError(handleUnsubscriber)
{
  return function(message)
  {
    if (isError(message))
    {
      console.error( message.value0 );
    }
    else
    {
      handleUnsubscriber( message.value0 );
    }
  }
}

// A proxy testing whether o is constructed with the ApiResponse Error data constructor.
function isError(o)
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

// exports.connect = connect;

module.exports = {
  Perspectives: Perspectives,
  connect: connect
};
