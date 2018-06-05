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
      contextID: contextID,
      rolName: rolName,
      // receiveValues must have type: Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit
      reactStateSetter: function (arrString)
      {
        receiveValues(arrString);
        return function () {};
      }
    };
    this.setter(req)(this.request)();
    this.getter(this.response)().then(handleUnsubscriber);
  }

  getRol (contextID, rolName, receiveValues, handleUnsubscriber)
  {
    const req = {
      request: "GetRol",
      contextID: contextID,
      rolName: rolName,
      // receiveValues must have type: Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit
      reactStateSetter: function (arrString)
      {
        receiveValues(arrString);
        return function () {};
      }
    };
    this.setter(req)(this.request)();
    this.getter(this.response)().then(handleUnsubscriber);
  }
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