"use strict";

function connect({request, response, getter, setter})
{
  return function()
  {
      window.pproxy = new PerspectivesProxy(request, response, getter, setter);
  };
}

class PerspectivesProxy {
    constructor(request, response, getter, setter)
    {
        this.request = request;
        this.response = response;
        this.getter = getter;
        this.setter = setter;
    }

    // callback moet de functie van een QueryEffect kunnen zijn: Array String -> Eff (AjaxAvarCache (ref :: REF | e)) Unit
	getRol(contextID, rolName, callback)
	{
		const req = {
			request: "GetRol",
			contextID: contextID,
			rolName: rolName,
			reactStateSetter: function(arrString){ callback(arrString); return function(){}; }
		};
		// Dit lijkt me kwetsbaar. De unsubscriber hoort bij het request. Maar de processen zijn asynchroon!
		this.setter( req)(this.request )();
		this.getter( this.response )().then(
			function(unsubscriber)
			{
				// Save for later so you can unsubscribe.
                console.log("Received an unsubscriber");
			} );
	}
}


function test(contextID, rolName)
{
    pproxy.getRol(contextID, rolName,
		   function(rolIds)
		   {
			   console.log( "Rol " + rolName + " van context " + contextID + " is: " + rolIds );
		   });
}

window.test = test;

// export {connect};
exports.connect = connect;
