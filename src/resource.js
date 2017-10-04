// module Resource
"use strict";

exports.setPropertyDefinitions = function(r)
{
  return function(pd)
  {
    return function()
    {
      r.propDefs = pd;
    };
  }
}
