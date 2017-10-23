"use strict";

exports.addDependency = function(triple)
{
  return function(tripleRef)
  {
    return function()
    {
      triple.dependencies.push(tripleRef);
      return tripleRef;
    };
  };
};
