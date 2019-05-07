"use strict";

exports.saveChangedObject = function(triple)
{
  return function(obj)
  {
    return function()
    {
      triple.object = obj;
      console.log( "Triple " + triple.subject + "-" + triple.predicate + " has now object: " +  triple.object);
      return triple;
    };
  };
};
