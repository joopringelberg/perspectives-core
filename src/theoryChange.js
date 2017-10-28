"use strict";

exports.saveChangedObject_ = function(triple)
{
  return function(obj)
  {
    return function()
    {
      triple.object = obj;
      return {};
    };
  };
};
