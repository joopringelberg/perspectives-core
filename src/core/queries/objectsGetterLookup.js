"use strict";

exports.addToArray = function (a) {
  return function(arr)
  {
    arr.push(a);
    return arr;
  };
};
