"use strict";

exports.addToArray = function (a) {
  return function(arr)
  {
    arr.push(a);
    return arr;
  };
};

exports.objectsGettersEqual = function(g1)
{
  return function(g2)
  {
    return g1 == g2;
  }
}
