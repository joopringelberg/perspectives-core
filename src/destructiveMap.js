"use strict";

exports.getFromMap = function (m) {
  return function (k) {
    return m[k];
  };
};

exports.setInMap = function(m){
  return function(k) {
    return function(v)
    {
      return m.set( k, v );
    }
  }
};

exports.deleteFromMap = function(m){
  return function(k){
    {
      m.delete(k);
      return m;
    }
  }
}

exports.createMap = function(){
  return new Map();
};

exports.showMapImpl = function(m){
  var r = "{";
  m.forEach( function(v,k)
    {
      r = r + k.toString() + ": " + v.toString();
    });
  return r + "}";
};
