"use strict";

exports["new"] = function () {
  return {};
};

exports.peekImpl = function (m) {
  return function (k) {
    return function()
      {
        return m[k];
      };
  };
};

exports.poke = function (m) {
  return function (k) {
    return function (v) {
      return function () {
        m[k] = v;
        return m;
      };
    };
  };
};

exports["delete"] = function (m) {
  return function (k) {
    return function () {
      delete m[k];
      return m;
    };
  };
};

exports["clear"] = function(m) {
  return function ()
  {
    Object.keys(m).forEach(
      function(k)
      {
        delete m[k];
      }
    );
  }
}
