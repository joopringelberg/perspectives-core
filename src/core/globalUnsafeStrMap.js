// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

"use strict";

exports["new"] = function () {
  return {};
};

exports.peekImpl = function (m) {
  return function (k) {
    return m[k];
  };
};

exports.poke = function (m) {
  return function (k) {
    return function (v) {
      m[k] = v;
      return m;
    };
  };
};

exports["delete_"] = function (m) {
  return function (k) {
    var r = m[k];
    delete m[k];
    return r;
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

exports["keys"] = function(m) {
  return Object.keys(m);
}

exports["values"] = function(m) {
  return Object.values(m);
}

exports["filterKeys"] = function(predicate){
  return function(m){
    Object.keys(m).forEach(function(key) {
      if (!predicate( key ))
      {
        delete m[key];
      }
    });
    return m;
  };
}
