// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

"use strict";

export function newMap () {
  return {};
};

export function peekImpl (m) {
  return function (k) {
    return m[k];
  };
};

export function poke (m) {
  return function (k) {
    return function (v) {
      m[k] = v;
      return m;
    };
  };
};

export function delete_ (m) {
  return function (k) {
    var r = m[k];
    delete m[k];
    return r;
  };
};

export function clear(m) {
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

export function keys(m) {
  return Object.keys(m);
}

export function values(m) {
  return Object.values(m);
}

export function filterKeys(predicate){
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
