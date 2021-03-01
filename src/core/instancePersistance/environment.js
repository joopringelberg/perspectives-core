// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

"use strict";

exports.empty = {};

exports._lookup = function (no, yes, k, m) {
  return k in m ? yes(m[k]) : no;
};

exports._pushFrame = function(env) {
  var fr = {};
  Object.setPrototypeOf(fr, env);
  return fr;
}

exports._addVariable = function(varName, value, environment) {
  environment[varName] = value;
  return environment;
}

// (Environment a) -> (Array (Object a))
// The first element corresponds to the frame at the top of the stack.
exports._toObjectArray = function( env ) {
  // The prototype of the prototype of empty is null.
  var proto = Object.getPrototypeOf( env );
  var accumulator = [env];
  while (proto) {
    accumulator.push(proto);
    proto = Object.getPrototypeOf( proto );
  }
  return accumulator;
}

exports._fromObjectArray = function( frames ) {
  var i = 0;
  while (i < length( frames ) - 1 ) {
    Object.setPrototypeOf( frames[i], frames[ i + 1]);
    i++;
  }
  return frames[0];
}
