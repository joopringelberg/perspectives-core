// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

"use strict";

export const empty = {};

export function _lookup (no, yes, k, m) {
  return k in m ? yes(m[k]) : no;
};

export function _pushFrame(env) {
  var fr = {};
  Object.setPrototypeOf(fr, env);
  return fr;
}

export function _addVariable(varName, value, environment) {
  environment[varName] = value;
  return environment;
}

// (Environment a) -> (Array (Object a))
// The first element corresponds to the frame at the top of the stack.
export function _toObjectArray( env ) {
  // The prototype of the prototype of empty is null.
  var proto = Object.getPrototypeOf( env );
  var accumulator = [env];
  while (proto) {
    accumulator.push(proto);
    proto = Object.getPrototypeOf( proto );
  }
  return accumulator;
}

export function _fromObjectArray( frames ) {
  var i = 0;
  while (i < length( frames ) - 1 ) {
    Object.setPrototypeOf( frames[i], frames[ i + 1]);
    i++;
  }
  return frames[0];
}
