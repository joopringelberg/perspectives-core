// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

export function runEffectFnAff2(fn) {
  return function(a) {
    return function(b) {
        return fn(a, b);
      };
  };
};

export function runEffectFnAff3(fn) {
  return function(a) {
    return function(b) {
      return function(c){
        return fn(a, b, c);
      };
    };
  };
};

export function runEffectFnAff4(fn) {
  return function(a) {
    return function(b) {
      return function(c){
        return function(d)
        {
          return fn(a, b, c, d);
        }
      };
    };
  };
};

export function runEffectFnAff5(fn) {
  return function(a) {
    return function(b) {
      return function(c){
        return function(d){
          return function(e)
          {
            return fn(a, b, c, d, e);
          };
        };
      };
    };
  };
};

export function runEffectFnAff6(fn) {
  return function(a) {
    return function(b) {
      return function(c){
        return function(d){
          return function(e){
            return function(f){
              return fn(a, b, c, d, e, f);
            }
          }
        };
      };
    };
  };
};
