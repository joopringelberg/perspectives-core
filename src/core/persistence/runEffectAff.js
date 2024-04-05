// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

exports.runEffectFnAff2 = function runEffectFnAff2(fn) {
  return function(a) {
    return function(b) {
        return fn(a, b);
      };
  };
};

exports.runEffectFnAff3 = function runEffectFnAff3(fn) {
  return function(a) {
    return function(b) {
      return function(c){
        return fn(a, b, c);
      };
    };
  };
};

exports.runEffectFnAff4 = function runEffectFnAff4(fn) {
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

exports.runEffectFnAff5 = function runEffectFnAff5(fn) {
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

exports.runEffectFnAff6 = function runEffectFnAff6(fn) {
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
