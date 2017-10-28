"use strict";

exports.magic = function(sig) {
  var innerSig = sig.get();
  var out = make(innerSig.get());
  var f = function(val) { out.set(val); };
  innerSig.subscribe(f);
  sig.subscribe(function(val)
  {
    // No support for unsubscribe in Signal.js!
    // innerSig.unsubscribe(f);
    innerSig = val;
    out.set(innerSig.get());
  });
  return out;
};
