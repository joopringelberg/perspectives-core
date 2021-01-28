const fuzzy = require("fuzzysort");

// fuzzysort.go('mr', ['Monitor.cpp', 'MeshRenderer.cpp'])
exports.matchStringsImpl = function(target, alternatives)
{
  return fuzzy.go( target, alternatives );
};
