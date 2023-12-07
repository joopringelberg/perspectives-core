// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

const fuzzy = require("fuzzysort");

// fuzzysort.go('mr', ['Monitor.cpp', 'MeshRenderer.cpp'])
exports.matchStringsImpl = function(target, alternatives)
{
  if (target == "")
  {
    return alternatives.map( function (s) {return {target: s}});
  }
  else
  {
    return fuzzy.go( target, alternatives );
  }
};
