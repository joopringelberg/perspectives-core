// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import fuzzy from "fuzzysort";

// fuzzysort.go('mr', ['Monitor.cpp', 'MeshRenderer.cpp'])
exports.matchStringsImpl = function(target, alternatives)
{
  return fuzzy.go( target, alternatives );
};
