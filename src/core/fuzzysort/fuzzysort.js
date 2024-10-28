// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import * as fuzzy from 'fuzzysort';

// fuzzysort.go('mr', ['Monitor.cpp', 'MeshRenderer.cpp'])
export function matchStringsImpl(target, alternatives)
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
