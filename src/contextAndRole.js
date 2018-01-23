"use strict";

// This function should call methods of ClassicContext or CompactContext respectively.
exports.rolToContextID = function(r)
{
  return r.context;
}

exports.contextToBuitenRolID = function(c)
{
  return c.buitenRol;
}

exports.contextToBinnenRol = function(c)
{
  return c.binnenRol;
}
