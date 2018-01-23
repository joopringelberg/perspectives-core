"use strict";

// CONTEXT
exports.context_id = function(c)
{
  return c.id;
}

exports.context_displayName = function(c)
{
  return c.displayName;
}

exports.context_pspType = function(c)
{
  return c.pspType;
}

exports.context_binnenRol = function(c)
{
  return c.binnenRol;
}

exports.context_buitenRol = function(c)
{
  return c.buitenRol;
}

exports.context_rolInContext = function(c)
{
  return c.rolInContext;
}

exports.context_comments = function(c)
{
  return c.comments;
}

// ROL
// This function should call methods of ClassicContext or CompactContext respectively.
exports.rol_id = function(c)
{
  return c.id;
}

exports.rol_occurrence = function(c)
{
  return c.occurrence;
}

exports.rol_pspType = function(c)
{
  return c.pspType;
}

exports.rol_binding = function(c)
{
  return c.binding;
}

exports.rol_context = function(r)
{
  return r.context;
}

exports.rol_properties = function(r)
{
  return r.properties;
}

exports.rol_gevuldeRollen = function(r)
{
  return r.gevuldeRollen;
}

exports.rol_comments = function(r)
{
  return r.comments;
}
