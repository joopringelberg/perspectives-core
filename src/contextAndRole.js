"use strict";

// CONTEXT
exports.context_id = function(c)
{
  return c.id;
}

exports.context_displayName = function(c)
{
  if ( !c.displayName )
  {
    c.displayName = c.id.substring( c.id.lastIndexOf("$") + 1 );
  }
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
  if ( !c.rolInContext )
  {
    c.rolInContext = {};
  }
  return c.rolInContext;
}

exports.context_comments = function(c)
{
  if (!c.comments)
  {
    c.comments = {commentBefore: [], commentAfter: []};
  }
  return c.comments;
};

// ROL
exports.rol_id = function(c)
{
  return c.id;
}

exports.rol_occurrence = function(c)
{
  if (!c.occurrence )
  {
    return 0;
  }
  else
  {
    return c.occurrence;
  }
}

exports.rol_pspType = function(c)
{
  return c.pspType;
}

exports.rol_binding_aux = function(nothing)
{
  return function(c)
  {
    if ( !c.binding )
    {
      return nothing;
    }
    else
    {
      return c.binding;
    }
  }
}

exports.rol_context = function(r)
{
  return r.context;
}

exports.rol_properties = function(r)
{
  if ( !r.properties )
  {
    r.properties = {};
  }
  return r.properties;
}

exports.rol_gevuldeRollen = function(r)
{
  if ( !r.gevuldeRollen )
  {
    r.gevuldeRollen = {};
  }
  return r.gevuldeRollen;
}

exports.rol_comments = function(r)
{
  if (!r.comments)
  {
    r.comments = {commentBefore: [], commentAfter: []};
  }
  return r.comments;
}

exports.createPerspectRol = function(r) {return r;};
