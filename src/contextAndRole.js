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
  // Do a case analysis.
  if ( c.isCompactContext )
  {
    return c;
  }
  else
  {
    return c.binnenRol;
  }
}

exports.context_buitenRol = function(c)
{
  if ( c.isCompactContext )
  {
    return c.id;
  }
  else
  {
    return c.buitenRol;
  }
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

exports.context_internalProperties = function(r)
{
  if ( !r.internalProperties )
  {
    r.internalProperties = {};
  }
  return r.internalProperties;
}

exports.context_externalProperties = function(r)
{
  if ( !r.externalProperties )
  {
    r.externalProperties = {};
  }
  return r.externalProperties;
}

exports.createCompactContext = function(c)
{
  c.isCompactContext = true;
  return c;
};

exports.createClassicContext = function(c)
{
  return c;
};

exports.isCompactContext = function(c)
{
  return c.isCompactContext;
}

// ROL
exports.rol_id = function(c)
{
  return c.id;
}

exports.rol_occurrence = function(c)
{
  if (!c.occurrence )
  {
    c.occurrence = 0;
  }
  return c.occurrence;
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
      c.binding = nothing;
    }
    return c.binding;
  }
}

exports.rol_context = function(r)
{
  if (r.isCompactContext )
  {
    return r.id;
  }
  else
  {
    return r.context;
  }
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

///////////////////// TYPES OF CONTEXT AND ROL /////////////////////
function isClassicContext(c)
{
  return c.isClassicContext;
}

function isCompactContext(c)
{
  return c.isCompactContext;
}

exports.isBuitenRol = function(r)
{
  return r.isCompactContext;
}
