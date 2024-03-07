// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

exports.roleView = (function (doc)
{
  // a proxy for being a role:
  if (doc.universeRoleDelta)
  {
    doc.allTypes.forEach(
      function(typeId)
      {
        emit(typeId, doc.id);
      }
    );
  }
  

}).toString();

// OBSOLETE. Remove if testing shows the current definitioin of pendingInvitations works.
exports.pendingInvitations = (function(doc)
{
  if (doc.properties["model://perspectives.domains#System$Invitation$External$Message"])
  {
    emit(doc.id, doc.id);
  }
}).toString();

exports.contextView = (function (doc)
{
  emit(doc.pspType, doc.id);
}).toString();

// We want to filter the result on two criteria:
//  * the pspType should be a certain value
//  * the context should be a certain value.
// In order to abstract from the storage scheme, we only emit
// the part of the context identifier following the hash (#).
exports.roleFromContextView = (function (doc)
 {
  function takeGuid(s)
  {
    return s.substring( s.lastIndexOf("#") + 1 );
  }
  // a proxy for being a role:
  if (doc.universeRoleDelta)
  {
    doc.allTypes.forEach(
      function(typeId)
      {
        emit([typeId, takeGuid( doc.context )], doc.id);
      }
    );
  }
 }.toString())

//  This is a view in the models database.
exports.roleSpecialisations = (function( doc )
 {
   var eroles = doc.enumeratedRoles;
   Object.values( eroles ).forEach(
     function(erole)
     {
      var aspects = erole.roleAspects;
      aspects.forEach(
        function( aspectRoleInContext )
        {
          emit( aspectRoleInContext.role, erole.id );
        }
      );
     }
   );
 }.toString());

//  This is a view in the models database.
exports.contextSpecialisations = (function( doc )
 {
   Object.values( doc.contexts ).forEach(
     function(context)
     {
      var aspects = context.contextAspects;
      aspects.forEach(
        function( aspectContext )
        {
          emit( aspectContext, context.id );
        }
      );
     }
   );
 }.toString());

 exports.credentials = (function (doc)
 {
    // Only PerspectRol instances have an isMe field. roleAliases
    if (doc.isMe && doc.allTypes.find( t => t == "model://perspectives.domains#System$WithCredentials"))
    {
      emit(doc.id, doc.id);
    }
 }).toString();

////// REFERENTIAL INTEGRITY

// This view is a table [filler; filled]
// Use it by selecting on filler to obtain those roles that are filled by it.
exports.filler2filledView = (function(filled)
  {
    // a proxy for being a role:
    if (filled.universeRoleDelta)
    {
      emit(filled.binding, filled.id);
    }
  }).toString();

// This view is a table [filled; {filler, filledContextType, filledRoleType}].
// Use it by selecting on filled to obtain the role that fills it.
exports.filled2fillerView = (function(filler)
{
  // a proxy for being a role:
  if (filler.universeRoleDelta)
  {
    Object.keys( filler.filledRoles ).forEach(
      function( contextType )
      {
        const contextGroup = filler.filledRoles[contextType];
        Object.keys( contextGroup ).forEach(
          function( roleType )
          {
            contextGroup[roleType].forEach(
              function(filledRole)
              {
                emit(filledRole, {filler: filler.id, filledContextType: contextType, filledRoleType: roleType});
              });
          });
      }
    );
  }
}).toString();

// This view is a table [context; role].
// Use it by selecting on role to obtain its context.
exports.context2roleView = (function(role)
{
  // a proxy for being a role:
  if (role.universeRoleDelta)
  {
    emit( role.context, role.id);
  }
}).toString()

// This view is a table [role; context]
// Use it by selecting on context to obtain its roles.
exports.role2contextView = (function(context)
{
  // a proxy for being a context.
  if (context.universeContextDelta)
  {
    Object.values( context.rolInContext ).forEach(
      function(roles)
      {
        roles.forEach( function(role)
        {
          emit(role, context.id);
        })
      }
    );
  }
}).toString()