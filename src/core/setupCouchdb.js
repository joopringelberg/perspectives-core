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

 // Emit the filler of the role.
exports.filledRoles = (function(doc)
  {
    // a proxy for being a role:
    if (doc.universeRoleDelta)
    {
      emit(doc.binding, doc.id);
    }
  }).toString();