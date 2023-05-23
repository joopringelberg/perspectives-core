// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

exports.roleView = (function (doc)
{
  // a proxy for being a role:
  if (doc.contents.universeRoleDelta)
  {
    doc.contents.allTypes.forEach(
      function(typeId)
      {
        emit(typeId, doc.contents._id);
      }
    );
  }
  

}).toString();

exports.pendingInvitations = (function(doc)
{
  if (doc.contents.properties["model:System$Invitation$External$Message"])
  {
    emit(doc.contents._id, doc.contents._id);
  }
}).toString();

exports.contextView = (function (doc)
{
  emit(doc.contents.pspType, doc.contents._id);
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
   if (doc.contents.universeRoleDelta)
   {
     doc.contents.allTypes.forEach(
       function(typeId)
       {
         emit([typeId, takeGuid( doc.contents.context )], doc.contents._id);
       }
     );
   }
 }.toString())

exports.roleSpecialisations = (function( doc )
 {
   var eroles = doc.contents.enumeratedRoles;
   Object.values( eroles ).forEach(
     function(erole)
     {
      var aspects = erole.contents.roleAspects;
      aspects.forEach(
        function( aspectRoleInContext )
        {
          emit( aspectRoleInContext.role, erole.contents._id );
        }
      );
     }
   );
 }.toString());

 exports.contextSpecialisations = (function( doc )
 {
   Object.values( doc.contents.contexts ).forEach(
     function(context)
     {
      var aspects = context.contents.contextAspects;
      aspects.forEach(
        function( aspectContext )
        {
          emit( aspectContext, context.contents._id );
        }
      );
     }
   );
 }.toString());

 exports.credentials = (function (doc)
 {
    // Only PerspectRol instances have an isMe field. roleAliases
    if (doc.contents.isMe && doc.contents.allTypes.find( t => t == "model://perspectives.domains#System$WithCredentials"))
    {
      emit(doc.contents._id, doc.contents._id);
    }
 }).toString();