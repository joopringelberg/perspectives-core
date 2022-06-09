// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

exports.modelDescriptions = (function (doc)
{
  emit(doc._id, doc.contents.modelDescription);
}).toString();

exports.roleView = (function (doc)
{
  emit(doc.contents.pspType, doc._id);
}).toString();

exports.pendingInvitations = (function(doc)
{
  if (doc.contents.properties["model:System$Invitation$External$Message"])
  {
    emit(doc._id, doc._id);
  }
}).toString();

exports.contextView = (function (doc)
{
  emit(doc.contents.pspType, doc._id);
}).toString();

// We want to filter the result on two criteria:
//  * the pspType should be a certain value
//  * the context should be a certain value.
exports.roleFromContextView = (function (doc)
 {
   // a proxy for being a role:
   if (doc.contents.universeRoleDelta)
   {
     doc.contents.allTypes.forEach(
       function(typeId)
       {
         emit([typeId, doc.contents.context], doc._id);
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