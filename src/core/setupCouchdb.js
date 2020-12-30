// TODO. The views below emit `doc` while Couchdb documentation warns against it:
// http://127.0.0.1:5984/_utils/docs/ddocs/views/intro.html

exports.modelDescriptions = (function (doc)
{
  emit(doc._id, doc.contents.modelDescription);
}).toString();

exports.roleView = (function (doc)
{
  emit(doc.contents.pspType, doc);
}).toString();

exports.pendingInvitations = (function(doc)
{
  if (doc.contents.properties["model:System$Invitation$External$Message"])
  {
    emit(doc._id, doc);
  }
}).toString();

exports.contextView = (function (doc)
{
  emit(doc.contents.pspType, doc);
}).toString();

// We want to filter the result on two criteria:
//  * the pspType should be a certain value
//  * the context should be a certain value.
exports.roleFromContextView = (function (doc)
 {
   // a proxy for being a role:
   if (doc.contents.universeRoleDelta)
   {
     emit( [doc.contents.pspType, doc.contents.context], doc._id );
   }
 }.toString())
