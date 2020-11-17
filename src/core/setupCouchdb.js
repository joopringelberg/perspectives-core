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
