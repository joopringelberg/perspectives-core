exports.modelDescriptions = (function (doc)
{
  emit(doc._id, doc.contents.modelDescription);
}).toString();

exports.roleView = (function (doc)
{
  emit(doc.contents.pspType, doc);
}).toString();
