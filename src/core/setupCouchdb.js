exports.modelDescriptions = (function (doc)
{
  emit(doc._id, doc.contents.modelDescription);
}).toString();
