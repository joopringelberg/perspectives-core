
var PouchDB = require('pouchdb-browser').default;

exports.createDatabaseImpl = function( databaseName )
{
  return new Pouchdb( databaseName );
}

exports.createRemoteDatabaseImpl = function( databaseName, couchdbUrl )
{
  var P = PouchDB.defaults({ prefix: couchdbUrl });
  return new P(databaseName);
}

exports.addDocumentImpl = function( database, doc, docName )
{
  return database.put(doc).catch( convertPouchError );
}

exports.deleteDocumentImpl = function( database, docName, rev )
{
  return database.remove(docName, rev).catch( convertPouchError );
}

// TODO. Zodra we een encoding toepassen waarbij _rev en _id bewaard blijven, is deze functie overbodig.
exports.addNameAndVersionHack = function( doc, name, rev)
{
  doc._id = name;
  if ( rev !== "")
  {
    doc._rev = rev;
  }
  return doc;
}

function convertPouchError( obj )
{
  // Pouchdb returns an object in case of promise rejection.
  // By throwing a Javascript error, we make toAff catch it
  // and coerce it to an error in MonadError.
  // This is caught by catchError in Purescript, e.g. in addDocument.
  throw new Error( JSON.stringify(obj))
}

exports.getDocumentImpl = function( database, docName )
{
  return database.get( docName ).catch( convertPouchError );
}
