// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

var PouchDB = require('pouchdb-browser').default;

exports.createDatabaseImpl = function( databaseName )
{
  return new PouchDB( databaseName );
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

function convertPouchError( e )
{
  // Pouchdb returns an object in case of promise rejection.
  // But sometimes it returns a TypeError. We convert that to the same form.
  // We then stringify the objects and make a Javascript Error out of it.
  // By throwing a Javascript error, we make toAff catch it
  // and coerce it to an error in MonadError.
  // This is caught by catchError in Purescript, e.g. in addDocument.

  if ( e instanceof TypeError )
  {
    throw new Error( JSON.stringify(
      { name: e.name
      , message: e.message
      , error: e.stack})
    );
  }
  else
  {
    throw new Error( JSON.stringify( e ) );
  }
}

exports.getDocumentImpl = function( database, docName )
{
  return database.get( docName ).catch( convertPouchError );
}
