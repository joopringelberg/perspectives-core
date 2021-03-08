// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

var PouchDB = require('pouchdb-browser').default;

// TODO. Zodra we een encoding toepassen waarbij _rev en _id bewaard blijven, is deze functie overbodig.
exports.addNameAndVersionHack = function( doc, name, rev)
{
  doc._id = name;
  if ( rev == "")
  {
    delete doc._rev;
  }
  else
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
  // It even sometimes (e.g. in addAttachment) returns a Response type object.

  if ( e instanceof TypeError )
  {
    return new Error( JSON.stringify(
      { name: e.name
      , message: e.message
      , error: e.stack})
    );
  }
  else if (e instanceof Response)
  {
    return new Error( JSON.stringify(
      { status: e.status
      , name: "response"
      , message: e.statusText
      , error: true
      , docId: e.url
      }
    ));
  }
  else
  {
    return new Error( JSON.stringify( e ) );
  }
}

exports.createDatabaseImpl = function( databaseName )
{
  return new PouchDB( databaseName );
}

exports.createRemoteDatabaseImpl = function( databaseName, couchdbUrl )
{
  var P = PouchDB.defaults({ prefix: couchdbUrl });
  return new P(databaseName);
}

exports.deleteDatabaseImpl = function ( database ) {
  return function (onError, onSuccess) {
    database.destroy( function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) ); // invoke the error callback in case of an error
        }
        else
        {
          onSuccess(response); // invoke the success callback with the reponse
        }
      });

    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};

exports.documentsInDatabaseImpl = function( database ) {
  return function (onError, onSuccess) {
    database.allDocs( function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) ); // invoke the error callback in case of an error
        }
        else
        {
          onSuccess(response); // invoke the success callback with the reponse
        }
      });

    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};

exports.databaseInfoImpl = function ( database ) {
  return function (onError, onSuccess) {
    database.info( function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) ); // invoke the error callback in case of an error
        }
        else
        {
          onSuccess(response); // invoke the success callback with the reponse
        }
      });

    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};

exports.addDocumentImpl = function ( database, doc ) {
  return function (onError, onSuccess) {
    database.put(doc, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) ); // invoke the error callback in case of an error
        }
        else
        {
          onSuccess(response); // invoke the success callback with the reponse
        }
      });

    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};

exports.deleteDocumentImpl = function ( database, docName, rev ) {
  return function (onError, onSuccess) {
    database.remove(docName, rev, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) ); // invoke the error callback in case of an error
        }
        else
        {
          onSuccess(response); // invoke the success callback with the reponse
        }
      });

    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};

exports.getDocumentImpl = function ( database, docName ) {
  return function (onError, onSuccess) {
    database.get( docName, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) ); // invoke the error callback in case of an error
        }
        else
        {
          onSuccess(response); // invoke the success callback with the reponse
        }
      });

    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};

// db.putAttachment(docId, attachmentId, [rev], attachment, type, [callback]);
exports.addAttachmentImpl = function ( database, docName, attachmentId, mrev, attachment, type) {
  return function (onError, onSuccess) {
    database.putAttachment(docName, attachmentId, mrev, btoa(attachment), type, function(err, response)
      {
        if (err != null)
        {
          onError( convertPouchError(err) ); // invoke the error callback in case of an error
        }
        else
        {
          onSuccess(response); // invoke the success callback with the reponse
        }
      });

    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
};

exports.getAttachmentImpl = function( database, docName, attachmentId )
{
  return function (onError, onSucces)
  {
    database.getAttachment(docName, attachmentId, function( err, blob )
      {
        if (err != null)
        {
          onError( convertPouchError( err ))
        }
        else
        {
          blob.text().then( function(t)
          {
            onSucces(t);
          });
        }
      });
    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
}

exports.getViewOnDatabaseImpl = function( database, viewname, key )
{
  return function (onError, onSucces)
  {
    if (key)
    {
      database.query( viewname,
        { key: key },
        function (err, result)
        {
          if (err != null)
          {
            onError( convertPouchError( err ))
          }
          else
          {
            onSucces( result );
          }
        });
    }
    else
    {
      database.query( viewname,
        function (err, result)
        {
          if (err != null)
          {
            onError( convertPouchError( err ))
          }
          else
          {
            onSucces( result );
          }
        });
    }
    // Return a canceler, which is just another Aff effect.
    return function (cancelError, cancelerError, cancelerSuccess) {
      // No way to cancel the request.
      cancelerSuccess(); // invoke the success callback for the canceler
    };
  };
}
