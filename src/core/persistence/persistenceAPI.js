// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

// NOTE. We have to decide in coding time whether to include the browser or the Node version
// Otherwise, we'd carry the Node code to the browser and vice versa.
var PouchDB = require('pouchdb-browser').default;
// var PouchDB = require('pouchdb');

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

function convertPouchError( originalE )
{
  // Pouchdb returns an object in case of promise rejection.
  // But sometimes it returns a TypeError. We convert that to the same form.
  // We then stringify the objects and make a Javascript Error out of it.
  // By throwing a Javascript error, we make toAff catch it
  // and coerce it to an error in MonadError.
  // This is caught by catchError in Purescript, e.g. in addDocument.
  // It even sometimes (e.g. in addAttachment) returns a Response type object.

  if ( originalE instanceof TypeError )
  {
    return new Error( JSON.stringify(
      { status: originalE.status
      , name: originalE.name
      , message: originalE.message
      , error: originalE.stack})
    );
  }
  else try
  {
    // Apparently, 'Response' is unknown in the Node distribution and then this fails.
    if (originalE instanceof Response)
    {
      return new Error( JSON.stringify(
        { status: originalE.status
        , name: "response"
        , message: originalE.statusText
        , error: true
        , docId: originalE.url
        }
      ));
    }
    else
    {
      return new Error( JSON.stringify(
        { status: originalE.status
        , name: originalE.constructor.name
        , message: originalE.message
        , error: originalE.error}));
    }
  }
  catch(ignore)
  {
    return new Error( JSON.stringify(
      { status: originalE.status
      , name: originalE.constructor.name
      , message: originalE.message
      , error: originalE.error}));
  }
}

// On the assumption that we never persist files to the Inplace (or MyContexts) domains,
// every request will be a cors request.
function captureFetch(url, opts)
{
  opts.mode = "cors";
  return fetch(url, opts);
}

exports.createDatabaseImpl = function( databaseName )
{
  return new PouchDB( 
    databaseName
    // NOTE. We have to decide in coding time whether to include the browser or the Node version.
    // Node doesn't know fetch.
    // Outcomment when running Node!
    , {fetch: captureFetch} 
    );
}

exports.createRemoteDatabaseImpl = function( databaseName, couchdbUrl )
{
  var P = PouchDB.defaults(
    { prefix: couchdbUrl
    // NOTE. We have to decide in coding time whether to include the browser or the Node version.
    // Node doesn't know fetch.
    // Outcomment when running Node!
    , fetch: captureFetch 
    });
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

exports.documentsInDatabaseImpl = function( database, options ) {
  return function (onError, onSuccess) {
    database.allDocs( options, function(err, response)
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

var b2a;
try
{
  // Browser environment.
  b2a = btoa;
}
catch (e)
{
  // Node environment.
  b2a = function (b)
    {
      return Buffer.from(b).toString('base64');
    };
}

// db.putAttachment(docId, attachmentId, [rev], attachment, type, [callback]);
exports.addAttachmentImpl = function ( database, docName, attachmentId, mrev, attachment, type) {
  return function (onError, onSuccess) {
    database.putAttachment(docName, attachmentId, mrev, b2a(attachment), type, function(err, response)
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
          // blob is a Buffer on node. Then use toString().
          if (blob.text)
          {
            blob.text().then( function(t)
            {
              onSucces(t);
            });
          }
          else
          {
            onSucces( blob.toString() );
          }
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
