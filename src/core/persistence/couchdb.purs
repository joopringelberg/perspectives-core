-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Couchdb where

import Affjax (Response)
import Affjax (printError, Error) as AJ
 
import Affjax.StatusCode (StatusCode)
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromObject, fromString, jsonSingletonObject)
import Data.Array (elemIndex)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map, fromFoldable, lookup, empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Foreign.Object (Object, fromFoldable, empty) as OBJ
import Perspectives.Couchdb.Revision (class Revision)
import Prelude (class Eq, class Show, show, ($), (<$>), (<<<), (<>), (==))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write, writeImpl)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- ALIASES
-----------------------------------------------------------
-- | URL terminated with a forward slash, e.g. http://www.perspectives.nl/.
type TerminatedURL = String
type User = String
type Password = String
type DatabaseName = String

-----------------------------------------------------------
-- ARGONAUT JSON FROM SIMPLE.JSON
-----------------------------------------------------------
-- Produce a Argonaut.Core Json value, such as is compatible with AffJax.RequestBody.json.
-- write (writeImpl) produces valid JSON, but it is typed as Foreign.
toJson :: forall a. WriteForeign a => a -> Json
toJson a = unsafeCoerce $ write a

-- Use at your own risk! Only when it is known that a actually has the shape of a bona fide JSON structure.
coerceToJson :: forall a. a -> Json
coerceToJson = unsafeCoerce

-----------------------------------------------------------
-- PUTCOUCHDBDOCUMENT
-----------------------------------------------------------

newtype PutCouchdbDocument = PutCouchdbDocument
  { ok :: Maybe Boolean
  , id :: Maybe String
  , rev :: Maybe String
  , error :: Maybe String
  , reason :: Maybe String}

derive instance genericPutCouchdbDocument :: Generic PutCouchdbDocument _

derive instance newtypePutCouchdbDocument :: Newtype PutCouchdbDocument _

instance revisionPutCouchdbDocument :: Revision PutCouchdbDocument where
  rev = _.rev <<< unwrap
  changeRevision _ d = d

-----------------------------------------------------------
-- DELETECOUCHDBDOCUMENT
-----------------------------------------------------------
newtype DeleteCouchdbDocument = DeleteCouchdbDocument
  { ok :: Maybe Boolean
  , id :: Maybe String
  , rev :: Maybe String}

derive instance genericDeleteCouchdbDocument :: Generic DeleteCouchdbDocument _

derive instance newtypeDeleteCouchdbDocument :: Newtype DeleteCouchdbDocument _

derive newtype instance ReadForeign DeleteCouchdbDocument

instance revisionDeleteCouchdbDocument :: Revision DeleteCouchdbDocument where
  rev = _.rev <<< unwrap
  changeRevision _ d = d

-----------------------------------------------------------
-- REPLICATIONDOCUMENT
-----------------------------------------------------------
-- {
--     "_id": "my_rep",
--     "source": "http://myserver.com/foo",
--     "target":  "http://user:pass@localhost:5984/bar",
--     "create_target":  true,
--     "continuous": true
-- }
newtype ReplicationDocument = ReplicationDocument
  { _id :: String
  , _rev :: Maybe String
  , source :: ReplicationEndpoint
  , target :: ReplicationEndpoint
  , create_target :: Boolean
  , continuous :: Boolean
  , selector :: Maybe SelectorObject
  }

derive instance genericReplicationDocument :: Generic ReplicationDocument _

derive instance newtypeReplicationDocument :: Newtype ReplicationDocument _

instance showReplicationDocument :: Show ReplicationDocument where
  show = genericShow

-- We use the EncodeJson instance in setReplicationDocument.
instance encodeJsonReplicationDocument :: EncodeJson ReplicationDocument where
    encodeJson (ReplicationDocument ddr) = unsafeCoerce $ write ddr

instance revisionReplicationDocument :: Revision ReplicationDocument where
  rev _ = Nothing
  changeRevision _ d = d

-----------------------------------------------------------
-- REPLICATIONENDPOINT
-----------------------------------------------------------
newtype ReplicationEndpoint = ReplicationEndpoint
  { url :: String
  , headers :: OBJ.Object String
  }

derive instance genericReplicationEndpoint :: Generic ReplicationEndpoint _

instance showReplicationEndpoint :: Show ReplicationEndpoint where
  show = genericShow

-- We use the EncodeJson instance in setReplicationDocument.
instance encodeJsonReplicationEndpoint :: EncodeJson ReplicationEndpoint where
    encodeJson (ReplicationEndpoint ddr) = unsafeCoerce $ write ddr

instance writeForeignReplicationEndpoint :: WriteForeign ReplicationEndpoint where
  writeImpl (ReplicationEndpoint r) = write r
-----------------------------------------------------------
-- ATTACHMENTS
-----------------------------------------------------------
  -- {
  --     "content_type": "text/plain",
  --     "digest": "md5-Ids41vtv725jyrN7iUvMcQ==",
  --     "length": 1872,
  --     "revpos": 4,
  --     "stub": true
  -- }
type AttachmentRecord =
  { content_type :: String
  , digest :: String
  , length :: Int
  , revpos :: Int
  , stub :: Boolean
  }

type AttachmentInfo = OBJ.Object AttachmentRecord

newtype DocWithAttachmentInfo = DocWithAttachmentInfo DocWithAttachmentInfoR

type DocWithAttachmentInfoR = {_attachments :: Maybe AttachmentInfo}

instance showDocWithAttachmentInfo :: Show DocWithAttachmentInfo where
  show (DocWithAttachmentInfo r) = show r

instance revisionDocWithAttachmentInfo :: Revision DocWithAttachmentInfo where
  rev _ = Nothing
  changeRevision _ d = d

derive newtype instance WriteForeign DocWithAttachmentInfo
derive newtype instance ReadForeign DocWithAttachmentInfo
-----------------------------------------------------------
-- SELECTOROBJECT
-----------------------------------------------------------
-- | We support simple SelectorObjects that specify one or more fields with a required value.
-- | Notice that we specify a subfield selector structure. This is because we anticipate
-- | the use of genericEncode to encode objects. This means that, instead of being a toplevel
-- | key, the key of interest is a key in the object with key "contents".
-- | See http://127.0.0.1:5984/_utils/docs/api/database/find.html#subfields for an explanation of subfields.
-- | See http://127.0.0.1:5984/_utils/docs/replication/replicator.html#selectorobj and
-- | see http://127.0.0.1:5984/_utils/docs/api/database/find.html#find-selectors
type SelectorObject = OBJ.Object (OBJ.Object (OBJ.Object String))

-- | The value selected by the key must be equal to the given value.
-- | This produces the following selector:
-- |      {
-- |			"contents": {
-- |			  "<key>": {"$eq": "<value>"}
-- |			}
selectOnFieldEqual :: String -> String -> SelectorObject
selectOnFieldEqual key value = OBJ.fromFoldable [Tuple "contents" (OBJ.fromFoldable [Tuple key (OBJ.fromFoldable [Tuple "$eq" value])])]

-- | The value selected by the key must be different from the given value.
-- | This produces the following selector:
-- |      {
-- |			"contents": {
-- |			  "<key>": {"$ne": "<value>"}
-- |			}
selectOnFieldNotEqual :: String -> String -> SelectorObject
selectOnFieldNotEqual key value = OBJ.fromFoldable [Tuple "contents" (OBJ.fromFoldable [Tuple key (OBJ.fromFoldable [Tuple "$ne" value])])]

emptySelector :: SelectorObject
emptySelector = OBJ.fromFoldable [Tuple "contents" OBJ.empty]
-----------------------------------------------------------
-- DBS
-----------------------------------------------------------
newtype DBS = DBS (Array String)
derive instance genericDBS :: Generic DBS _
derive instance newtypeDBS :: Newtype DBS _

-----------------------------------------------------------
-- GETCOUCHDBALLDOCS
-----------------------------------------------------------
newtype GetCouchdbAllDocs = GetCouchdbAllDocs
  { offset :: Int
  , rows :: Array DocReference
  , total_rows :: Int
  , update_seq :: Maybe Int
  }

derive instance genericCouchdbAllDocs :: Generic GetCouchdbAllDocs _
derive instance newtypeCouchdbAllDocs :: Newtype GetCouchdbAllDocs _

instance revisionGetCouchdbAllDocs :: Revision GetCouchdbAllDocs where
  rev _ = Nothing
  changeRevision _ d = d

newtype DocReference = DocReference { id :: String, value :: Rev}

derive instance genericDocReference :: Generic DocReference _

newtype Rev = Rev { rev :: String}

derive instance genericRef :: Generic Rev _

-----------------------------------------------------------
-- AUTHENTICATION
-----------------------------------------------------------
newtype PostCouchdb_session = PostCouchdb_session
  { ok :: Boolean
  , name :: Maybe User
  , roles :: Array String
  }

derive instance genericPostCouchdb_session :: Generic PostCouchdb_session _
derive instance newtypePostCouchdb_session :: Newtype PostCouchdb_session _

-----------------------------------------------------------
-- DESIGN DOCUMENT
-----------------------------------------------------------
-- {
--     "_id": "_design/application",
--     "_rev": "1-C1687D17",
--     "views": {
--         "viewname": {
--             "map": "function(doc) { ... }",
--             "reduce": "function(keys, values) { ... }"
--         }
--     }
-- }
newtype DesignDocument = DesignDocument DesignDocumentRecord

type DesignDocumentRecord =
  { _id :: String
  , _rev :: Maybe String
  , views :: OBJ.Object View
}

derive instance genericDesignDocument :: Generic DesignDocument _
derive instance newtypeDesignDocument :: Newtype DesignDocument _
derive newtype instance WriteForeign DesignDocument
derive newtype instance ReadForeign DesignDocument

instance showDesignDocument :: Show DesignDocument where
  show = genericShow
instance encodeJonDesignDocument :: EncodeJson DesignDocument where
  -- encodeJson (DesignDocument ddr) = encodeJson ddr
  encodeJson (DesignDocument{_id, _rev, views})= case _rev of
    Nothing -> fromObject $ OBJ.fromFoldable
      [ Tuple "_id" (fromString _id)
      , Tuple "views" (fromObject (encodeJson <$> views))
      ]
    Just r -> fromObject $ OBJ.fromFoldable
      [ Tuple "_id" (fromString _id)
      , Tuple "_rev" (fromString r)
      , Tuple "views" (fromObject (encodeJson <$> views))
      ]

instance revisionDesignDocument :: Revision DesignDocument where
  rev = _._rev <<< unwrap
  changeRevision _ d = d

designDocumentViews :: DesignDocument -> OBJ.Object View
designDocumentViews = _.views <<< unwrap

-----------------------------------------------------------
-- DESIGN DOCUMENT
-----------------------------------------------------------
newtype View = View
  { map :: String
  , reduce :: Maybe String
}

derive instance genericView :: Generic View _
derive instance newtypeView :: Newtype View _
derive newtype instance WriteForeign View
derive newtype instance ReadForeign View

instance eqView :: Eq View where
  eq = genericEq
instance showView :: Show View where
  show = genericShow
instance encodeJonView :: EncodeJson View where
  encodeJson (View{map,reduce}) = case reduce of
    Nothing -> jsonSingletonObject "map" (fromString map)
    Just r -> fromObject $ OBJ.fromFoldable
      [ Tuple "map" (fromString map)
      , Tuple "reduce" (fromString r)
      ]

-----------------------------------------------------------
-- VIEWRESULT
-----------------------------------------------------------
-- {
--     "offset": 0,
--     "rows": [
--         {
--             "id": "SpaghettiWithMeatballs",
--             "key": "meatballs",
--             "value": 1
--         },
--         {
--             "id": "SpaghettiWithMeatballs",
--             "key": "spaghetti",
--             "value": 1
--         },
--         {
--             "id": "SpaghettiWithMeatballs",
--             "key": "tomato sauce",
--             "value": 1
--         }
--     ],
--     "total_rows": 3
-- }

newtype ViewDocResult f k = ViewDocResult
  { offset :: Int
  , rows :: Array (ViewDocResultRow f k)
  , total_rows :: Int
  }
derive newtype instance (ReadForeign k, ReadForeign f) => ReadForeign (ViewDocResult f k)

newtype ViewDocResultRow f k = ViewDocResultRow { id :: String, key :: k, doc :: f }
derive newtype instance (ReadForeign k, ReadForeign f) => ReadForeign (ViewDocResultRow f k)


newtype ViewResult f k = ViewResult
  { offset :: Int
  , rows :: Array (ViewResultRow f k)
  , total_rows :: Int
  }

instance revisionViewResult :: Revision (ViewResult f k) where
  rev _ = Nothing
  changeRevision _ d = d

newtype ViewResultRow f k = ViewResultRow { id :: String, key :: k, value :: f }
derive newtype instance (ReadForeign k, ReadForeign f) => ReadForeign (ViewResultRow f k)

derive instance genericViewResult :: Generic (ViewResult f k) _
derive instance newtypeViewResult :: Newtype (ViewResult f k) _
derive newtype instance (ReadForeign k, ReadForeign f) => ReadForeign (ViewResult f k)

derive instance genericViewResultRow :: Generic (ViewResultRow f k) _
derive instance newtypeViewResultRow :: Newtype (ViewResultRow f k) _
  -- Decodeer het eerste niveau. Pas decode to op f: dan wordt de revisie gezet.
  -- decode = readString >=> parseJSON >=> decode

-----------------------------------------------------------
-- SECURITY DOCUMENT
-----------------------------------------------------------
-- {
--     "admins": {
--         "names": [
--             "Bob"
--         ],
--         "roles": []
--     },
--     "members": {
--         "names": [
--             "Mike",
--             "Alice"
--         ],
--         "roles": []
--     }
-- }
newtype SecurityDocument = SecurityDocument
  { admins :: { names :: Maybe (Array String), roles :: Array String}
  , members :: { names :: Maybe (Array String), roles :: Array String}
}

derive instance genericSecurityDocument :: Generic SecurityDocument _
derive instance newtypeSecurityDocument :: Newtype SecurityDocument _

instance readForeignSecurityDocument :: ReadForeign SecurityDocument where
  readImpl sd = SecurityDocument <$> readImpl sd

instance writeForeignSecurityDocument :: WriteForeign SecurityDocument where
  writeImpl (SecurityDocument sd) = writeImpl sd

instance showSecurityDocument :: Show SecurityDocument where
  show = genericShow

instance encodeJonSecurityDocument :: EncodeJson SecurityDocument where
  encodeJson (SecurityDocument r) = encodeJson r

instance decodeJonSecurityDocument :: DecodeJson SecurityDocument where
  decodeJson j = SecurityDocument <$> decodeJson j

-----------------------------------------------------------
-- STATUS CODES
-----------------------------------------------------------

type CouchdbStatusCodes = Map Int String

couchdDBStatusCodes :: CouchdbStatusCodes
couchdDBStatusCodes = fromFoldable
  [ Tuple 200 "OK. Request completed successfully"
  , Tuple 201 "Created. Document created successfully."
  , Tuple 202 "Accepted. Request has been accepted, but the corresponding operation may not have completed. This is used for background operations, such as database compaction."
  , Tuple 304 "Not Modified. The additional content requested has not been modified. This is used with the ETag system to identify the version of information returned."
  , Tuple 400 "Bad Request. Bad request structure. The error can indicate an error with the request URL, path or headers. Differences in the supplied MD5 hash and content also trigger this error, as this may indicate message corruption."
  , Tuple 401 "Unauthorized. The item requested was not available using the supplied authorization, or authorization was not supplied."
  , Tuple 403 "Forbidden. The requested item or operation is forbidden."
  , Tuple 404 "Not found. The requested content could not be found. The content will include further information, as a JSON object, if available. The structure will contain two keys, error and reason."
  , Tuple 405 "Resource not allowed. A request was made using an invalid HTTP request type for the URL requested. For example, you have requested a PUT when a POST is required. Errors of this type can also triggered by invalid URL strings."
  , Tuple 406 "Not acceptable. The requested content type is not supported by the server."
  , Tuple 409 "Conflict. Request resulted in an update conflict."
  , Tuple 412 "Precondition failed. The request headers from the client and the capabilities of the server do not match."
  , Tuple 415 "Bad Content type. The content types supported, and the content type of the information being requested or submitted indicate that the content type is not supported."
  , Tuple 416 "Requested Range not satisfiable. The range specified in the request header cannot be satisfied by the server."
  , Tuple 417 "Expectation failed. When sending documents in bulk, the bulk load operation failed."
  , Tuple 500 "Internal server error. The request was invalid, either because the supplied JSON was invalid, or invalid information was supplied as part of the request."
  ]

type CouchdbFunctionName = String

-- | Throws an error pertinent to the statuscode if it is not one of the statusCodes that are acceptable.
-- | Otherwise evaluates f.
onAccepted :: forall a m f. MonadError Error m =>
  (Either AJ.Error (Response a)) ->
  Array StatusCode ->
  CouchdbFunctionName ->
  (Response a -> m f) ->
  m f
onAccepted = onAccepted_ handleCouchdbError

-- | As `onAccepted` but with specialised error messages (overriding existing StatusCodes with a custom message).
onAccepted' :: forall a m f. MonadError Error m =>
  CouchdbStatusCodes ->
  (Either AJ.Error (Response a)) ->
  Array StatusCode ->
  CouchdbFunctionName ->
  (Response a -> m f) ->
  m f
onAccepted' _ (Left e) _ fname _ = throwError $ error (fname <> ": error in call: " <> AJ.printError e)
onAccepted' overriddenCodes (Right response) statusCodes fname onExpectedStatus = case elemIndex response.status statusCodes of
  Nothing -> do
    handleError (unwrap response.status) overriddenCodes fname
  _ -> onExpectedStatus response

onAccepted_ :: forall a m f. MonadError Error m =>
  (Response a -> CouchdbFunctionName -> m f) ->
  (Either AJ.Error (Response a)) ->
  Array StatusCode ->
  CouchdbFunctionName ->
  (Response a -> m f) ->
  m f
onAccepted_ _ (Left e) _ fname _ = throwError $ error (fname <> ": error in call: " <> AJ.printError e)
onAccepted_ onOtherStatus (Right response) expectedStatusCodes fname onExpectedStatus = case elemIndex response.status expectedStatusCodes of
  Nothing -> onOtherStatus response fname
  _ -> onExpectedStatus response

handleCouchdbError :: forall a m f. MonadError Error m =>
  Response a ->
  CouchdbFunctionName ->
  m f
handleCouchdbError response fname = handleError (unwrap response.status) empty fname

-- | Always throws an error.
handleError :: forall a m. MonadError Error m => Int -> CouchdbStatusCodes -> String -> m a
handleError n statusCodes fname =
  if n == 401
    then throwError $ error "UNAUTHORIZED"
    else
      case lookup n statusCodes of
        (Just m) -> throwError $ error $ "Failure in " <> fname <> ". " <> m
        Nothing ->
          case lookup n couchdDBStatusCodes of
            (Just m) -> throwError $ error $  "Failure in " <> fname <> ". " <> m
            Nothing -> throwError $ error $ "Failure in " <> fname <> ". " <> "Unknown HTTP statuscode " <> show n

escapeCouchdbDocumentName :: String -> String
escapeCouchdbDocumentName s = replaceAll (Pattern ":") (Replacement "%3A") (replaceAll (Pattern "$") (Replacement "%24") s)
