-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE


module Perspectives.Persistence.CouchdbFunctions

( setSecurityDocument
, replicateContinuously
, endReplication
, createUser
, deleteUser
, ensureSecurityDocument
, setPassword
, createDatabase
, deleteDatabase
)

where

import Prelude

import Affjax (ResponseFormatError, Response, printResponseFormatError)
import Affjax as AJ
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Argonaut (fromObject, fromString, fromArray, Json)
import Data.Array (find)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (insert, fromFoldable) as MAP
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (toLower)
import Data.String.Base64 (btoa)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error, error)
import Foreign (MultipleErrors)
import Foreign.Class (decode)
import Foreign.Generic (decodeJSON)
import Foreign.JSON (parseJSON)
import Foreign.Object (fromFoldable, singleton)
import Perspectives.Couchdb (CouchdbStatusCodes, ReplicationDocument(..), ReplicationEndpoint(..), SecurityDocument(..), SelectorObject, onAccepted, onAccepted')
import Perspectives.Persistence.Authentication (defaultPerspectRequest, ensureAuthentication)
import Perspectives.Persistence.State (getCouchdbPassword, getSystemIdentifier)
import Perspectives.Persistence.Types (DatabaseName, Url, MonadPouchdb)
import Simple.JSON (writeJSON)

-- | This module contains functions to write and read documents on Couchdb endpoints that Pouchdb prohibits:
-- | _security, _replicator.
-- | It therefore duplicates the following functions (from Perspectives.Couchdb.Databases):
-- |  - deleteDocument
-- |  - getDocumentFromUrl
-- |  - retrieveDocumentVersion
-- |  - version
-- | These functions are not exported.
-- | To complete a suite of functions to manipulate databases in Couchdb,
-- | we add createDatabase and addDatabase, too. This is mainly for use in model:Couchdb
-- | where we offer a functions to handle couchdb installations through Perspectives.

-----------------------------------------------------------
-- SET SECURITY DOCUMENT
-- See http://127.0.0.1:5984/_utils/docs/api/database/security.html#api-db-security
-----------------------------------------------------------
-- | Set the security document in the database.
-- | Authentication ensured.
setSecurityDocument :: forall f. Url -> DatabaseName -> SecurityDocument -> MonadPouchdb f Unit
setSecurityDocument base db doc = ensureAuthentication do
  rq <- defaultPerspectRequest
  -- Security documents have no versions.
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> db <> "/_security"), content = Just $ RequestBody.string (writeJSON $ unwrap doc)}
  liftAff $ onAccepted res.status [200, 201, 202] "setSecurityDocument" $ pure unit

-- | Returns a security document, even if none existed before.
-- | The latter is probably superfluous, as Couchdb returns a default design document anyway
-- | ("If the security object for a database has never been set, then the value returned will be empty." from the
-- | above reference). However, what is returned on a new database in an installation with a Server Admin, is:
-- | {"members":{"roles":["_admin"]},"admins":{"roles":["_admin"]}}
ensureSecurityDocument :: forall f. Url -> DatabaseName -> MonadPouchdb f SecurityDocument
ensureSecurityDocument base db = do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left GET, url = (base <> db <> "/_security")}
  if res.status == StatusCode 200
    then case res.body of
      Left e -> throwError $ error ("ensureSecurityDocument: error in response: " <> printResponseFormatError e)
      Right (result :: String) -> do
        (x :: Either MultipleErrors SecurityDocument) <- pure $ runExcept ((parseJSON >=> decode) result)
        case x of
          (Left e) -> do
            throwError $ error ("ensureSecurityDocument: error in decoding result: " <> show e)
          (Right securityDoc) -> pure securityDoc
    else do
      doc <- pure $ SecurityDocument
        { _id: db <> "Security"
        , admins: { names: [], roles: ["_admin"]}
        , members: { names: [], roles: ["_admin"]}
      }
      setSecurityDocument base db doc
      pure doc

-----------------------------------------------------------
-- REPLICATECONTINUOUSLY
-----------------------------------------------------------
-- | Authentication ensured.
replicateContinuously :: forall f. Url -> DatabaseName -> Url -> Url -> Maybe SelectorObject -> MonadPouchdb f Unit
replicateContinuously couchdbUrl name source target selector = do
  usr <- getSystemIdentifier
  pwd <- getCouchdbPassword
  bvalue <- pure (btoa (usr <> ":" <> pwd))
  case bvalue of
    Left e -> pure unit
    Right auth -> setReplicationDocument couchdbUrl (ReplicationDocument
        { _id: name
        , source: ReplicationEndpoint {url: source, headers: singleton "Authorization" ("Basic " <> auth)}
        , target: ReplicationEndpoint {url: target, headers: singleton "Authorization" ("Basic " <> auth)}
        , create_target: false
        , continuous: true
        -- , selector: maybe (Just emptySelector) Just selector
        , selector
        })

-- | Authentication ensured.
setReplicationDocument :: forall f. Url -> ReplicationDocument -> MonadPouchdb f Unit
setReplicationDocument base rd@(ReplicationDocument{_id}) = ensureAuthentication do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> "_replicator/" <> _id), content = Just $ RequestBody.string (writeJSON $ unwrap rd)}
  liftAff $ onAccepted res.status [200, 201, 202] "setReplicationDocument" $ pure unit

-----------------------------------------------------------
-- ENDREPLICATION
-----------------------------------------------------------
-- | Authentication ensured.
endReplication :: forall f. Url -> DatabaseName -> DatabaseName -> MonadPouchdb f Boolean
endReplication couchdbUrl source target = deleteDocument (couchdbUrl <> "_replicator/" <> source <> "_" <> target) Nothing

-----------------------------------------------------------
-- CREATE USER
-----------------------------------------------------------
-- NOTE. These functions may be redundant, as I believe we can manage documents
-- in the _users database through Pouchdb.
type User = String
type Password = String
-- | Create a non-admin user.
createUser :: forall f. Url -> User -> Password -> Array Role -> MonadPouchdb f Unit
createUser base user password roles = ensureAuthentication do
  rq <- defaultPerspectRequest
  (content :: Json) <- pure (fromObject (fromFoldable
    [ Tuple "name" (fromString user)
    , Tuple "password" (fromString password)
    , Tuple "roles" (fromArray (fromString <$> roles))
    , Tuple "type" (fromString "user")]))
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> "_users/org.couchdb.user:" <> user), content = Just $ RequestBody.json content}
  liftAff $ onAccepted res.status [200, 201] "createUser" $ pure unit

type Role = String

-----------------------------------------------------------
-- DELETE USER
-----------------------------------------------------------
deleteUser :: forall f. Url -> User -> MonadPouchdb f Boolean
deleteUser base user = deleteDocument (base <> "_users/org.couchdb.user:" <> user) Nothing

-----------------------------------------------------------
-- SETPASSWORD
-----------------------------------------------------------
setPassword :: forall f. Url -> User -> Password -> MonadPouchdb f Unit
setPassword base user password = ensureAuthentication do
  rq <- defaultPerspectRequest
  (res :: Response (Either ResponseFormatError Json)) <- liftAff $ AJ.request $ rq
    { method = Left GET
    , url = (base <> "_users/org.couchdb.user:" <> user)
    , responseFormat = ResponseFormat.json
    }
  if res.status == StatusCode 200
    then case res.body of
      Left e -> throwError $ error ("setPassword: error in response: " <> printResponseFormatError e)
      Right (result :: Json) -> do
        res1 <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> "_users/org.couchdb.user:" <> user), content = Just $ RequestBody.json (changePassword result password)}
        liftAff $ onAccepted res1.status [200, 201] "createUser" $ pure unit
    else throwError $ error ("setPassword: cannot retrieve user " <> user)

foreign import changePassword :: Json -> String -> Json

-----------------------------------------------------------
-- DELETE DOCUMENT (COPIED FROM Perspectives.Couchdb.Databases)
-----------------------------------------------------------
-- | Authentication ensured.
deleteDocument :: forall f. Url -> Maybe String -> MonadPouchdb f Boolean
deleteDocument url version' = ensureAuthentication do
  mrev <- case version' of
    Nothing -> retrieveDocumentVersion url
    Just v -> pure $ Just v
  case mrev of
    Nothing -> pure false
    Just rev -> do
      (rq :: (AJ.Request String)) <- defaultPerspectRequest
      res <- liftAff $ AJ.request $ rq {method = Left DELETE, url = (url <> "?rev=" <> rev) }
      -- pure $ isJust (elemIndex res.status [StatusCode 200, StatusCode 304])
      onAccepted res.status [200, 202] ("removeEntiteit(" <> url <> ")") (pure true)

-----------------------------------------------------------
-- DOCUMENT VERSION (COPIED FROM Perspectives.Couchdb.Databases)
-----------------------------------------------------------
retrieveDocumentVersion :: forall f. Url -> MonadPouchdb f (Maybe String)
retrieveDocumentVersion url = do
  (rq :: (AJ.Request String)) <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left HEAD, url = url}
  case res.status of
    StatusCode 200 -> version res.headers
    StatusCode 304 -> version res.headers
    otherwise -> pure Nothing

-----------------------------------------------------------
-- VERSION (COPIED FROM Perspectives.Couchdb.Databases)
-----------------------------------------------------------
type Revision_ = Maybe String
-- | Read the version from the headers.
version :: forall m. MonadError Error m => Array ResponseHeader -> m Revision_
version headers =  case find (\rh -> toLower (name rh) == "etag") headers of
  Nothing -> throwError $ error ("Perspectives.Instances.version: retrieveDocumentVersion: couchdb returns no ETag header holding a document version number")
  (Just h) -> case runExcept $ decodeJSON (value h) of
    Left e -> pure Nothing
    Right v -> pure $ Just v

-----------------------------------------------------------
-- CREATEDATABASE
-----------------------------------------------------------
-- Database names must comply to rules given in https://docs.couchdb.org/en/stable/api/database/common.html#db
createDatabase :: forall f. DatabaseName -> MonadPouchdb f Unit
createDatabase databaseUrl = ensureAuthentication do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = databaseUrl}
  -- (res :: AJ.AffjaxResponse String) <- liftAff $ AJ.put' (base <> dbname) (Nothing :: Maybe String)
  liftAff $ onAccepted' createStatusCodes res.status [201] "createDatabase" $ pure unit
  where
    createStatusCodes = MAP.insert 412 "Precondition failed. Database already exists."
      databaseStatusCodes

-----------------------------------------------------------
-- DELETEDATABASE
-----------------------------------------------------------
deleteDatabase :: forall f. Url -> MonadPouchdb f Unit
deleteDatabase databaseUrl = ensureAuthentication do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left DELETE, url = databaseUrl}
  -- liftAff $ AJ.put' (base <> dbname) (Nothing :: Maybe String)
  liftAff $ onAccepted' deleteStatusCodes res.status [200] "deleteDatabase" $ pure unit
  where
    deleteStatusCodes = MAP.insert 412 "Precondition failed. Database does not exist."
      databaseStatusCodes

databaseStatusCodes :: CouchdbStatusCodes
databaseStatusCodes = MAP.fromFoldable
  [ Tuple 400 "Bad AJ.Request. Invalid database name."
  , Tuple 401 "Unauthorized. CouchDB Server Administrator privileges required."]
