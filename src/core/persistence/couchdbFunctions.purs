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
  ( addRoleToUser
  , concatenatePathSegments
  , createUser
  , deleteUser
  , endReplication
  , ensureSecurityDocument
  , removeRoleFromUser
  , replicateContinuously
  , setPassword
  , setSecurityDocument
  , user2couchdbuser
  )
  where

import Prelude

import Affjax (Response)
import Affjax.Web as AJ
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut (Json)
import Data.Array (difference, find, union)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Map (insert, fromFoldable) as MAP
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (toLower)
import Data.String.Base64 (btoa)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Exception (Error, error)
import Foreign (MultipleErrors)
import Foreign.Object (singleton)
import Perspectives.Couchdb (CouchdbStatusCodes, ReplicationDocument(..), ReplicationEndpoint(..), SecurityDocument(..), SelectorObject, coerceToJson, onAccepted, onAccepted', onAccepted_, toJson)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Identifiers (endsWithSegments)
import Perspectives.Persistence.Authentication (AuthoritySource(..), defaultPerspectRequest, ensureAuthentication, requestAuthentication)
import Perspectives.Persistence.State (getCouchdbCredentials)
import Perspectives.Persistence.Types (Credential(..), DatabaseName, MonadPouchdb, Url)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON)
import Unsafe.Coerce (unsafeCoerce)

-- | This module contains functions to write and read documents on Couchdb endpoints that Pouchdb prohibits:
-- | _security, _replicator.
-- | It therefore duplicates the following functions (from Perspectives.Couchdb.Databases):
-- |  - deleteDocument
-- |  - getDocumentFromUrl
-- |  - retrieveDocumentVersion
-- |  - version
-- | These functions are not exported.
-- | To complete a suite of functions to manipulate databases in Couchdb,
-- | we add createDatabase and addDatabase, too. This is mainly for use in model://perspectives.domains#Couchdb
-- | where we offer a functions to handle couchdb installations through Perspectives.

-----------------------------------------------------------
-- SET SECURITY DOCUMENT
-- See http://127.0.0.1:5984/_utils/docs/api/database/security.html#api-db-security
-----------------------------------------------------------
-- | Set the security document in the database.
-- | Authentication ensured.
setSecurityDocument :: forall f. Url -> DatabaseName -> SecurityDocument -> MonadPouchdb f Unit
setSecurityDocument base db doc = do 
  -- Couchdb does not return recognizable "you are not authorized" information. Hence we authenticate anyway.
  requestAuthentication (Authority base)
  rq <- defaultPerspectRequest
  -- Security documents have no versions.
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> db <> "/_security"), content = Just $ RequestBody.json (toJson $ unwrap doc)}
  liftAff $ onAccepted res [StatusCode 200, StatusCode 201, StatusCode 202] "setSecurityDocument" (\_ -> pure unit)

-- | Returns a security document, even if none existed before.
-- | The latter is probably superfluous, as Couchdb returns a default design document anyway
-- | ("If the security object for a database has never been set, then the value returned will be empty." from the
-- | above reference). However, what is returned on a new database in an installation with a Server Admin, is:
-- | {"members":{"roles":["_admin"]},"admins":{"roles":["_admin"]}}
ensureSecurityDocument :: forall f. Url -> DatabaseName -> MonadPouchdb f SecurityDocument
ensureSecurityDocument base db = do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left GET, url = (base <> db <> "/_security")}
  onAccepted_
    (\_ _ -> do
      doc <- pure $ SecurityDocument
        { admins: { names: Just [], roles: ["_admin"]}
        , members: { names: Just [], roles: ["_admin"]}
      }
      setSecurityDocument base db doc
      pure doc)
    res
    [StatusCode 200]
    "ensureSecurityDocument"
    \response -> do
      (x :: Either MultipleErrors SecurityDocument) <- pure $ readJSON response.body
      case x of
        (Left e) -> do
          throwError $ error ("ensureSecurityDocument: error in decoding result: " <> show e)
        (Right securityDoc) -> pure securityDoc

-----------------------------------------------------------
-- REPLICATECONTINUOUSLY
-----------------------------------------------------------
-- | Authentication ensured.
replicateContinuously :: forall f. PerspectivesUser -> Url -> DatabaseName -> Url -> Url -> Maybe SelectorObject -> MonadPouchdb f Unit
replicateContinuously (PerspectivesUser usr) couchdbUrl name source target selector = do
  mpwd <- getCouchdbCredentials
  case mpwd of 
    Nothing -> throwError (error $ "replicateContinuously: no password found for user " <> usr <> " in " <> couchdbUrl)
    Just (Credential username pwd) -> do 
      bvalue <- pure (btoa (usr <> ":" <> pwd))
      case bvalue of
        Left _ -> pure unit
        Right auth -> setReplicationDocument couchdbUrl (ReplicationDocument
            { _id: name
            , _rev: Nothing
            , source: ReplicationEndpoint {url: source, headers: singleton "Authorization" ("Basic " <> auth)}
            , target: ReplicationEndpoint {url: target, headers: singleton "Authorization" ("Basic " <> auth)}
            , create_target: false
            , continuous: true
            -- , selector: maybe (Just emptySelector) Just selector
            , selector
            })

-- | Authentication ensured.
setReplicationDocument :: forall f. Url -> ReplicationDocument -> MonadPouchdb f Unit
setReplicationDocument base (ReplicationDocument rd@{_id}) = ensureAuthentication (Authority base) \_ -> do
  rq <- defaultPerspectRequest
  rev <- retrieveDocumentVersion (base <> "_replicator/" <> _id)
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> "_replicator/" <> _id), content = Just $ RequestBody.json (toJson (rd {_rev = rev}))}
  onAccepted res [StatusCode 200, StatusCode 201, StatusCode 202] "setReplicationDocument" (\_ -> pure unit)

-----------------------------------------------------------
-- ENDREPLICATION
-----------------------------------------------------------
-- | End replication by throwing away the replication document.
-- | Authentication ensured.
endReplication :: forall f. Url -> DatabaseName -> DatabaseName -> MonadPouchdb f Boolean
endReplication couchdbUrl source target = deleteDocument (couchdbUrl <> "_replicator/" <> source <> "_" <> target) Nothing

-----------------------------------------------------------
-- USERBDOCUMENT
-----------------------------------------------------------

newtype UserDocument = UserDocument
  { _id :: String
  , _rev :: Maybe String
  , name :: String
  , password_scheme :: String
  , password :: Maybe String 
  , iterations :: Int
  , derived_key :: String
  , salt :: String
  , roles :: Array String
  , type :: String
  }

derive instance Generic UserDocument _

derive instance Newtype UserDocument _

instance Revision UserDocument where
  rev = _._rev <<< unwrap
  changeRevision _ d = d

derive newtype instance ReadForeign UserDocument
derive newtype instance WriteForeign UserDocument
-----------------------------------------------------------
-- CREATE USER
-----------------------------------------------------------
-- NOTE. These functions may be redundant, as I believe we can manage documents
-- in the _users database through Pouchdb.
type User = String
type Password = String
-- | Create a non-admin user.
createUser :: forall f. Url -> User -> Password -> Array Role -> MonadPouchdb f Unit
createUser base user password roles = ensureAuthentication (Authority base) \_ -> do
  rq <- defaultPerspectRequest
  (content :: Json) <- pure (toJson
    { _id: "org.couchdb.user:" <> user2couchdbuser user
    , name: user2couchdbuser user
    , password
    , roles
    , type: "user"
    })
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> "_users/org.couchdb.user:" <> user2couchdbuser user ), content = Just $ RequestBody.json content}
  onAccepted res [StatusCode 200, StatusCode 201] "createUser" (\_ -> pure unit)

type Role = String

-----------------------------------------------------------
-- GET USER
-----------------------------------------------------------
getUserDocument :: forall f. Url -> User -> MonadPouchdb f UserDocument
getUserDocument base user = getDocumentFromUrl (base <> "_users/org.couchdb.user:" <> user2couchdbuser user)

-----------------------------------------------------------
-- ADD OR REMOVE ROLE
-----------------------------------------------------------
-- | Add a role to a user document.
addRoleToUser :: forall f. Url -> User -> Role -> MonadPouchdb f Unit
addRoleToUser base user role = do
  rq <- defaultPerspectRequest
  -- Request the user document
  UserDocument urecord@{roles} <- getUserDocument base user
  res <- liftAff $ AJ.request $ rq 
    { method = Left PUT
    , url = (base <> "_users/org.couchdb.user:" <> user2couchdbuser user )
    , content = Just $ RequestBody.json $ toJson (urecord {roles = union [role] roles})
    }
  onAccepted res [StatusCode 200, StatusCode 201] "createUser" (\_ -> pure unit)

removeRoleFromUser :: forall f. Url -> User -> Role -> MonadPouchdb f Unit
removeRoleFromUser base user role = do
  rq <- defaultPerspectRequest
  -- Request the user document
  UserDocument urecord@{roles} <- getUserDocument base user
  res <- liftAff $ AJ.request $ rq 
    { method = Left PUT
    , url = (base <> "_users/org.couchdb.user:" <> user2couchdbuser user )
    , content = Just $ RequestBody.json $ toJson (urecord {roles = difference roles [role]})
    }
  onAccepted res [StatusCode 200, StatusCode 201] "createUser" (\_ -> pure unit)

-----------------------------------------------------------
-- DELETE USER
-----------------------------------------------------------
deleteUser :: forall f. Url -> User -> MonadPouchdb f Boolean
deleteUser base user = deleteDocument (base <> "_users/org.couchdb.user:" <> user2couchdbuser user) Nothing

-----------------------------------------------------------
-- SETPASSWORD
-----------------------------------------------------------
setPassword :: forall f. Url -> User -> Password -> MonadPouchdb f Unit
setPassword base user password = ensureAuthentication (Authority base) \_ -> do
  rq <- defaultPerspectRequest
  (res :: (Either AJ.Error (Response Json))) <- liftAff $ AJ.request $ rq
    { method = Left GET
    , url = (base <> "_users/org.couchdb.user:" <> user2couchdbuser user)
    , responseFormat = ResponseFormat.json
    }
  onAccepted
    res
    [StatusCode 200]
    "setPassword"
    \response -> do
        res1 <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> "_users/org.couchdb.user:" <> user2couchdbuser user), content = Just $ RequestBody.json (changePassword response.body password)}
        onAccepted res1 [StatusCode 200, StatusCode 201] "createUser" \_ -> pure unit

changePassword :: Json -> Password -> Json
changePassword r password = coerceToJson ((unsafeCoerce r) {password = password})

-----------------------------------------------------------
-- GET DOCUMENT FROM URL (COPIED FROM Perspectives.Couchdb.Databases)
-----------------------------------------------------------
getDocumentFromUrl :: forall d f. Revision d => ReadForeign d => String -> MonadPouchdb f d
getDocumentFromUrl url = ensureAuthentication (Url url) \_ -> do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {url = url}
  onAccepted
    res
    [StatusCode 200]
    "getDocumentFromUrl"
    \response -> do
      (x :: Either MultipleErrors d) <- pure $ readJSON response.body
      case x of
        (Left e) -> throwError $ error ("getDocumentFromUrl: error in decoding result: " <> show e)
        (Right doc) -> pure doc

-----------------------------------------------------------
-- DELETE DOCUMENT (COPIED FROM Perspectives.Couchdb.Databases)
-----------------------------------------------------------
-- | Authentication ensured.
deleteDocument :: forall f. Url -> Maybe String -> MonadPouchdb f Boolean
deleteDocument url version' = ensureAuthentication (Url url) \_ -> do
  mrev <- case version' of
    Nothing -> retrieveDocumentVersion url
    Just v -> pure $ Just v
  case mrev of
    Nothing -> pure false
    Just rev -> do
      (rq :: (AJ.Request String)) <- defaultPerspectRequest
      res <- liftAff $ AJ.request $ rq {method = Left DELETE, url = (url <> "?rev=" <> rev) }
      onAccepted_
        (\_ _ -> pure false)
        res
        [StatusCode 200, StatusCode 202]
        ("removeEntiteit(" <> url <> ")")
        (\_ -> pure true)

-----------------------------------------------------------
-- DOCUMENT VERSION (COPIED FROM Perspectives.Couchdb.Databases)
-----------------------------------------------------------
retrieveDocumentVersion :: forall f. Url -> MonadPouchdb f (Maybe String)
retrieveDocumentVersion url = do
  (rq :: (AJ.Request String)) <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left HEAD, url = url}
  onAccepted_
    (\_ _ -> pure Nothing)
    res
    [StatusCode 200, StatusCode 203]
    "retrieveDocumentVersion"
    (\response -> version response.headers)

-----------------------------------------------------------
-- VERSION (COPIED FROM Perspectives.Couchdb.Databases)
-----------------------------------------------------------
type Revision_ = Maybe String
-- | Read the version from the headers.
version :: forall m. MonadError Error m => Array ResponseHeader -> m Revision_
version headers =  case find (\rh -> toLower (name rh) == "etag") headers of
  Nothing -> throwError $ error ("Perspectives.Instances.version: retrieveDocumentVersion: couchdb returns no ETag header holding a document version number")
  (Just h) -> case readJSON (value h) of
    Left _ -> pure Nothing
    Right v -> pure $ Just v

-----------------------------------------------------------
-- CREATEDATABASE
-----------------------------------------------------------
-- Database names must comply to rules given in https://docs.couchdb.org/en/stable/api/database/common.html#db
createDatabase :: forall f. DatabaseName -> MonadPouchdb f Unit
createDatabase databaseUrl = ensureAuthentication (Url databaseUrl) \_ -> do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = databaseUrl}
  onAccepted' createStatusCodes res [StatusCode 201] "createDatabase" (\_ -> pure unit)
  where
    createStatusCodes = MAP.insert 412 "Precondition failed. Database already exists."
      databaseStatusCodes

-----------------------------------------------------------
-- DOCUMENT, DATABASE EXISTS
-----------------------------------------------------------
documentExists :: forall f. Url -> MonadPouchdb f Boolean
documentExists url = ensureAuthentication (Url url) \_ -> do
  (rq :: (AJ.Request String)) <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left HEAD, url = url}
  onAccepted_
    (\response _ -> if response.status == StatusCode 401
      then throwError (error "unauthorized")
      else pure false)
    res
    [StatusCode 200, StatusCode 304]
    "documentExists"
    (\_ -> pure true)

databaseExists :: forall f. Url -> MonadPouchdb f Boolean
databaseExists = documentExists

-----------------------------------------------------------
-- DELETEDATABASE
-----------------------------------------------------------
deleteDatabase :: forall f. Url -> MonadPouchdb f Unit
deleteDatabase databaseUrl = ensureAuthentication (Url databaseUrl) \_ -> do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left DELETE, url = databaseUrl}
  onAccepted'
    deleteStatusCodes
    res
    [StatusCode 200]
    "deleteDatabase"
    (\_ -> pure unit)
  where
    deleteStatusCodes = MAP.insert 412 "Precondition failed. Database does not exist."
      databaseStatusCodes

databaseStatusCodes :: CouchdbStatusCodes
databaseStatusCodes = MAP.fromFoldable
  [ Tuple 400 "Bad AJ.Request. Invalid database name."
  , Tuple 401 "Unauthorized. CouchDB Server Administrator privileges required."]

-----------------------------------------------------------
-- user2couchdbuser
-----------------------------------------------------------
-- | Transforms a canonical user identification in Perspectives to an acceptable user name in Couchdb, by stripping away the storage scheme.
user2couchdbuser :: String -> String
user2couchdbuser = takeGuid

-----------------------------------------------------------
-- concatenatePathSegments
-----------------------------------------------------------
-- | Safely concatenate two segments of a path (ensuring both are separated by a forward slash.)
concatenatePathSegments :: String -> String -> String
concatenatePathSegments s1 s2 = if endsWithSegments s1 "/" 
  then s1 <> s2
  else s1 <> "/" <> s2