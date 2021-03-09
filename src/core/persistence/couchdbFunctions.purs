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
)

where

import Prelude

import Affjax as AJ
import Affjax.RequestBody as RequestBody
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.Base64 (btoa)
import Effect.Aff.Class (liftAff)
import Foreign.Object (singleton)
import Perspectives.Couchdb (ReplicationDocument(..), ReplicationEndpoint(..), SecurityDocument, SelectorObject, onAccepted)
import Perspectives.Couchdb.Databases (version)
import Perspectives.Persistence.Authentication (defaultPerspectRequest, ensureAuthentication)
import Perspectives.Persistence.State (getCouchdbPassword, getSystemIdentifier)
import Perspectives.Persistence.Types (DatabaseName, Url, MonadPouchdb)
import Simple.JSON (writeJSON)

-- | This module contains functions to write and read documents on Couchdb endpoints that Pouchdb prohibits:
-- | _security, _replicator.
-- | It therefore duplicates the following functions:
-- |  - deleteDocument
-- |  - retrieveDocumentVersion
-- | These functions are not exported.
-- |

-----------------------------------------------------------
-- SET SECURITY DOCUMENT
-- See http://127.0.0.1:5984/_utils/docs/api/database/security.html#api-db-security
-----------------------------------------------------------
-- | Set the security document in the database.
-- | Authentication ensured.
setSecurityDocument :: forall f. Url -> DatabaseName -> SecurityDocument -> MonadPouchdb f Unit
setSecurityDocument base db doc = ensureAuthentication do
  rq <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left PUT, url = (base <> db <> "/_security"), content = Just $ RequestBody.string (writeJSON $ unwrap doc)}
  liftAff $ onAccepted res.status [200, 201, 202] "setSecurityDocument" $ pure unit

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
-- DELETE DOCUMENT
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
-- DOCUMENT VERSION
-----------------------------------------------------------
retrieveDocumentVersion :: forall f. Url -> MonadPouchdb f (Maybe String)
retrieveDocumentVersion url = do
  (rq :: (AJ.Request String)) <- defaultPerspectRequest
  res <- liftAff $ AJ.request $ rq {method = Left HEAD, url = url}
  case res.status of
    StatusCode 200 -> version res.headers
    StatusCode 304 -> version res.headers
    otherwise -> pure Nothing
