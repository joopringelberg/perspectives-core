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
module Perspectives.ResourceIdentifiers  where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty (index)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.Guid as GUID
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (MonadPouchdb)
import Perspectives.Representation.TypeIdentifiers (ResourceType)
import Perspectives.Sync.Transaction (StorageScheme(..), WriteUrl, Url, DbName) as TRANS
import Perspectives.Sync.Transaction (Transaction(..))

{------------------------------------------------------------
------------------------------------------------------------
RESOURCE IDENTIFIYING SCHEMES
This module is about identifiers for context- and role instances, collectively called _resources_.  It also covers DomeinFiles,
which are resources of another kind. 
This INCLUDES public resource identifiers: they are in the Remote scheme.
However, it is NOT about type identifiers.

These identifiers should be used for Persistent and Cacheable instances.

They govern where resources are stored. The end user has no influence on the location of DomeinFiles - they are stored in a local database.
Only the _creating_ user possibly has influence on the location of public resources (unless the modeller has set their location in 
the model itself). Unless specified otherwise, all context- and role instances go into a default local database.
But the end user can specify, per type, where his resources should be stored: either in e local database, or in a remote one.

Loosely inspired by libp2p multiadresses, we distinguish various _schemes_ for resource identifiers. 
On looking up a resource we dispatch to various mechanisms based on the scheme according to which the resource identifier is constructed. 
This also allows us to add new ways of storing resources in future versions of the PDR.

Here is a BNF description of resource identifiers:

resourceID = scheme '#' guid
scheme = defaultScheme | localScheme | remoteScheme | publicScheme | modelScheme
defaultScheme = 'def' ':'
localScheme = 'loc' ':' databaseName
remoteScheme = 'rem' ':' url
pubicScheme = 'pub' ':' url
modelScheme = model: '//' authority # LocalDomainIdentifier

databaseName conforms to Couchb database name restrictions.

url conforms to https://www.w3.org/TR/url-1/.

guid conforms to https://www.ietf.org/rfc/rfc4122.txt.

Finally, notice a convention: each scheme SHOULD be constructed from a preferably short prefix, 
separated from other parts of the scheme by a colon (:). In BNF (to be interpreted at a meta level wrt the definitions above):

scheme = prefix ':' any

ELABORATION OF THE VARIOUS SCHEMES

DEFAULT SCHEME
Identifiers following this scheme point to resources stored in the PDR's default resource store. 
Currently, this is implemented as a store accessible through Pouchdb, named <userID>_entities.

LOCAL SCHEME
The local scheme allows us to store resources in _another_ database than the default resource store, albeit locally 
(i.e. through Pouchdb with a non-url database name).

REMOTE SCHEME
The remote scheme allows us to fetch and store resources through some REST interface. 
The URL represents the endpoint where we fetch and store resources. The resources themselves are stored using the <guid> part as key.

PUBLIC SCHEME
The public scheme is just like the remote scheme. However, is is used exclusively to publish resources in a public Umwelt, or
in other words, according to a public-facing perspective. It is possible to fill a role with an identifier in the public scheme. 
Unlike the other schemes, the public scheme is preserved in deltas (the other identifiers are stripped to their guid parts,
as the receivers themselves decide on where to store them).
Note that the storage location of resources in the public: scheme is under control of the modeller. She can include the location 
verbatim in the model, or she can specify an expression that allows the end users of the model to set it. But this is different from
the control that the end user has over storage in the local and remote schemes (the latter can be set per user, while the former 
is set once for all users).

MODEL SCHEME
The model scheme form is designed to be equal to the model URI. It was introduced to be able to handle DomeinFiles through the 
classes Persistent and Cacheable. 

------------------------------------------------------------
------------------------------------------------------------}


-----------------------------------------------------------
-- DECOMPOSING RESOURCE IDENTIFIERS
-- In this section we have functions to split a string that represents a ResourceIdentifier
-- into constituent parts according to the scheme of the identifier (default, local or remote).
-----------------------------------------------------------
type ResourceIdentifier = String
type Guid = String
type LocalModelName = String

-- | EXAMPLES:
-- | def:0083caf8_6c12_4905_a7ce_1b10a40f0ad8
-- | loc:secretdatabase#0083caf8_6c12_4905_a7ce_1b10a40f0ad8
-- | rem:https://perspectives.domains/cw_perspectives_domains/#0083caf8_6c12_4905_a7ce_1b10a40f0ad8
data DecomposedResourceIdentifier = 
    Default TRANS.DbName Guid 
  | Local TRANS.DbName Guid 
  | Remote TRANS.Url TRANS.WriteUrl Guid
  | Public TRANS.Url TRANS.WriteUrl Guid
  | Model TRANS.DbName LocalModelName

-- | Match a string of word characters, separated by ":" from an arbitrary string.
resourceIdentifierRegEx :: Regex
resourceIdentifierRegEx = unsafeRegex "^(\\w+):(.*)$" noFlags

-- TODO: replace the first part of the match by a regex that captures Couchdb database names;
-- replace the second part by a regex that captures a GUID.
-- Match an arbitrary string built from word characters, separated by "#" from a
-- string built from word characters, digits and underscore.
locRegex :: Regex
locRegex = unsafeRegex "^(\\w+)#([_\\w\\d]+)$" noFlags

-- TODO: replace the first part of the match by a regex that captures a URL;
-- replace the second part by a regex that captures a GUID.
-- Match a string starting with "https://" followed by arbitrary characters, separated by "#" from a
-- string built from word characters, digits and underscore.
-- pub:https://perspectives.domains/cw_servers_and_repositories/perspectives_domains$External
remRegex :: Regex
remRegex = unsafeRegex "^(https://[^#]+)#([_\\w\\d]+)$" noFlags

parseResourceIdentifier :: forall f. ResourceIdentifier -> MonadPouchdb f DecomposedResourceIdentifier
parseResourceIdentifier resId = 
  case match resourceIdentifierRegEx resId of 
    Nothing -> throwError (error $ "Cannot parse this resource identifier: " <> resId)
    Just matches -> case index matches 1, index matches 2 of 
      Just (Just scheme), Just (Just rest) -> case scheme of 
        -- def:guid
        "def" -> do
          sysId <- getSystemIdentifier
          pure $ Default (sysId <> "_entities") rest
        -- loc:dbname#guid
        "loc" -> case match locRegex rest of
          Nothing -> throwError (error $ "Cannot parse this as a Local resource identifier: " <> resId) 
          Just locMatches -> case index locMatches 1, index locMatches 2 of 
            Just (Just dbName), Just (Just g) -> pure $ Local dbName g
            _, _ -> throwError (error $ "Cannot parse this as a Local resource identifier: " <> resId)
        -- rem:url#guid
        "rem" -> case match publicResourceRegex rest of
          Nothing -> throwError (error $ "Cannot parse this as a Remote resource identifier: " <> resId)
          Just remMatches -> case index remMatches 1, index remMatches 2 of 
            Just (Just url), Just (Just g) -> pure $ Remote (url <> "/") (url <> "_write/") g
            _, _ -> throwError (error $ "Cannot parse this as a Remote resource identifier: " <> resId)
        "pub" -> case match publicResourceRegex rest of
          Nothing -> throwError (error $ "Cannot parse this as a Public resource identifier: " <> resId)
          Just remMatches -> case index remMatches 1, index remMatches 2 of 
            Just (Just url), Just (Just g) -> pure $ Remote (url <> "/") (url <> "_write/") g
            _, _ -> throwError (error $ "Cannot parse this as a Public resource identifier: " <> resId)
        "model" -> getSystemIdentifier >>= \sysId -> pure $ Model (sysId <> "_models") (unsafePartial fromJust $ stripPrefix (Pattern "//") rest)
        -- rest is nu //perspectives.domains#System. Haal '//' eraf, maak er een Local van met default database voor modellen.
        _ -> throwError (error $ "Unknown resource identifier scheme: " <> resId)
      _, _ -> throwError (error $ "Cannot parse this resource identifier: " <> resId)

-----------------------------------------------------------
-- THE LOCAL IDENTIFIER OF A RESOURCE IDENTIFIER
-----------------------------------------------------------
-- | Returns the unique identifier for a resource that can be used in Deltas.
guid :: ResourceIdentifier -> MonadPerspectives Guid
guid = parseResourceIdentifier >=> pure <<< guid_

guid_ :: DecomposedResourceIdentifier -> Guid
guid_ (Default _ g) = g
guid_ (Local _ g) = g
guid_ (Remote _ _ g) = g
guid_ (Public _ _ g) = g
guid_ (Model _ g) = g

-----------------------------------------------------------
-- THE DATABASE PART (POSSIBLY A URL) OF A RESOURCE IDENTIFIER TO READ FROM
-----------------------------------------------------------
-- | Returns an identifier that is understood by Pouchdb in the sense that
-- | it creates a database accessor object from it, either locally or remote.
-- | This database can be read from.
pouchdbDatabaseName :: ResourceIdentifier -> MonadPerspectives TRANS.DbName
pouchdbDatabaseName = parseResourceIdentifier >=> pure <<< pouchdbDatabaseName_

pouchdbDatabaseName_ :: DecomposedResourceIdentifier -> TRANS.DbName
pouchdbDatabaseName_ (Default dbName _) = dbName
pouchdbDatabaseName_ (Local dbName _) = dbName
pouchdbDatabaseName_ (Remote dbName _ _) = dbName
pouchdbDatabaseName_ (Public dbName _ _) = dbName
pouchdbDatabaseName_ (Model dbName _) = dbName

-----------------------------------------------------------
-- THE DATABASE PART (POSSIBLY A URL) OF A RESOURCE IDENTIFIER TO WRITE TO
-----------------------------------------------------------
-- | Notice that what is returned in the Default and Local case is not an Url at all, but just a local 
-- | Couchdb database name.
writeUrl_ :: DecomposedResourceIdentifier -> TRANS.WriteUrl
writeUrl_ (Default dbName _) = dbName
writeUrl_ (Local dbName _) = dbName
writeUrl_ (Remote _ dbName _) = dbName
writeUrl_ (Public _ dbName _) = dbName
writeUrl_ (Model _ dbName) = dbName

-- | Pouchdb constructs a database accessor object from the identifier returned from 
-- | this ResourceIdentifier, that can be written to.
writeUrl :: ResourceIdentifier -> MonadPerspectives TRANS.WriteUrl
writeUrl = parseResourceIdentifier >=> pure <<< writeUrl_

-----------------------------------------------------------
-- DOCUMENT LOCATORS
-----------------------------------------------------------
-- | A DocLocator is a convenient structure to provide arguments to 
-- | various Pouchdb functions.
-- | Member `database` is either a Url or a local database name.
type DocLocator = 
  { database :: String 
  , documentName :: String
  }

resourceIdentifier2DocLocator :: ResourceIdentifier -> MonadPerspectives DocLocator
resourceIdentifier2DocLocator resId = do
  decomposed <- parseResourceIdentifier resId
  pure  { database: pouchdbDatabaseName_ decomposed
        , documentName: guid_ decomposed }

resourceIdentifier2WriteDocLocator :: ResourceIdentifier -> MonadPerspectives DocLocator
resourceIdentifier2WriteDocLocator resId = do
  decomposed <- parseResourceIdentifier resId
  pure  { database: writeUrl_ decomposed
        , documentName: guid_ decomposed }

-----------------------------------------------------------
-- CREATING IDENTIFIERS
-----------------------------------------------------------
-- | This function never creates an identifier in the Public scheme.
-- | Such identifiers are only ever created when processing a Transaction for a publishing Proxy role.
createResourceIdentifier :: ResourceType -> MonadPerspectivesTransaction ResourceIdentifier
createResourceIdentifier ctype = do
  mstorageScheme <- gets \(Transaction{typeToStorage}) -> lookup ctype typeToStorage 
  g <- show <$> liftEffect GUID.guid
  case mstorageScheme of
    Nothing -> createDefaultIdentifier g
    Just (TRANS.Default dbName) -> pure $ "def:" <> dbName <> "#" <> g
    Just (TRANS.Local dbName) -> createLocalIdentifier dbName g
    Just (TRANS.Remote url _) -> createRemoteIdentifier url g

  where
    createDefaultIdentifier :: Guid -> MonadPerspectivesTransaction ResourceIdentifier
    createDefaultIdentifier g = (lift getSystemIdentifier) >>= \dbName -> pure ("def:" <> dbName <> "#" <> g)

    createLocalIdentifier :: TRANS.DbName -> Guid -> MonadPerspectivesTransaction ResourceIdentifier
    createLocalIdentifier dbName g = pure ("loc:" <> dbName <> "#" <> g)

    createRemoteIdentifier :: TRANS.Url -> Guid -> MonadPerspectivesTransaction ResourceIdentifier
    createRemoteIdentifier url g = pure ("rem:" <> url <> "#" <> g)

-----------------------------------------------------------
-- GET THE SCHEME
-----------------------------------------------------------
type Scheme = String

getResourceIdentifierScheme :: ResourceIdentifier -> Maybe Scheme
getResourceIdentifierScheme s = case match resourceIdentifierRegEx s of 
  Nothing -> Nothing
  Just matches -> case index matches 1, index matches 2 of 
    Just (Just scheme), Just (Just rest) -> Just scheme
    _, _ -> Nothing

isInPublicScheme :: ResourceIdentifier -> Boolean
isInPublicScheme s = maybe false (eq "pub") (getResourceIdentifierScheme s)

-----------------------------------------------------------
-- TEST THE SHAPE OF A PUBLIC RESOURCE URL
-----------------------------------------------------------
-- | A pattern to match https://{authority}/cw_{databasename}/{SegmentedIdentifier} exactly.
-- | It is very permissive, allowing any character in the authority except the forward slash.
-- | The model name must start on an upper case alphabetic character.
-- | index 1 is the authority (scheme plus domain name).
-- | index 2 is the database name.
-- | index 3 is the resource name.
publicResourcePattern :: String
publicResourcePattern = "^(https://[^/]+/cw_[^/]+)/(.+)$"

publicResourceRegex :: Regex
publicResourceRegex = unsafeRegex publicResourcePattern noFlags

hasPublicResourceShape :: String -> Boolean
hasPublicResourceShape = test publicResourceRegex
