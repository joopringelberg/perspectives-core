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
import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty (index)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, MonadPerspectives)
import Perspectives.CoreTypes (StorageScheme(..)) as CT
import Perspectives.Cuid2 (cuid2)
import Perspectives.Persistence.State (getCouchdbBaseURL, getSystemIdentifier)
import Perspectives.Persistence.Types (MonadPouchdb, Url)
import Perspectives.Representation.TypeIdentifiers (ResourceType)
import Perspectives.ResourceIdentifiers.Parser (DecomposedResourceIdentifier(..), Guid, ResourceIdentifier, parseResourceIdentifier, resourceIdentifierRegEx, pouchdbDatabaseName_)

{------------------------------------------------------------
------------------------------------------------------------
RESOURCE IDENTIFIYING SCHEMES
This module is about identifiers for context- and role instances, collectively called _resources_.  It also covers DomeinFiles,
which are resources of another kind. 
NOTICE that this is not about DomeinFiles in repositories!
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
For example, if the system instance name is "dev1", it is identified by def:#dev1

LOCAL SCHEME
The local scheme allows us to store resources in _another_ database than the default resource store, albeit locally 
(i.e. through Pouchdb with a non-url database name).
For example, a bank account role kept safe in a separate local database where the guid would be "myAccount" would be represented with 
loc:mybankingstuff#myAccount

REMOTE SCHEME
The remote scheme allows us to fetch and store resources through some REST interface. 
The URL represents the endpoint where we fetch and store resources. The resources themselves are stored using the <guid> part as key.
Same bank account role example but now stored with the bank:
rem:https://mybank.com/customerdata#myAccount

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
-- THE LOCAL IDENTIFIER OF A RESOURCE IDENTIFIER
-----------------------------------------------------------
-- | Returns the unique identifier for a resource that can be used in Deltas.
guid :: ResourceIdentifier -> MonadPerspectives Guid
guid = parseResourceIdentifier >=> pure <<< guid_

guid_ :: DecomposedResourceIdentifier -> Guid
guid_ (Default _ g) = g
guid_ (Local _ g) = g
guid_ (Remote _ g) = g
guid_ (Public _ g) = g
guid_ (Model _ g) = g

-----------------------------------------------------------
-- THE DATABASE LOCATION (POSSIBLY A URL) OF A RESOURCE IDENTIFIER TO READ FROM
-----------------------------------------------------------
-- | Returns an URL for all databases except for IndexedDB.
databaseLocation :: forall f. ResourceIdentifier -> MonadPouchdb f (Maybe Url)
databaseLocation s = do
  r <- parseResourceIdentifier s 
  case r of 
    Default dbName _ -> addBase dbName
    Local dbName _ -> addBase dbName
    Model dbName _ -> addBase dbName
    Remote url _ -> pure $ Just url
    Public url _ -> pure $ Just url
  where 
  addBase :: String -> MonadPouchdb f (Maybe String)
  addBase dbname = (map ((flip append) dbname)) <$> getCouchdbBaseURL

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
  pure  { database: pouchdbDatabaseName_ decomposed
        , documentName: guid_ decomposed }

-----------------------------------------------------------
-- CREATING IDENTIFIERS
-----------------------------------------------------------
-- | This function never creates an identifier in the Public scheme.
-- | Such identifiers are only ever created when processing a Transaction for a publishing Proxy role.
createResourceIdentifier :: ResourceType -> MonadPerspectivesTransaction ResourceIdentifier
createResourceIdentifier ctype = do
  s <- lift $ getSystemIdentifier
  g <- liftEffect $ cuid2 s
  createResourceIdentifier' ctype g  

-- | This function never creates an identifier in the Public scheme.
-- | Such identifiers are only ever created when processing a Transaction for a publishing Proxy role.
createResourceIdentifier' :: ResourceType -> String -> MonadPerspectivesTransaction ResourceIdentifier
createResourceIdentifier' ctype g = do
  mstorageScheme <- lift $ gets \({typeToStorage}) -> lookup ctype typeToStorage 
  case mstorageScheme of
    Nothing -> pure $ createDefaultIdentifier g
    Just (CT.Default _) -> pure $ createDefaultIdentifier g
    Just (CT.Local dbName) -> pure $ createLocalIdentifier dbName g
    Just (CT.Remote url) -> pure $ createRemoteIdentifier url g

createCuid :: MonadPerspectives String
createCuid = do
  s <- getSystemIdentifier
  liftEffect $ cuid2 s

createDefaultIdentifier :: String -> ResourceIdentifier
createDefaultIdentifier g = "def:#" <> g

type DbName = String
createLocalIdentifier :: DbName -> String -> ResourceIdentifier
createLocalIdentifier dbName g = "loc:" <> dbName <> "#" <> g

createRemoteIdentifier :: Url -> String -> ResourceIdentifier
createRemoteIdentifier url g = "rem:" <> url <> "#" <> g

addPublicScheme :: String -> ResourceIdentifier
addPublicScheme s = "pub:" <> s

-- | Add the public scheme and the url unless the identifier is already in the public scheme.
createPublicIdentifier :: String -> String -> ResourceIdentifier
createPublicIdentifier url s = if isInPublicScheme s
  then s
  else "pub:" <> url <> "#" <> s

-----------------------------------------------------------
-- GET THE SCHEME
-----------------------------------------------------------
type Scheme = String

-- | The prefix up to but not including the colon.
getResourceIdentifierScheme :: ResourceIdentifier -> Maybe Scheme
getResourceIdentifierScheme s = case match resourceIdentifierRegEx s of 
  Nothing -> Nothing
  Just matches -> case index matches 1, index matches 2 of 
    Just (Just scheme), Just (Just rest) -> Just scheme
    _, _ -> Nothing

isInPublicScheme :: ResourceIdentifier -> Boolean
isInPublicScheme s = maybe false (eq "pub") (getResourceIdentifierScheme s)

-----------------------------------------------------------
-- TRANSFORM A SCHEMED RESOURCE IDENTIFIER INTO AN IDENTIFIER IN A DELTA
-----------------------------------------------------------
-- | For the public scheme, just retain the entire ResourceIdentifier.
-- | For all other schemes, take the guid.
-- | Notice that identifiers that are not well-shaped are returned as such.
stripNonPublicIdentifiers :: ResourceIdentifier -> String
stripNonPublicIdentifiers s = if isInPublicScheme s
  then s
  else takeGuid s

-- | Captures everything following the "#" as its first and only capturing group.
discardStorageRegex :: Regex
discardStorageRegex = unsafeRegex "^[^#$]+#(.+)" noFlags

-- | Just retains the (unique) identifier of the ResourceIdentifier; 
-- | discards all storage information such as scheme, database name or url.
takeGuid :: ResourceIdentifier -> String
takeGuid s = case match discardStorageRegex s of
  Nothing -> s
  Just matches -> case index matches 1 of
    Just (Just g) -> g
    _ -> s

-----------------------------------------------------------
-- TEST THE SHAPE OF A PUBLIC RESOURCE IDENTIFIER
-----------------------------------------------------------
publicResourceRegex :: Regex
publicResourceRegex = unsafeRegex "^pub:https://[^/]+/cw_[^/]+/#(.+)$" noFlags

hasPublicResourceShape :: String -> Boolean
hasPublicResourceShape = test publicResourceRegex
