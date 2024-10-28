module Perspectives.ResourceIdentifiers.Parser where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty (index)
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.Identifiers (modelUri2ModelUrl)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (MonadPouchdb, Url, DatabaseName)

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
-- | model://perspectives.domains#System
data DecomposedResourceIdentifier = 
    Default DatabaseName Guid 
  | Local DatabaseName Guid 
  | Remote Url Guid
  | Public Url Guid
  | Model DatabaseName LocalModelName

-- | Match a string of word characters, separated by ":" from an arbitrary string.
resourceIdentifierRegEx :: Regex
resourceIdentifierRegEx = unsafeRegex "^(\\w+):(.*)$" noFlags

-- TODO: replace the first part of the match by a regex that captures Couchdb database names;
-- replace the second part by a regex that captures a GUID.
-- Match an arbitrary string built from word characters, separated by "#" from a
-- string built from word characters, digits and underscore.
locRegex :: Regex
locRegex = unsafeRegex "^(\\w+)#([_\\w\\d]+)$" noFlags

parseResourceIdentifier :: forall f. ResourceIdentifier -> MonadPouchdb f DecomposedResourceIdentifier
parseResourceIdentifier resId = 
  case match resourceIdentifierRegEx resId of 
    Nothing -> throwError (error $ "Cannot parse this resource identifier: " <> resId)
    Just matches -> case index matches 1, index matches 2 of 
      Just (Just scheme), Just (Just rest) -> case scheme of 
        -- def:#guid
        "def" -> do
          sysId <- getSystemIdentifier
          pure $ Default (sysId <> "_entities") (drop 1 rest)
        -- loc:dbname#guid
        "loc" -> case match locRegex rest of
          Nothing -> throwError (error $ "Cannot parse this as a Local resource identifier: " <> resId) 
          Just locMatches -> case index locMatches 1, index locMatches 2 of 
            Just (Just dbName), Just (Just g) -> pure $ Local dbName g
            _, _ -> throwError (error $ "Cannot parse this as a Local resource identifier: " <> resId)
        -- rem:url#guid
        "rem" -> case match publicResourceUrlRegex rest of
          Nothing -> throwError (error $ "Cannot parse this as a Remote resource identifier: " <> resId)
          Just remMatches -> case index remMatches 1, index remMatches 2 of 
            Just (Just url), Just (Just g) -> pure $ Remote (url <> "/") g
            _, _ -> throwError (error $ "Cannot parse this as a Remote resource identifier: " <> resId)
        "pub" -> case match publicResourceUrlRegex rest of
          Nothing -> throwError (error $ "Cannot parse this as a Public resource identifier: " <> resId)
          Just remMatches -> case index remMatches 1, index remMatches 2 of 
            Just (Just url), Just (Just g) -> pure $ Remote (url <> "/") g
            _, _ -> throwError (error $ "Cannot parse this as a Public resource identifier: " <> resId)
        "model" -> do
          sysId <- getSystemIdentifier
          {documentName} <- pure $ unsafePartial modelUri2ModelUrl resId
          pure $ Model (sysId <> "_models") documentName
        -- "model" -> getSystemIdentifier >>= \sysId -> pure $ Model (sysId <> "_models") (unsafePartial fromJust $ stripPrefix (Pattern "//") (rest <> ".json"))
        -- rest is nu //perspectives.domains#System. Haal '//' eraf, maak er een Local van met default database voor modellen.
        _ -> throwError (error $ "Unknown resource identifier scheme: " <> resId)
      _, _ -> throwError (error $ "Cannot parse this resource identifier: " <> resId)

-----------------------------------------------------------
-- TEST THE SHAPE OF A PUBLIC RESOURCE URL
-----------------------------------------------------------
-- | A pattern to match https://{authority}/cw_{databasename}/{SegmentedIdentifier} exactly.
-- | It is very permissive, allowing any character in the authority except the forward slash.
-- | The model name must start on an upper case alphabetic character.
-- | index 1 is the authority (scheme plus domain name).
-- | index 2 is the database name.
-- | index 3 is the resource name.
publicResourceUrlPattern :: String
publicResourceUrlPattern = "^(https://[^/]+/cw_[^/]+)/#(.+)$"

publicResourceUrlRegex :: Regex
publicResourceUrlRegex = unsafeRegex publicResourceUrlPattern noFlags

-----------------------------------------------------------
-- THE DATABASE PART (POSSIBLY A URL) OF A RESOURCE IDENTIFIER TO READ FROM
-----------------------------------------------------------
-- | Returns an identifier that is understood by Pouchdb in the sense that
-- | it creates a database accessor object from it, either locally or remote.
-- | This database can be read from.
pouchdbDatabaseName :: forall f. ResourceIdentifier -> MonadPouchdb f DatabaseName
pouchdbDatabaseName = parseResourceIdentifier >=> pure <<< pouchdbDatabaseName_

pouchdbDatabaseName_ :: DecomposedResourceIdentifier -> DatabaseName
pouchdbDatabaseName_ (Default dbName _) = dbName
pouchdbDatabaseName_ (Local dbName _) = dbName
pouchdbDatabaseName_ (Remote dbName _) = dbName
pouchdbDatabaseName_ (Public dbName _) = dbName
pouchdbDatabaseName_ (Model dbName _) = dbName

