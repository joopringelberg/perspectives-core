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

module Perspectives.Identifiers

where

import Data.Array (intercalate, null, uncons, unsnoc)
import Data.Array.NonEmpty (NonEmptyArray, index)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.String (Pattern(..), Replacement(..), indexOf, replaceAll, split, splitAt, stripPrefix, stripSuffix)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex) 
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Show, append, eq, flip, identity, ($), (&&), (<<<), (<>), (==), (||), (+))

-- | The unsafeRegex function takes a string `pattern` as first argument and uses it as is in a new Regex(`pattern`).
-- | Hence, in Purescript, we can follow the rules for the new Regex("your pattern") approach.
-- | 
-- | So regexes are constructed from strings. The following characters have special meaning in Purescript string literals:
-- | \, ", ' (backslash, double quote, single quote). They must be escaped with a backslash to be used __as is__ in a string literal.
-- |
-- | The following character has special meaning in Regex patterns, while also occurring in Perspectives identifiers:
-- | $ (dollar sign). DOLLAR SIGN must be escaped in a Regex pattern to be matched literally.
-- | But because a backward slash has special meaning in Purescript strings, it must be escaped with a backslash!
-- | TO LITERALLY MATCH $, USE \\$
-- |
-- | / (forward slash) is used in Javascript syntax to delimit a regular expression. If you want to match it literally, you'll have to escape it.
-- | However, with the new Regex("your pattern") syntax, no escaping is necessary. As Purescript follows that syntax, 
-- | we DO NOT ESCAPE FORWARD SLASHES.
-- |
-- | As backslash has special meaning in Purescript string literals, we must escape it if we want to use it literally.
-- | Literal use comprises character class syntax, e.g. \w (for word characters). 
-- | FOR CHARACTER CLASSES, USE DOUBLE BACKSLASH: \\w, \\n, etc.


-----------------------------------------------------------
-----------------------------------------------------------
--      MODEL URIS AND HOW TO TRANSFORM THEM
-----------------------------------------------------------
-----------------------------------------------------------
-- | A Namespace has the general shape model://perspect.it#System@1.0.0-alpha

-- | A pattern to match "model://some.authority#Modelname" exactly.
-- | It is very permissive, allowing any character in the authority except the forward slash.
-- | The model name must start on an upper case alphabetic character.
-- | The first group captures the authority.
-- | The second group captures the local model name that is unique within the authority domain.
newModelPattern :: String
newModelPattern = "^model://([^/]+)#([A-Z][^\\$/]+)$"

newModelRegex :: Regex
newModelRegex = unsafeRegex newModelPattern noFlags

type ModelUri = String
type DomeinFileName = String

-----------------------------------------------------------
-- TESTING FOR A MODEL URI
-----------------------------------------------------------
isModelUri :: String -> Boolean
isModelUri = isJust <<< match newModelRegex

-----------------------------------------------------------
-- MODEL URI TO MODEL URL
-- Fetch the model from a repository using this URL.
-----------------------------------------------------------
-- | Transform a model URI of the form 
-- |	  model://{subdomains-with-dots}.{authority-with-dots}/{LocalModelName}
-- | to:
-- |    { repositoryUrl: https://{authority-with-dots}/models_{subdomains-with-underscores}_{authority-with-underscores}
-- |    , documentName: {subdomains-with-underscores}_{authority-with-underscores}-{LocalModelName}.json}
-- | Where LocalModelName may include a Semantic Version Number, as in "System@1.0.0"
-- | When concatenated with a forward slash in between, those two parts form:
-- |    https://{authority-with-dots}/models_{subdomains-with-underscores}_{authority-with-underscores}/{subdomains-with-underscores}_{authority-with-underscores}-{LocalModelName}.json
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
modelUri2ModelUrl :: Partial => String -> {repositoryUrl :: String, documentName :: String}
modelUri2ModelUrl s = let
    (matches :: NonEmptyArray (Maybe String)) = fromJust $ match newModelRegex s
    (authority :: String) = fromJust $ fromJust $ index matches 1
    (localModelName :: String) = fromJust $ fromJust $ index matches 2
    (namespaceParts :: Array String) = split (Pattern ".") authority
    {init:lowerParts, last:toplevel} = fromJust $ unsnoc namespaceParts
    {init:subNamespaces, last:secondLevel} = fromJust $ unsnoc lowerParts
  in
    { repositoryUrl: "https://" <> secondLevel <> "." <> toplevel <> "/models_" <> intercalate "_" namespaceParts
    , documentName: (intercalate "_" namespaceParts) <> "-" <>  localModelName <> ".json"}

-----------------------------------------------------------
-- MODEL URI TO MANIFEST URL
-- The Manifest is served from this Repository Url.
-----------------------------------------------------------
modelUri2ManifestUrl :: Partial => String -> {repositoryUrl :: String, manifestName :: String}
modelUri2ManifestUrl s = let
    (matches :: NonEmptyArray (Maybe String)) = fromJust $ match newModelRegex s
    (authority :: String) = fromJust $ fromJust $ index matches 1
    (localModelName :: String) = fromJust $ fromJust $ index matches 2
    (namespaceParts :: Array String) = split (Pattern ".") authority
    {init:lowerParts, last:toplevel} = fromJust $ unsnoc namespaceParts
    {init:subNamespaces, last:secondLevel} = fromJust $ unsnoc lowerParts
  in
    { repositoryUrl: "https://" <> secondLevel <> "." <> toplevel <> "/cw_" <> intercalate "_" namespaceParts
    , manifestName: (intercalate "_" namespaceParts) <> "-" <>  localModelName}

-----------------------------------------------------------
-- MODEL URI TO REPOSITORY URL
-- The DomeinFile is served from this Repository Url.
-----------------------------------------------------------
-- | Transform a model URI of the form 
-- |	  model://{subdomains-with-dots}.{authority-with-dots}/{LocalModelName}
-- | to:
-- |    https://{authority-with-dots}/models_{subdomains-with-underscores}_{authority-with-underscores}
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
modelUri2ModelRepository :: Partial => String -> String
modelUri2ModelRepository s = let
    (matches :: NonEmptyArray (Maybe String)) = fromJust $ match newModelRegex s
    (authority :: String) = fromJust $ fromJust $ index matches 1
    (namespaceParts :: Array String) = split (Pattern ".") authority
    {init:lowerParts, last:toplevel} = fromJust $ unsnoc namespaceParts
    {init:subNamespaces, last:secondLevel} = fromJust $ unsnoc lowerParts
  in
    "https://" <> secondLevel <> "." <> toplevel <> "/models_" <> intercalate "_" namespaceParts

-----------------------------------------------------------
-- STRIP A MODEL URI FROM ITS VERSION
-----------------------------------------------------------
-- | Remove the version from a model URI. E.g. "model://joopringelberg.nl#TestQueries@1.0" becomes "model://joopringelberg.nl#TestQueries"
unversionedModelUri :: String -> String
unversionedModelUri s = case indexOf (Pattern "@") s of 
  Nothing -> s
  Just u -> case splitAt u s of 
    {before} -> before

-----------------------------------------------------------
-- GET THE VERSION OF A MODEL URI
-----------------------------------------------------------
modelUriVersion :: String -> Maybe String
modelUriVersion s = case indexOf (Pattern "@") s of
  Nothing -> Nothing
  Just u -> case splitAt (u + 1) s of
    {after} -> Just after

-----------------------------------------------------------
-- MODEL URI TO INSTANCES STORE
-- Instances representing the repository and its manifests are stored here.
-----------------------------------------------------------
-- | Transform a model URI of the form 
-- |	  model://{subdomains-with-dots}.{authority-with-dots}/{LocalModelName}
-- | to:
-- |    https://{authority-with-dots}/cw_{subdomains-with-underscores}_{authority-with-underscores}/
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
modelUri2InstancesStore :: Partial => String -> String
modelUri2InstancesStore s = let
    (matches :: NonEmptyArray (Maybe String)) = fromJust $ match newModelRegex s
    (authority :: String) = fromJust $ fromJust $ index matches 1
    (namespaceParts :: Array String) = split (Pattern ".") authority
    {init:lowerParts, last:toplevel} = fromJust $ unsnoc namespaceParts
    {init:subNamespaces, last:secondLevel} = fromJust $ unsnoc lowerParts
  in
    "https://" <> secondLevel <> "." <> toplevel <> "/cw_" <> intercalate "_" namespaceParts

-----------------------------------------------------------
-- URL2AUTHORITY
-----------------------------------------------------------
urlRegex :: Regex
urlRegex = unsafeRegex "^(https://[^/]+/).*$" noFlags

url2Authority :: String -> Maybe String
url2Authority = getFirstMatch urlRegex

-----------------------------------------------------------
-- QUALIFY A NAME
-----------------------------------------------------------
qualifyWith :: Namespace -> String -> String
qualifyWith ns = append (ns <> "$")

-----------------------------------------------------------
-----------------------------------------------------------
--      TYPE URIS AND HOW TO TRANSFORM THEM
-----------------------------------------------------------
-----------------------------------------------------------
-- 	type URI = model URI '$' segmentedName

type TypeUri = String

-- | A pattern to match "model://some.authority#Modelname$Sometype" exactly.
-- | The model URI capturing group is exactly equal to that of the newModelPattern except for the capturing groups.
-- | It is very permissive: 
-- |   - allowing any character in the authority except the forward slash;
-- |   - allowing any character in the model name but the $ sign;
-- |   - however, the model name should start on an upper case alphabetic character;
-- |   - and then we allow everything up till the end. This may be nothing, so we accept a model URI as a type name, too.
-- |     This is to cover the top context of a model.
-- | The first group captures the model URI; the second captures the segmented type name.
typePattern :: String
typePattern = "^(model://[^/]+#[A-Z][^\\$]+)\\$?(.*)$"

typeRegex :: Regex
typeRegex = unsafeRegex typePattern noFlags

-- | This regex is like the typeRegex but it separates THE LAST segment from the rest of the expression.
localNameRegEx :: Regex
localNameRegEx = unsafeRegex "^model://[^/]+#[A-Z][^#/]+\\$([A-Z][^\\$/]+)$" noFlags

-----------------------------------------------------------
-- TEST IF A STRING IS A TYPE URI
-----------------------------------------------------------
-- | Does NOW ALSO match: "model://perspectives.domains#System"
-- | Does match "model://perspectives.domains#System$PerspectivesSystem$User"
isTypeUri :: String -> Boolean
isTypeUri = isJust <<< match typeRegex

-----------------------------------------------------------
-- TAKE THE MODEL URI FROM A TYPE URI
-----------------------------------------------------------
-- | Transforms
-- |    model://perspectives.domains#System$PerspectivesSystem
-- | to 
-- |    model://perspectives.domains#System
-- |
-- | Strips away the first '$' and everything following it.
-- | Does not test whether the input actually is a TypeUri!
typeUri2ModelUri :: TypeUri -> Maybe ModelUri
typeUri2ModelUri = getFirstMatch typeRegex

typeUri2ModelUri_ :: Partial => TypeUri -> ModelUri
typeUri2ModelUri_ = fromJust <<< typeUri2ModelUri

-----------------------------------------------------------
-- TAKE THE TYPE NAMESPACE FROM A TYPE NAME
-- (the type namespace is the type directly enclosing the given type)
-----------------------------------------------------------
-- | Returns the entire name but for the last segment. This is the namespace of the local name.
-- | typeUri2typeNameSpace "model:Model$First$Second" == Just "model:Model$First"
-- | typeUri2typeNameSpace "model:Model" == Just "model:Model"
typeUri2typeNameSpace :: String -> Maybe Namespace
typeUri2typeNameSpace s = case typeUri2LocalName s of
  Nothing -> Just s
  (Just ln) -> stripSuffix (Pattern $ "$" <> ln) s

typeUri2typeNameSpace_ :: String -> Namespace
typeUri2typeNameSpace_ = unsafePartial $ fromJust <<< typeUri2typeNameSpace

-----------------------------------------------------------
-- REMOVE THE TYPE NAMESPACE FROM A TYPE NAME (=GET THE LOCALNAME PART)
-- (the type namespace is the type directly enclosing the given type)
-----------------------------------------------------------
-- | Returns "System" from "model:perspectives.domains$PerspectivesSystem$System" or Nothing
-- | So this function returns THE LAST SEGMENT of the name.
typeUri2LocalName :: String -> Maybe String
typeUri2LocalName = getFirstMatch localNameRegEx

typeUri2LocalName_ :: String -> String
typeUri2LocalName_ s = unsafePartial (fromJust (typeUri2LocalName s))

-----------------------------------------------------------
-- TEST FOR A LOCAL NAME
-----------------------------------------------------------
-- | "model:perspectives.domains$PerspectivesSystem$System" `hasLocalName` "System"
hasLocalName :: String -> String -> Boolean
hasLocalName qn ln = case typeUri2LocalName qn of
  Nothing -> false
  (Just ln') -> ln == ln'

-- | "System" `isLocalNameOf` "model:perspectives.domains$PerspectivesSystem$System"
-- | "Context" `isLocalNameOf` "model:Perspectives$Context"
isLocalNameOf :: String -> String -> Boolean
isLocalNameOf = flip hasLocalName

-----------------------------------------------------------
-- CONVERT A TYPE URI TO A STRING COMPATIBLE WITH COUCHDB FILE NAME RESTRICTIONS
-- Allowed Characters: The document ID can be any string value, but it must not 
-- contain control characters (e.g., null, tab, line feed) or the forward slash ("/") character. 
-- Other than these, any printable UTF-8 character is generally allowed.
-----------------------------------------------------------
typeUri2couchdbFilename :: String -> String
typeUri2couchdbFilename = replaceAll (Pattern "/") (Replacement "_")

-----------------------------------------------------------
-- CURIES
-----------------------------------------------------------
curieRegEx :: Regex
curieRegEx = unsafeRegex "^(\\w+)\\:([\\w|\\$]+)" noFlags

-- | Returns 'pre' from 'pre:someurl' or Nothing.
deconstructPrefix :: String -> Maybe Prefix
deconstructPrefix = getFirstMatch curieRegEx

-- | Returns "someurl" from "pre:someurl" or Nothing
deconstructLocalNameFromCurie :: String -> Maybe String
deconstructLocalNameFromCurie = getSecondMatch curieRegEx

-----------------------------------------------------------
-- PRICING APART SEGMENTED NAMES
-----------------------------------------------------------

-- | True iff the second argument is a suffix of the first argument, or if they are equal.
-- | Does not check whether names are well-formed qualified names.
-- | whole `endsWithSegments` part
-- | "model:Model$First$Second$Third" `endsWithSegments` "Second$Third" == true
-- | "model:Model$First$Second$Third" `endsWithSegments` "Second" == false
-- | "model:Model$First$Second$Third" `endsWithSegments` "model:Model$First$Second$Third" == true
endsWithSegments :: String -> String -> Boolean
endsWithSegments whole part = (whole == part) || (isJust $ stripSuffix (Pattern ("$" <> part)) whole)

-- | "Second$Third" `areLastSegmentsOf` "model:Model$First$Second$Third" == true
-- | "Second" `areLastSegmentsOf` "model:Model$First$Second$Third" == false
areLastSegmentsOf :: String -> String -> Boolean
areLastSegmentsOf = flip endsWithSegments

-- | True iff the second argument is a prefix of the first argument, or if they are equal.
-- | Does not check whether names are well-formed qualified names.
-- | "model:Model$First$Second$Third" `startsWithSegments` "model:Model$First$Second" == true
startsWithSegments :: String -> String -> Boolean
startsWithSegments whole part = (whole == part) || (isJust $ stripPrefix (Pattern (part <> "$")) whole)

-----------------------------------------------------------
-- CONCATENATE SEGMENTS
-----------------------------------------------------------
-- | The first argument (left) should be a qualfied name.
-- | The second argument may be a series of segments.
-- | Both are concatenated, guaranteeing that segments that are both
-- | the tail of the left part and the head of the right part, only occur once.
concatenateSegments :: String -> String -> String
concatenateSegments left right = run (split (Pattern "$") left) (split (Pattern "$") right) ""
  where
    run :: Array String -> Array String -> String -> String
    run leftParts rightParts result = case uncons leftParts of
      Nothing -> if null rightParts
        -- Nothing to append, we're done.
        then result
        -- We've completely included the left side in the result, just add the right part.
        else result <> "$" <> intercalate "$" rightParts
      Just {head, tail} -> case uncons rightParts of
        -- We've completely included the right side in the result.
        -- We'd expect the left side to be included, too, but apparantly not.
        -- This is an unusual case, following this pattern: abdc bd
        Nothing -> result <> "$" <> intercalate "$" leftParts
        Just {head:rightHead, tail:rightTail} -> if head == rightHead
          -- Overlap. Append the overlapping segment to the result, drop it from both sides.
          then run tail rightTail (result <> "$" <> head)
          -- Append the next segment from left, continue with all parts from right.
          else if result == ""
            then run tail rightParts head
            else run tail rightParts (result <> "$" <> head)

-----------------------------------------------------------
-- REGEX MATCHING HELPER FUNCTIONS
-----------------------------------------------------------
getFirstMatch :: Regex -> String -> Maybe String
getFirstMatch regex s = case match regex s of
  (Just matches) -> maybe Nothing identity (index matches 1)
  _ -> Nothing

getSecondMatch :: Regex -> String -> Maybe String
getSecondMatch regex s = case match regex s of
  (Just (matches :: NonEmptyArray (Maybe String))) -> maybe Nothing identity (index matches 2)
  _ -> Nothing

-----------------------------------------------------------
-- THE MODEL:USER DOMEIN
-----------------------------------------------------------
-- | Matches all segments of the name (the string after the first "$")
userNameRegEx :: Regex
userNameRegEx = unsafeRegex "^def:#(.*)\\$User" noFlags

-- Used in module Perspectives.Persistence.CouchdbFunctions. Returns the schemeless system identifier.
-- Will probably be OBSOLETE now since we have new resource identifiers.
deconstructUserName :: String -> Maybe String
deconstructUserName = getFirstMatch userNameRegEx

-----------------------------------------------------------
-- NAMESPACE, MODELNAME
-----------------------------------------------------------
-- | From a well-formed identifier of a ContextInstance, construct the identifier of its External Role.
buitenRol :: String -> String
buitenRol s = s <> "$External"

-- | Returns the identifier minus the "$External" part.
deconstructBuitenRol :: String -> String
deconstructBuitenRol s = case stripSuffix (Pattern "$External") s of
  Nothing -> s
  Just n -> n

isExternalRole :: String -> Boolean
isExternalRole n = isJust $ stripSuffix (Pattern "External") n

type LocalName = String
type Prefix = String

type PEIdentifier = String

type Namespace = String

-- Used in ContextRoleParser, IndentParser, PerspectSyntax.
-- | A QualifiedName consists of a namespace and a local name.
data QualifiedName = QualifiedName Namespace LocalName

instance showQualifiedName :: Show QualifiedName where
  show (QualifiedName mn ln) = if isUrl mn 
    then mn <> ln 
    -- temporary hack. CRL is on its way out
    else if mn == "def"
      then mn <> ":" <> ln 
      else mn <> "$" <> ln

instance eqQualifiedName :: Eq QualifiedName where
  eq (QualifiedName ns1 ln1) (QualifiedName ns2 ln2) = eq ns1 ns2 && eq ln1 ln2

urlRegEx :: Regex
urlRegEx = unsafeRegex "^http" noFlags

isUrl :: String -> Boolean
isUrl = test urlRegEx

