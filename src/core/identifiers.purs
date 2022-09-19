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

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (intercalate, null, uncons, unsnoc)
import Data.Array.NonEmpty (NonEmptyArray, index)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split, stripPrefix, stripSuffix)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Exception (Error, error)
import Partial.Unsafe (unsafePartial)
import Perspectives.Utilities (onNothing')
import Prelude (class Eq, class Show, append, eq, flip, identity, join, ($), (&&), (<$>), (<*>), (<<<), (<>), (==), (||))

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
-- NAMESPACE, MODELNAME
-----------------------------------------------------------
-- | A Namespace has the general shape model://perspect.it/System@1.0.0-alpha
-- | /^model:\/\/([^$]*)$/ or new Regex( "^model://([^$]*)$" )
modelPattern :: String
modelPattern = "^(model:(?://)?[^\\$]*)"
modelRegex :: Regex
modelRegex = unsafeRegex (modelPattern <> "$") noFlags

-- | True iff the string is exactly of the form model://Domain
isModelName :: String -> Boolean
isModelName s = test modelRegex s

-- | From a well-formed identifier of a ContextInstance, construct the identifier of its External Role.
buitenRol :: String -> String
buitenRol s = if isModelName s
  then s <> "$_External"
  else s <> "$External"

-- | Returns the identifier minus the "$External" or "$_External" part.
deconstructBuitenRol :: String -> String
deconstructBuitenRol s = replaceAll (Pattern "$External") (Replacement "")(replaceAll (Pattern "$_External") (Replacement "") s)

isExternalRole :: String -> Boolean
isExternalRole n = isJust $ stripSuffix (Pattern "External") n


-- | Return the Namespace that is the last segment of the URL.
namespaceFromUrl :: String -> Maybe String
namespaceFromUrl = getFirstMatch (unsafeRegex ".*\\/(model:.+)" noFlags)

type Namespace = String
type LocalName = String
type Prefix = String

type PEIdentifier = String

-- | Only a psp:Context can have a ModelName. In other words, if something has a ModelName, its pspType is psp:Context.
-- | However, a psp:Context may have a QualifiedName!
newtype ModelName = ModelName Namespace

instance showModelName :: Show ModelName where
  show (ModelName mn) = mn

instance eqModelName :: Eq ModelName where
  eq (ModelName n1) (ModelName n2) = n1 == n2

-- | A QualifiedName consists of a namespace and a local name.
data QualifiedName = QualifiedName Namespace LocalName

instance showQualifiedName :: Show QualifiedName where
  show (QualifiedName mn ln) = if isUrl mn 
    then mn <> ln 
    else mn <> "$" <> ln

instance eqQualifiedName :: Eq QualifiedName where
  eq (QualifiedName ns1 ln1) (QualifiedName ns2 ln2) = eq ns1 ns2 && eq ln1 ln2

-----------------------------------------------------------
-- URI STYLE MODEL NAMES
-----------------------------------------------------------
-- | A Namespace has the general shape model://perspect.it/System@1.0.0-alpha

-- | A pattern to match "model:Modelname" exactly.
oldModelPattern :: String
oldModelPattern = "^model:[^\\$/]*$"

oldModelRegex :: Regex
oldModelRegex = unsafeRegex oldModelPattern noFlags

-- | A pattern to match "model://some.authority/Modelname" exactly.
-- | It is very permissive, allowing any character in the authority except the forward slash.
-- | The model name must start on an upper case alphabetic character.
newModelPattern :: String
newModelPattern = "^model://([^/]+)/([A-Z][^\\$/]+)$"

newModelRegex :: Regex
newModelRegex = unsafeRegex newModelPattern noFlags

-- | Maps both an old and a new style model name to the old style.
namespace2modelname :: String -> Maybe String
namespace2modelname s = if test oldModelRegex s
  then Just s
  else (<>) <$> Just "model:" <*> getSecondMatch newModelRegex s

namespace2modelname_ :: Partial => String -> String
namespace2modelname_ = fromJust <<< namespace2modelname

-- | Transform a model URI of the form 
-- |	  model://{subdomains-with-dots}.{authority-with-dots}/{LocalModelName}
-- | to:
-- |    https://{authority-with-dots}/models_{subdomains-with-underscores}_{authority-with-underscores}/{LocalModelName}.json
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
modelName2modelUrl :: Partial => String -> String
modelName2modelUrl s = let
    (matches :: NonEmptyArray (Maybe String)) = fromJust $ match newModelRegex s
    (hierarchicalNamespace :: String) = fromJust $ fromJust $ index matches 1
    (localModelName :: String) = fromJust $ fromJust $ index matches 2
    (namespaceParts :: Array String) = split (Pattern ".") hierarchicalNamespace
    {init:lowerParts, last:toplevel} = fromJust $ unsnoc namespaceParts
    {init:subNamespaces, last:secondLevel} = fromJust $ unsnoc lowerParts
  in
    "https://" <> secondLevel <> "." <> toplevel <> "/models_" <> intercalate "_" namespaceParts <> "/" <> localModelName <> ".json"

-- | Transform a model URI of the form 
-- |	  model://{subdomains-with-dots}.{authority-with-dots}/{LocalModelName}
-- | to:
-- |    https://{authority-with-dots}/models_{subdomains-with-underscores}_{authority-with-underscores}
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
modelName2modelStore :: Partial => String -> String
modelName2modelStore s = let
    (matches :: NonEmptyArray (Maybe String)) = fromJust $ match newModelRegex s
    (hierarchicalNamespace :: String) = fromJust $ fromJust $ index matches 1
    (namespaceParts :: Array String) = split (Pattern ".") hierarchicalNamespace
    {init:lowerParts, last:toplevel} = fromJust $ unsnoc namespaceParts
    {init:subNamespaces, last:secondLevel} = fromJust $ unsnoc lowerParts
  in
    "https://" <> secondLevel <> "." <> toplevel <> "/models_" <> intercalate "_" namespaceParts

-- | Transform a model URI of the form 
-- |	  model://{subdomains-with-dots}.{authority-with-dots}/{LocalModelName}
-- | to:
-- |    https://{authority-with-dots}/cw_{subdomains-with-underscores}_{authority-with-underscores}/
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
modelName2NamespaceStore :: Partial => String -> String
modelName2NamespaceStore s = let
    (matches :: NonEmptyArray (Maybe String)) = fromJust $ match newModelRegex s
    (hierarchicalNamespace :: String) = fromJust $ fromJust $ index matches 1
    (namespaceParts :: Array String) = split (Pattern ".") hierarchicalNamespace
    {init:lowerParts, last:toplevel} = fromJust $ unsnoc namespaceParts
    {init:subNamespaces, last:secondLevel} = fromJust $ unsnoc lowerParts
  in
    "https://" <> secondLevel <> "." <> toplevel <> "/models_" <> intercalate "_" namespaceParts


-----------------------------------------------------------
-- PUBLIC RESOURCE IDENTIFIERS
-----------------------------------------------------------
-- | A pattern to match https://{authority}/cw_{databasename}/{SegmentedIdentifier} exactly.
-- | It is very permissive, allowing any character in the authority except the forward slash.
-- | The model name must start on an upper case alphabetic character.
publicResourcePattern :: String
publicResourcePattern = "^https://([^/]+)/(cw_[^/]+)/(.+)$"

publicResourceRegex :: Regex
publicResourceRegex = unsafeRegex publicResourcePattern noFlags

isPublicResource :: String -> Boolean
isPublicResource = test publicResourceRegex

-- | Transform a resource URI of the form 
-- |	  https://{authority}/cw_{databasename}/{SegmentedIdentifier}
-- | to:
-- |    https://{authority}/
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
publicResourceIdentifier2Authority :: String -> Maybe String
publicResourceIdentifier2Authority s = let
    (matches :: Maybe (NonEmptyArray (Maybe String))) = match publicResourceRegex s
    (hierarchicalNamespace :: (Maybe String)) = join $ join $ flip index 1 <$> matches
  in
    append "https://" <$> (append hierarchicalNamespace (Just "/"))

publicResourceIdentifier2Authority_ :: Partial => String -> String
publicResourceIdentifier2Authority_ = fromJust <<< publicResourceIdentifier2Authority


-- | Transform a resource URI of the form 
-- |	  https://{authority}/cw_{databasename}/{SegmentedIdentifier}
-- | to:
-- |    SegmentedIdentifier
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
publicResourceIdentifier2LocalName :: String -> Maybe String
publicResourceIdentifier2LocalName s = let
    (matches :: Maybe (NonEmptyArray (Maybe String))) = match publicResourceRegex s
    (hierarchicalNamespace :: (Maybe String)) = join $ join $ flip index 3 <$> matches
  in
    hierarchicalNamespace

publicResourceIdentifier2LocalName_ :: Partial => String -> String
publicResourceIdentifier2LocalName_ = fromJust <<< publicResourceIdentifier2LocalName

-- | Transform a resource URI of the form 
-- |	  https://{authority}/cw_{databasename}/{SegmentedIdentifier}
-- | to:
-- |    databasename
-- | The function is Partial because it should only be applied to a string that matches newModelPattern.
publicResourceIdentifier2database :: String -> Maybe String
publicResourceIdentifier2database s = let
    (matches :: Maybe (NonEmptyArray (Maybe String))) = match publicResourceRegex s
    (hierarchicalNamespace :: (Maybe String)) = join $ join $ flip index 2 <$> matches
  in
    hierarchicalNamespace

publicResourceIdentifier2database_ :: Partial => String -> String
publicResourceIdentifier2database_ = fromJust <<< publicResourceIdentifier2database

-- | From the identification of a resource, return the identifier of the resource in Couchdb.
couchdbResourceIdentifier :: String -> String
couchdbResourceIdentifier s = if isUrl s
  then unsafePartial publicResourceIdentifier2LocalName_ s
  else s

-----------------------------------------------------------
-- URL2AUTHORITY
-----------------------------------------------------------
urlRegex :: Regex
urlRegex = unsafeRegex "^(https://[^/]+/).*$" noFlags

url2Authority :: String -> Maybe String
url2Authority = getFirstMatch urlRegex


-----------------------------------------------------------
-- CLASS PERSPECTENTITEITIDENTIFIER
-----------------------------------------------------------
-- | Abstracts over identifiers for Perspect, used in the CRL parser. There are two instances: ModelName and QualifiedName.
class PerspectEntiteitIdentifier a where
  pe_namespace :: a -> Namespace
  pe_localName :: a -> Maybe LocalName

instance peIdentifierModelName :: PerspectEntiteitIdentifier ModelName where
  pe_namespace (ModelName ns) = ns
  pe_localName _ = Nothing

instance peIdentifierQualifiedName :: PerspectEntiteitIdentifier QualifiedName where
  pe_namespace (QualifiedName ns _) = ns
  pe_localName (QualifiedName _ ln) = Just ln

-----------------------------------------------------------
-- THROW ERROR WHEN RESULT IS NOTHING
-----------------------------------------------------------
guardWellFormedNess :: forall m. MonadThrow Error m => (String -> Maybe String) -> String -> m String
guardWellFormedNess f a = onNothing' (error $ "This identifier is not well formed: " <> a ) (f a)

-----------------------------------------------------------
-- QUALIFY A NAME
-----------------------------------------------------------
qualifyWith :: Namespace -> String -> String
qualifyWith ns = append (ns <> "$")

-----------------------------------------------------------
-- DECONSTRUCTING NAMESPACES
-----------------------------------------------------------
-- | A qualified name has the form `model://ModelName$First$Second`.
-- | In other words, it consists of
-- |  * a modelName: `model://ModelName`, followed by any number of
-- |  * segments: `$segment` (a '$' followed by word characters).
-- | Alternatively, we can split a qualifiedName in
-- |  * a namespace: everything but the last segment, and
-- |  * the localName: the last segment.

qualifiedNameRegex :: Regex
qualifiedNameRegex = unsafeRegex (modelPattern <> "\\$(.*)$") noFlags

-- | Is the identifier of the form `model://ModelName$atLeastOneSegment`?
isQualifiedName :: String -> Boolean
isQualifiedName s = test qualifiedNameRegex s

matchModelnameRegex :: Regex
matchModelnameRegex = unsafeRegex modelPattern noFlags

-- | Tests whether the string is at least of the form `model://ModelName`.
isQualifiedWithDomein :: String -> Boolean
isQualifiedWithDomein = test matchModelnameRegex

-- | Matches the entire namespace part of a qualified name (everything but the last segment).
namespaceRegEx :: Regex
namespaceRegEx = unsafeRegex "^(model:(?://)?.*)\\$\\w*" noFlags

-- | Returns the entire name but for the last segment. This is the namespace of the local name.
-- | deconstructNamespace "model:Model$First$Second" == Just "model:Model$First"
-- | deconstructNamespace "model:Model" == Just "model:Model"
deconstructNamespace :: String -> Maybe Namespace
deconstructNamespace s = case deconstructLocalName s of
  Nothing -> Just s
  (Just ln) -> stripSuffix (Pattern $ "$" <> ln) s

-- | As deconstructNamespace, but will throw a runtime error if it fails.
deconstructNamespace_ :: String -> Namespace
deconstructNamespace_ = unsafePartial $ fromJust <<< deconstructNamespace

-- | Returns the "model://authority/ModelName" part of an identifier or Nothing if it does not start with model://authority/ModelName.
-- | deconstructModelName "model://authority/Model$First$Second" == Just "model://authority/Model"
-- | deconstructModelName "model://authority/Model" == Just "model://Modelauthority/"
deconstructModelName :: String -> Maybe Namespace
deconstructModelName = getFirstMatch matchModelnameRegex

unsafeDeconstructModelName :: String -> Namespace
unsafeDeconstructModelName = unsafePartial fromJust <<< deconstructModelName

-- | Returns "Context$localName" from "model://ModelName$Context$localName" or Nothing if it does
-- | not start with model://ModelName
-- | So this function returns ALL SEGMENTS of the name, omitting just the model://ModelName part.
-- | deconstructSegments "model://ModelName$Context$localName" == Just "Context$localName"
-- | deconstructSegments "model://Model" == Just ""
deconstructSegments :: String -> Maybe String
deconstructSegments = getSecondMatch qualifiedNameRegex

lastPartRegEx :: Regex
lastPartRegEx = unsafeRegex ".*[\\$|/](\\w+)" noFlags

-- | Returns "localName" from "model:ModelName$Context$localName" and from "https://some.domain/database/localName" or Nothing
-- | So this function returns THE LAST SEGMENT of the name.
-- | deconstructLocalName "model:Model$First$Second" == Just "Second"
-- | deconstructLocalName "model:Model" == Nothing
deconstructLocalName :: String -> Maybe String
deconstructLocalName = getFirstMatch lastPartRegEx

deconstructLocalName_ :: String -> String
deconstructLocalName_ s = unsafePartial (fromJust (deconstructLocalName s))

-- | "model:Perspectives$Context" `hasLocalName` "Context"
hasLocalName :: String -> String -> Boolean
hasLocalName qn ln = case deconstructLocalName qn of
  Nothing -> false
  (Just ln') -> ln == ln'

-- | "Context" `isLocalNameOf` "model:Perspectives$Context"
isLocalNameOf :: String -> String -> Boolean
isLocalNameOf = flip hasLocalName

-----------------------------------------------------------
-- URL
-----------------------------------------------------------
urlRegEx :: Regex
urlRegEx = unsafeRegex "^http" noFlags

isUrl :: String -> Boolean
isUrl = test urlRegEx

-----------------------------------------------------------
-- URI REPRESENTING RESOURCE
-----------------------------------------------------------
-- | Returns the part of the string following the last "$".
localNameFromUriRegEx :: Regex
localNameFromUriRegEx = unsafeRegex "(.*)\\$(.*)" noFlags

deconstructAuthorityFromUri :: String -> Maybe String
deconstructAuthorityFromUri = getFirstMatch localNameFromUriRegEx

deconstructAuthorityFromUri_ :: Partial => String -> String
deconstructAuthorityFromUri_ = fromJust <<< deconstructAuthorityFromUri

deconstructLocalNameFromUri :: String -> Maybe String
deconstructLocalNameFromUri = getSecondMatch localNameFromUriRegEx

deconstructLocalNameFromUri_ :: Partial => String -> String
deconstructLocalNameFromUri_ = fromJust <<< deconstructLocalNameFromUri

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
-- THE MODEL:USER DOMEIN
-----------------------------------------------------------
-- | Matches all segments of the name (the string after the first "$")
userNameRegEx :: Regex
userNameRegEx = unsafeRegex "^model:User\\$(.*)\\$.*" noFlags

deconstructUserName :: String -> Maybe String
deconstructUserName = getFirstMatch userNameRegEx

constructUserIdentifier :: String -> String
constructUserIdentifier s = "model:User$" <> s

-----------------------------------------------------------
-- ROLNAMES
-----------------------------------------------------------
roleIndexNrRegex :: Regex
roleIndexNrRegex = unsafeRegex "_(\\d+)$" noFlags

-- | Role names are postfixed with an index to distinghuish between multiple occurrences of the same role type.
roleIndexNr :: String -> Maybe String
roleIndexNr s = case match roleIndexNrRegex s of
  (Just (matches :: NonEmptyArray (Maybe String))) -> maybe Nothing identity (index matches 1)
  _ -> Nothing

-----------------------------------------------------------
-- ESCAPING FOR RETRIEVAL FROM COUCHDB
--  Couchdb accepts documentnames with ":" en "$". But to retrieve them through http, these
--  characters have to be escaped.
-----------------------------------------------------------

escapeCouchdbDocumentName :: String -> String
escapeCouchdbDocumentName s = replaceAll (Pattern ":") (Replacement "%3A") (replaceAll (Pattern "$") (Replacement "%24") s)

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
