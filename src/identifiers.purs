module Perspectives.Identifiers
( isStandardNamespaceCURIE
, isStandardNamespacePrefix
, getStandardNamespace
, deconstructPrefix
, deconstructLocalNameFromCurie
, deconstructLocalNameFromDomeinURI
, deconstructNamespace
, getFirstMatch
, getSecondMatch
, Namespace
, LocalName
, roleIndexNr
, escapeCouchdbDocumentName
, isInNamespace
, isQualifiedWithDomein
, ModelName(..)
, QualifiedName(..)
, class PerspectEntiteitIdentifier
, pe_namespace
, pe_localName
, PEIdentifier
, Prefix
, isUserURI
  )

where
import Data.Array (index, unsafeIndex)
import Data.Foldable (or)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.String (Pattern(..), Replacement(..), contains, replaceAll, stripPrefix)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, const, flip, id, not, ($), (<$>), (<>))

standardPrefixes2namespaces :: StrMap String
standardPrefixes2namespaces = fromFoldable [
  (Tuple "xsd" "http://www.w3.org/2001/XMLSchema$"),
  (Tuple "rdfs" "http://www.w3.org/2000/01/rdf-schema$"),
  (Tuple "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns$"),
  (Tuple "owl" "http://www.w3.org/2002/07/owl$"),
  (Tuple "xml" "http://www.w3.org/XML/1998/namespace")]

type Namespace = String
type LocalName = String
type Prefix = String

type PEIdentifier = String

-- | Only a psp:Context can have a ModelName. In other words, if something has a ModelName, its pspType is psp:Context.
-- | However, a psp:Context may have a QualifiedName!
newtype ModelName = ModelName Namespace

instance showModelName :: Show ModelName where
  show (ModelName mn) = mn

data QualifiedName = QualifiedName Namespace LocalName

instance showQualifiedName :: Show QualifiedName where
  show (QualifiedName mn ln) = mn <> "$" <> ln

class PerspectEntiteitIdentifier a where
  pe_namespace :: a -> Namespace
  pe_localName :: a -> Maybe LocalName

instance peIdentifierModelName :: PerspectEntiteitIdentifier ModelName where
  pe_namespace (ModelName ns) = ns
  pe_localName _ = Nothing

instance peIdentifierQualifiedName :: PerspectEntiteitIdentifier QualifiedName where
  pe_namespace (QualifiedName ns _) = ns
  pe_localName (QualifiedName _ ln) = Just ln

getFirstMatch :: Regex -> String -> Maybe String
getFirstMatch regex s = case match regex s of
  (Just matches) -> unsafePartial unsafeIndex matches 1
  _ -> Nothing

getSecondMatch :: Regex -> String -> Maybe String
getSecondMatch regex s = case match regex s of
  (Just matches) -> unsafePartial unsafeIndex matches 2
  _ -> Nothing

domeinURIQualifiedRegex :: Regex
domeinURIQualifiedRegex = unsafeRegex "^model:(\\w*)(.*)$" noFlags

isQualifiedWithDomein :: String -> Boolean
isQualifiedWithDomein s = test domeinURIQualifiedRegex s

curieRegEx :: Regex
curieRegEx = unsafeRegex "^(\\w+)\\:(\\w+)" noFlags

-- | Returns 'pre' from 'pre:someurl' or Nothing.
deconstructPrefix :: String -> Maybe Prefix
deconstructPrefix = getFirstMatch curieRegEx

-- | Returns "someurl" from "pre:someurl" or Nothing
deconstructLocalNameFromCurie :: String -> Maybe String
deconstructLocalNameFromCurie = getSecondMatch curieRegEx

-- | True iff the string is a curie with a recognizable prefix that is one of prefixes of the standard namespaces: "owl:whatever" returns true.
isStandardNamespaceCURIE :: String -> Boolean
isStandardNamespaceCURIE s =
  case match curieRegEx s of
    Nothing -> false
    Just matches -> or (maybe false isStandardNamespacePrefix <$> matches)

isStandardNamespacePrefix :: Prefix -> Boolean
isStandardNamespacePrefix pre = maybe false (const true) (lookup pre standardPrefixes2namespaces)

-- | From "owl:Thing", get "http://www.w3.org/2002/07/owl$"
getStandardNamespace :: String -> Maybe Namespace
getStandardNamespace s = maybe Nothing (flip lookup standardPrefixes2namespaces) (deconstructPrefix s)

userCurieRegEx :: Regex
userCurieRegEx = unsafeRegex "^usr:" noFlags

-- | True iff the string starts on "usr:"
isUserCurie :: String -> Boolean
isUserCurie = test userCurieRegEx

userUriRegEx :: Regex
userUriRegEx = unsafeRegex "^model:User\\$" noFlags

-- | True iff the string starts on "model:User$"
isUserURI :: String -> Boolean
isUserURI = test userUriRegEx

namespaceRegex :: Regex
namespaceRegex = unsafeRegex "^(model:\\w*)" noFlags

deconstructNamespace :: String -> Maybe Namespace
deconstructNamespace = getFirstMatch namespaceRegex

domeinURIRegex :: Regex
domeinURIRegex = unsafeRegex "^model:(\\w*)\\$(\\w*)$" noFlags

-- | Returns "someurl" from "model:ModelName$someurl" or Nothing
deconstructLocalNameFromDomeinURI :: String -> Maybe String
deconstructLocalNameFromDomeinURI = getSecondMatch domeinURIRegex

roleIndexNrRegex :: Regex
roleIndexNrRegex = unsafeRegex "_(\\d+)$" noFlags

-- | Role names are postfixed with an index to distinghuish between multiple occurrences of the same role type.
roleIndexNr :: String -> Maybe String
roleIndexNr s = case match roleIndexNrRegex s of
  (Just (matches :: Array (Maybe String))) -> maybe Nothing id (index matches 1)
  _ -> Nothing

{-
  ESCAPING FOR RETRIEVAL FROM COUCHDB
  In couchdb kun je documentnamen met een ":" en "$" prima gebruiken. But to retrieve them through http, these
  characters have to be escaped.
-}

escapeCouchdbDocumentName :: String -> String
escapeCouchdbDocumentName s = replaceAll (Pattern ":") (Replacement "%3A") (replaceAll (Pattern "$") (Replacement "%24") s)

-- | ident is in ns iff the namespace part of ident equals ns.
-- | E.g. model:Perspectives$Aangifte$Aangever is in the namespace model:Perspectives$Aangifte$.
isInNamespace :: String -> String -> Boolean
isInNamespace ns ident =
  -- A quick test: strip ns from ident. What remains may not hold a "$".
  not $ contains (Pattern "$") (maybe "$" id (stripPrefix (Pattern (ns <> "$")) ident))
