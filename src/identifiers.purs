module Perspectives.Identifiers where
import Data.Array (unsafeIndex)
import Data.Foldable (or)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (const, (<$>))

standardPrefixes2namespaces :: StrMap String
standardPrefixes2namespaces = fromFoldable [ (Tuple "user" "model:user#"),
  (Tuple "blank" "model:blank#"),
  (Tuple "_" "model:blank#"),
  (Tuple "xsd" "http://www.w3.org/2001/XMLSchema#"),
  (Tuple "rdfs" "http://www.w3.org/2000/01/rdf-schema#"),
  (Tuple "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
  (Tuple "owl" "http://www.w3.org/2002/07/owl#"),
  (Tuple "xml" "http://www.w3.org/XML/1998/namespace")]

domeinURIRegex :: Regex
domeinURIRegex = unsafeRegex "^model:(\\w*)#(\\w*)$" noFlags

-- | True iff the string conforms to the model scheme, i.e. "model:#SomeDomein".
isDomeinURI :: String -> Boolean
isDomeinURI s = test domeinURIRegex s

curieRegEx :: Regex
curieRegEx = unsafeRegex "^(\\w+)\\:(\\w+)" noFlags

type Prefix = String

-- | Returns 'pre' from 'pre:someurl' or Nothing.
getPrefix :: String -> Maybe Prefix
getPrefix s = case match curieRegEx s of
  (Just matches) -> unsafePartial unsafeIndex matches 1
  _ -> Nothing

-- | True iff the string is a curie with a recognizable prefix that is one of prefixes of the standard namespaces.
isStandardNamespaceCURIE :: String -> Boolean
isStandardNamespaceCURIE s =
  case match curieRegEx s of
    Nothing -> false
    Just matches -> or (maybe false isStandardNamespacePrefix <$> matches)

isStandardNamespacePrefix :: String -> Boolean
isStandardNamespacePrefix pre = maybe false (const true) (lookup pre standardPrefixes2namespaces)
