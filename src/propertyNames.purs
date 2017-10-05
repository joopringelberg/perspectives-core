module Perspectives.PropertyNames where

import Data.Maybe (Maybe(..), maybe)
import Data.String (take, toUpper, drop, toLower)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Perspectives.Identifiers (getFirstMatch, getLocalNameFromCurie, getLocalNameFromURI, getNamespace, getPrefix, getSecondMatch, isStandardNamespaceCURIE)
import Prelude (id, ($), (<>))

{-
This is the algorithm:
a. Standard namespaces such as rdf, rdfs and owl: prefix the local part with "inverse_";
b. Model namespaces: if the name conforms to the domain_range rule,
  - reverse those parts;
  - otherwise, prefix the local part with "inverse_";
c. If the above two cases did not succeed, just prefix the name with "inverse_"

-}

getInversePropertyName :: String -> String
getInversePropertyName pn =
  case invertIdentifierFromStandardNamespace pn of
    (Just n) -> n
    Nothing -> case invertDomainAndRange pn of
      (Just n) -> n
      Nothing -> "inverse_" <> pn

capitalizeWord :: String -> String
capitalizeWord word = (toUpper $ take 1 word) <> drop 1 word

decapitalizeWord :: String -> String
decapitalizeWord = toLower

invertIdentifierFromStandardNamespace :: String -> Maybe String
invertIdentifierFromStandardNamespace s = if isStandardNamespaceCURIE s then Just (((maybe "" id) (getPrefix s)) <> ":inverse_" <> (maybe "" id)  (getLocalNameFromCurie s)) else Nothing

domainRangeRuleRegex :: Regex
domainRangeRuleRegex = unsafeRegex "(.+?)\\_(.+)" noFlags

conformsToDomainRangeRule :: String -> Boolean
conformsToDomainRangeRule s = test domainRangeRuleRegex s

-- | Note that String cannot be a curie!
invertDomainAndRange :: String -> Maybe String
invertDomainAndRange pn =
    let
      namespace = getNamespace pn
      in case getLocalNameFromURI pn of
        (Just ln) ->
          if conformsToDomainRangeRule ln
            then let
              part1 = decapitalizeWord ((maybe "" id) (getSecondMatch domainRangeRuleRegex ln))
              part2 = capitalizeWord ((maybe "" id) (getFirstMatch domainRangeRuleRegex ln))
              in
                Just $ (maybe "" id) namespace <> part1 <> "_" <> part2
            else Just $ (maybe "" id) namespace <> "inverse_" <> ln
        Nothing -> Nothing
