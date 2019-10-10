module Perspectives.Identifiers

where
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array.NonEmpty (NonEmptyArray, index)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.String (Pattern(..), Replacement(..), contains, indexOf, replace, replaceAll, stripPrefix, stripSuffix)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Exception (Error, error)
import Partial.Unsafe (unsafePartial)
import Perspectives.Utilities (onNothing')
import Prelude (class Show, flip, identity, ($), (<<<), (<>), (==), (>>>), (||))

-----------------------------------------------------------
-- NAMESPACE, MODELNAME
-----------------------------------------------------------
-- | A Namespace has the form "model:Name"
modelRegex :: Regex
modelRegex = unsafeRegex "^model:(\\w*)$" noFlags

isModelName :: String -> Boolean
isModelName s = test modelRegex s

binnenRol :: String -> String
binnenRol s = if isModelName s
  then s <> "$_binnenRol"
  else s <> "_binnenRol"

-- | Returns the identifier minus the "_binnenRol" or "$_binnenRol" part.
deconstructBinnenRol :: String -> String
deconstructBinnenRol s = replaceAll (Pattern "_binnenRol") (Replacement "")(replaceAll (Pattern "$_binnenRol") (Replacement "") s)

buitenRol :: String -> String
buitenRol s = if isModelName s
  then s <> "$_buitenRol"
  else s <> "_buitenRol"

-- | Returns the identifier minus the "_buitenRol" or "$_buitenRol" part.
deconstructBuitenRol :: String -> String
deconstructBuitenRol s = replaceAll (Pattern "_buitenRol") (Replacement "")(replaceAll (Pattern "$_buitenRol") (Replacement "") s)

buitenToBinnenRol :: String -> String
buitenToBinnenRol = deconstructBuitenRol >>> binnenRol

type Namespace = String
type LocalName = String
type Prefix = String

type PEIdentifier = String

-- | Only a psp:Context can have a ModelName. In other words, if something has a ModelName, its pspType is psp:Context.
-- | However, a psp:Context may have a QualifiedName!
newtype ModelName = ModelName Namespace

instance showModelName :: Show ModelName where
  show (ModelName mn) = mn

-- | A QualifiedName consists of a namespace and a local name.
data QualifiedName = QualifiedName Namespace LocalName

instance showQualifiedName :: Show QualifiedName where
  show (QualifiedName mn ln) = mn <> "$" <> ln

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
-- DECONSTRUCTING NAMESPACES
-----------------------------------------------------------
-- | A qualified name has the form `model:ModelName$First$Second`.
-- | In other words, it consists of
-- |  * a modelName: `model:ModelName`, followed by any number of
-- |  * segments: `$segment` (a '$' followed by word characters).
-- | Alternatively, we can split a qualifiedName in
-- |  * a namespace: everything but the last segment, and
-- |  * the localName: the last segment.

domeinURIQualifiedRegex :: Regex
domeinURIQualifiedRegex = unsafeRegex "^model:(\\w*)(.*)$" noFlags

-- | Is the identifier qualified with a valid Namespace?
isQualifiedWithDomein :: String -> Boolean
isQualifiedWithDomein s = test domeinURIQualifiedRegex s

-- | Matches the entire namespace part of a qualified name (everything but the last segment).
namespaceRegEx :: Regex
namespaceRegEx = unsafeRegex "^(model:.*)\\$\\w*" noFlags

-- | Returns the entire name but for the last segment. This is the namespace of the local name.
-- | deconstructNamespace "model:Model$First$Second" == Just "model:Model$First"
-- | deconstructNamespace "model:Model" == Just "model:Model"
deconstructNamespace :: String -> Maybe Namespace
deconstructNamespace s = case deconstructLocalName s of
  Nothing -> Just s
  (Just ln) -> stripSuffix (Pattern ln) s

-- | As deconstructNamespace, but will throw a runtime error if it fails.
deconstructNamespace_ :: String -> Namespace
deconstructNamespace_ = unsafePartial $ fromJust <<< deconstructNamespace

namespaceRegex :: Regex
namespaceRegex = unsafeRegex "^(model:\\w*)" noFlags

-- | Returns the "model:ModelName" part of an identifier or Nothing if it does not start with model:ModelName.
-- | deconstructModelName "model:Model$First$Second" == Just "model:Model"
-- | deconstructModelName "model:Model" == Just "model:Model"
deconstructModelName :: String -> Maybe Namespace
deconstructModelName = getFirstMatch namespaceRegex

-- | Matches the last segment of the name (the word after the last "$")
localPartsRegEx :: Regex
localPartsRegEx = unsafeRegex "^model:\\w*(.*)$" noFlags

-- | Returns "Context$localName" from "model:ModelName$Context$localName" or Nothing if it does
-- | not start with model:ModelName
-- | So this function returns ALL SEGMENTS of the name, omitting just the model:ModelName part.
-- | deconstructSegments "model:ModelName$Context$localName" == Just "Context$localName"
-- | deconstructSegments "model:Model" == Just ""
deconstructSegments :: String -> Maybe String
deconstructSegments = getFirstMatch localPartsRegEx

lastPartRegEx :: Regex
lastPartRegEx = unsafeRegex ".*\\$(\\w+)" noFlags

-- | Returns "localName" from "model:ModelName$Context$localName" or Nothing
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
-- CURIES
-----------------------------------------------------------
curieRegEx :: Regex
curieRegEx = unsafeRegex "^(\\w+)\\:(\\w+)" noFlags

-- | Returns 'pre' from 'pre:someurl' or Nothing.
deconstructPrefix :: String -> Maybe Prefix
deconstructPrefix = getFirstMatch curieRegEx

-- | Returns "someurl" from "pre:someurl" or Nothing
deconstructLocalNameFromCurie :: String -> Maybe String
deconstructLocalNameFromCurie = getSecondMatch curieRegEx

-----------------------------------------------------------
-- THE MODEL:USER DOMEIN
-----------------------------------------------------------
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

isUserEntiteitID :: String -> Boolean
isUserEntiteitID id = isUserURI id || isUserURI id

-----------------------------------------------------------
-- BINNENROL
-----------------------------------------------------------
binnenRolRegEx :: Regex
binnenRolRegEx = unsafeRegex "binnenRol$" noFlags

-- | True iff the string ends on "binnenRol"
isBinnenRol :: String -> Boolean
isBinnenRol = test binnenRolRegEx

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
--  Couchdb accepts documentnames with ":" en "$" prima. But to retrieve them through http, these
--  characters have to be escaped.
-----------------------------------------------------------

escapeCouchdbDocumentName :: String -> String
escapeCouchdbDocumentName s = replaceAll (Pattern ":") (Replacement "%3A") (replaceAll (Pattern "$") (Replacement "%24") s)

-- | To be used for qualified names only!
-- | True iff the first argument contains the second (as its first part). E.g.:
-- | "model:Perspectives$Aangifte$Aangever" `isInNamespace` "model:Perspectives$Aangifte".
-- | "a" `isInNamespace` "a" is true, too.
isInNamespace :: String -> String -> Boolean
isInNamespace a b = a == b || (b `isContainingNamespace` a)

-- | To be used for qualified names only!
-- | True iff the first argument is the first part of the second. E.g.:
-- | "model:Perspectives$Aangifte" `isContainingNamespace` "model:Perspectives$Aangifte$Aangever".
-- | Hence, a SubNamespace is more specialized, thus longer.
isContainingNamespace :: String -> String -> Boolean
isContainingNamespace ns ident = case indexOf (Pattern ns) ident of
  (Just n) | n == 0 -> true
  otherwise -> false

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
-- EXPAND DEFAULT NAMESPACES
-----------------------------------------------------------
-- | Expand:
-- |  - psp: to model:Perspectives$,
-- |  - q: to model:QueryAst$,
-- |  - u: to model:User$.
expandDefaultNamespaces :: String -> String
expandDefaultNamespaces s = if (indexOf (Pattern "psp:") s) == Just 0 then
  replace (Pattern "psp:") (Replacement "model:Perspectives$") s
  else if (indexOf (Pattern "q:") s) == Just 0 then
    replace (Pattern "q:") (Replacement "model:QueryAst$") s
    else if (indexOf (Pattern "u:") s) == Just 0 then
      replace (Pattern "u:") (Replacement "model:User$") s
      else if (indexOf (Pattern "usr:") s) == Just 0 then
        replace (Pattern "usr:") (Replacement "model:User$") s
        else s

-----------------------------------------------------------
-- CONVENIENCE NAMESPACE PREFIX FUNCIONS
-----------------------------------------------------------
q :: String -> String
q ln = "model:QueryAst$" <> ln

psp :: String -> String
psp ln = "model:Perspectives$" <> ln
