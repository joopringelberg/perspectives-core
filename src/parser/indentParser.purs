module Perspectives.IndentParser where
  -- type Parser s = ParserT s Identity

import Control.Monad.State (StateT, evalStateT, runStateT)
import Control.Monad.State.Trans (gets, modify, put, get)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap, empty, fromFoldable, insert, lookup)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile, addContextToDomeinFile, addRolToDomeinFile, defaultDomeinFile)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.Identifiers (Prefix, QualifiedName(..))
import Perspectives.PerspectivesTypesInPurescript (RolDef(..))
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (Unit, bind, discard, pure, unit, (+), (<<<), (<>), (>>=), ($))
import Text.Parsing.Indent (runIndent)
import Text.Parsing.Parser (ParseError, ParserT, runParserT)
import Text.Parsing.Parser.Pos (Position)

-- | A type to keep track of:
-- | - the number of occurrences of a role type, and
-- | - the namespace for context declarations. The namespace does not terminate on a $!
-- | - the section, i.e. the current role that should be used to stick a role in a context;
-- | - the prefixes: a map of prefixes to namespaces.
type ContextRoleParserState =
  { rolOccurrences :: StrMap Int
  , namespace :: String
  , typeNamespace :: String
  , section :: QualifiedName
  , prefixes :: StrMap String
  , domeinFile :: DomeinFile}

defaultPrefixes :: StrMap String
defaultPrefixes = fromFoldable [Tuple "psp:" "model:Perspectives", Tuple "usr:" "model:User"]

initialContextRoleParserMonadState :: ContextRoleParserState
initialContextRoleParserMonadState =
  { rolOccurrences: empty
  , namespace: "model:Perspectives"
  , typeNamespace: "model:Perspectives"
  , section: (QualifiedName "model:Perspectives" "rolInContext")
  , prefixes: defaultPrefixes
  , domeinFile: defaultDomeinFile
}

-- | This is the monad stack we use for the ContextRoleParser.
-- | The underlying monad is MonadPerspectives, which we need to access couchdb.
-- | Then we have ContextRoleParserState, which we need to keep track of the number of
-- | instances of a particular role type in a context (to generate unique names).
-- | Finally, the StateT Position part is used by the IndentParser.
type ContextRoleParserMonad e = (StateT Position (StateT ContextRoleParserState (MonadPerspectives e)))

-- | This is the type that is produced by the ContextRoleParser.
-- | It can also be expressed in terms of the type synonym IndentParser:
-- | type IP a e = IndentParser (StateT ContextRoleParserState (Aff e)) String a
type IP a e = ParserT String (ContextRoleParserMonad e) a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a e.
  String -> IP a e -> MonadPerspectives e (Either ParseError a)
runIndentParser s p = evalStateT (runIndent (runParserT s p)) initialContextRoleParserMonadState

-- | Apply a parser, keeping both state and the parsed result.
runIndentParser' :: forall a e.
  String -> IP a e -> MonadPerspectives e (Tuple (Either ParseError a) ContextRoleParserState)
runIndentParser' s p = runStateT (runIndent (runParserT s p)) initialContextRoleParserMonadState

-- | As convienience, to lift functions on Aff all the way up:
liftAffToIP :: forall a e. MonadPerspectives e a -> IP a e
liftAffToIP = lift <<< lift <<< lift

-----------------------------------------------------------
-- RoleOccurrences
-----------------------------------------------------------
-- | Reach all the way into the stack to retrieve the number of times a particular
-- | role has been instantiated in a context:
getRoleOccurrences :: forall e. RolDef -> IP (Maybe Int) e
getRoleOccurrences (RolDef roleName) = do
  lift (lift (gets (\{rolOccurrences} -> lookup roleName rolOccurrences)))

-- | Increment the number of instances of a particular role.
incrementRoleInstances :: forall e. RolName -> IP Unit e
incrementRoleInstances roleName = lift (lift (modify f))
  where
    f s@{rolOccurrences} = s {rolOccurrences = insert roleName (maybe 1 ((+)1) (lookup roleName rolOccurrences)) rolOccurrences}

getRoleInstances :: forall e. IP (StrMap Int) e
getRoleInstances = lift (lift get) >>= pure <<< (_.rolOccurrences)

setRoleInstances :: forall e. (StrMap Int) -> IP Unit e
setRoleInstances rmap = do
  s <- lift (lift get)
  lift (lift (put s {rolOccurrences = rmap}))

-----------------------------------------------------------
-- Namespace
-----------------------------------------------------------
getNamespace :: forall e. IP String e
getNamespace = lift (lift get) >>= pure <<< (_.namespace)

setNamespace :: forall e. String -> IP Unit e
setNamespace ns = do
  s <- lift (lift get)
  lift (lift (put s {namespace = ns}))

-- Note that the namespace in state is not terminated by a $.
withExtendedNamespace :: forall a e. String -> IP a e -> IP a e
withExtendedNamespace extension p = do
  namespace <- getNamespace
  case null extension of
    false -> setNamespace (namespace <> "$" <> extension)
    otherwise -> pure unit
  -- _ <- setNamespace (namespace <> "$" <> extension)
  result <- p
  _ <- setNamespace namespace
  pure result

-- Note that the namespace in state is not terminated by a $.
withNamespace :: forall a e. String -> IP a e -> IP a e
withNamespace ns p = do
  namespace <- getNamespace
  _ <- setNamespace ns
  result <- p
  _ <- setNamespace namespace
  pure result

-----------------------------------------------------------
-- TypeNamespace
-----------------------------------------------------------
getTypeNamespace :: forall e. IP String e
getTypeNamespace = lift (lift get) >>= pure <<< (_.typeNamespace)

setTypeNamespace :: forall e. String -> IP Unit e
setTypeNamespace ns = do
  s <- lift (lift get)
  lift (lift (put s {typeNamespace = ns}))

-- Note that the namespace in state is not terminated by a $.
withExtendedTypeNamespace :: forall a e. String -> IP a e -> IP a e
withExtendedTypeNamespace extension p = do
  namespace <- getTypeNamespace
  case null extension of
    false -> setTypeNamespace (namespace <> "$" <> extension)
    otherwise -> pure unit
  -- _ <- setNamespace (namespace <> "$" <> extension)
  result <- p
  _ <- setTypeNamespace namespace
  pure result

-- Note that the namespace in state is not terminated by a $.
withTypeNamespace :: forall a e. String -> IP a e -> IP a e
withTypeNamespace ns p = do
  namespace <- getTypeNamespace
  _ <- setTypeNamespace ns
  result <- p
  _ <- setTypeNamespace namespace
  pure result

-----------------------------------------------------------
-- Section
-----------------------------------------------------------
setSection :: forall e. QualifiedName -> IP Unit e
setSection propertyName = do
  s <- lift (lift get)
  lift (lift (put s {section = propertyName}))

getSection :: forall e. IP QualifiedName e
getSection = lift (lift get) >>= pure <<< (_.section)

-----------------------------------------------------------
-- Prefixes
-----------------------------------------------------------
setPrefix :: forall e. Prefix -> String -> IP Unit e
setPrefix pre exp = do
  (s@{prefixes}) <- lift (lift get)
  lift (lift (put s {prefixes = insert pre exp prefixes}))

getPrefix :: forall e. Prefix -> IP (Maybe String) e
getPrefix pre = lift (lift (gets (\{prefixes} -> lookup pre prefixes)))

-----------------------------------------------------------
-- Domeinfile
-----------------------------------------------------------
addRol :: forall r b e. Newtype r String => Newtype b String => PerspectRol r b -> IP Unit e
addRol rol = lift $ lift $ modify \s@{domeinFile} -> s {domeinFile = addRolToDomeinFile rol domeinFile }

addContext :: forall c e. Newtype c String => PerspectContext c -> IP Unit e
addContext ctxt = lift (lift (modify \s@{domeinFile} -> s {domeinFile = addContextToDomeinFile ctxt domeinFile }))
