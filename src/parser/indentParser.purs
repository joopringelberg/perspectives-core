module Perspectives.IndentParser where
  -- type Parser s = ParserT s Identity

import Control.Monad.State (StateT, evalStateT, runStateT)
import Control.Monad.State.Trans (gets, modify, put, get)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Maybe (Maybe, maybe)
import Data.StrMap (StrMap, empty, fromFoldable, insert, lookup)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile, addContextToDomeinFile, addRolToDomeinFile, defaultDomeinFile)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.Identifiers (Prefix, QualifiedName(..))
import Perspectives.PerspectivesTypesInPurescript (class Binding, class ContextType, class RolType, RolDef(..))
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
type ContextRoleParserState c r b =
  { rolOccurrences :: StrMap Int
  , namespace :: String
  , typeNamespace :: String
  , section :: QualifiedName
  , prefixes :: StrMap String
  , domeinFile :: DomeinFile c r b}

defaultPrefixes :: StrMap String
defaultPrefixes = fromFoldable [Tuple "psp:" "model:Perspectives", Tuple "usr:" "model:User"]

initialContextRoleParserMonadState :: forall c r b. ContextType c => RolType r => Binding b => ContextRoleParserState c r b
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
type ContextRoleParserMonad s p o c r b e = (StateT Position (StateT (ContextRoleParserState c r b) (MonadPerspectives s p o c r b e)))

-- | This is the type that is produced by the ContextRoleParser.
-- | It can also be expressed in terms of the type synonym IndentParser:
-- | type IP a e = IndentParser (StateT ContextRoleParserState (Aff e)) String a
type IP a s p o c r b e = ParserT String (ContextRoleParserMonad s p o c r b e) a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a s p o c r b e.
  ContextType c =>
  RolType r =>
  Binding b =>
  String -> IP a s p o c r b e -> MonadPerspectives s p o c r b e (Either ParseError a)
runIndentParser s p = evalStateT (runIndent (runParserT s p)) initialContextRoleParserMonadState

-- | Apply a parser, keeping both state and the parsed result.
runIndentParser' :: forall a s p o c r b e.
  ContextType c =>
  RolType r =>
  Binding b =>
  String -> IP a s p o c r b e -> MonadPerspectives s p o c r b e (Tuple (Either ParseError a) (ContextRoleParserState c r b))
runIndentParser' s p = runStateT (runIndent (runParserT s p)) initialContextRoleParserMonadState

-- | As convienience, to lift functions on Aff all the way up:
liftAffToIP :: forall a s p o c r b e. MonadPerspectives s p o c r b e a -> IP a s p o c r b e
liftAffToIP = lift <<< lift <<< lift

-----------------------------------------------------------
-- RoleOccurrences
-----------------------------------------------------------
-- | Reach all the way into the stack to retrieve the number of times a particular
-- | role has been instantiated in a context:
getRoleOccurrences :: forall s p o c r b e. RolDef -> IP (Maybe Int) s p o c r b e
getRoleOccurrences (RolDef roleName) = do
  lift (lift (gets (\{rolOccurrences} -> lookup roleName rolOccurrences)))

-- | Increment the number of instances of a particular role.
incrementRoleInstances :: forall s p o c r b e. RolName -> IP Unit s p o c r b e
incrementRoleInstances roleName = lift (lift (modify f))
  where
    f s@{rolOccurrences} = s {rolOccurrences = insert roleName (maybe 1 ((+)1) (lookup roleName rolOccurrences)) rolOccurrences}

getRoleInstances :: forall s p o c r b e. IP (StrMap Int) s p o c r b e
getRoleInstances = lift (lift get) >>= pure <<< (_.rolOccurrences)

setRoleInstances :: forall s p o c r b e. (StrMap Int) -> IP Unit s p o c r b e
setRoleInstances rmap = do
  s <- lift (lift get)
  lift (lift (put s {rolOccurrences = rmap}))

-----------------------------------------------------------
-- Namespace
-----------------------------------------------------------
getNamespace :: forall s p o c r b e. IP String s p o c r b e
getNamespace = lift (lift get) >>= pure <<< (_.namespace)

setNamespace :: forall s p o c r b e. String -> IP Unit s p o c r b e
setNamespace ns = do
  s <- lift (lift get)
  lift (lift (put s {namespace = ns}))

-- Note that the namespace in state is not terminated by a $.
withExtendedNamespace :: forall a s p o c r b e. String -> IP a s p o c r b e -> IP a s p o c r b e
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
withNamespace :: forall a s p o c r b e. String -> IP a s p o c r b e -> IP a s p o c r b e
withNamespace ns p = do
  namespace <- getNamespace
  _ <- setNamespace ns
  result <- p
  _ <- setNamespace namespace
  pure result

-----------------------------------------------------------
-- TypeNamespace
-----------------------------------------------------------
getTypeNamespace :: forall s p o c r b e. IP String s p o c r b e
getTypeNamespace = lift (lift get) >>= pure <<< (_.typeNamespace)

setTypeNamespace :: forall s p o c r b e. String -> IP Unit s p o c r b e
setTypeNamespace ns = do
  s <- lift (lift get)
  lift (lift (put s {typeNamespace = ns}))

-- Note that the namespace in state is not terminated by a $.
withExtendedTypeNamespace :: forall a s p o c r b e. String -> IP a s p o c r b e -> IP a s p o c r b e
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
withTypeNamespace :: forall a s p o c r b e. String -> IP a s p o c r b e -> IP a s p o c r b e
withTypeNamespace ns p = do
  namespace <- getTypeNamespace
  _ <- setTypeNamespace ns
  result <- p
  _ <- setTypeNamespace namespace
  pure result

-----------------------------------------------------------
-- Section
-----------------------------------------------------------
setSection :: forall s p o c r b e. QualifiedName -> IP Unit s p o c r b e
setSection propertyName = do
  s <- lift (lift get)
  lift (lift (put s {section = propertyName}))

getSection :: forall s p o c r b e. IP QualifiedName s p o c r b e
getSection = lift (lift get) >>= pure <<< (_.section)

-----------------------------------------------------------
-- Prefixes
-----------------------------------------------------------
setPrefix :: forall s p o c r b e. Prefix -> String -> IP Unit s p o c r b e
setPrefix pre exp = do
  (s@{prefixes}) <- lift (lift get)
  lift (lift (put s {prefixes = insert pre exp prefixes}))

getPrefix :: forall s p o c r b e. Prefix -> IP (Maybe String) s p o c r b e
getPrefix pre = lift (lift (gets (\{prefixes} -> lookup pre prefixes)))

-----------------------------------------------------------
-- Domeinfile
-----------------------------------------------------------
addRol :: forall s p o c r b e.
  ContextType c =>
  RolType r =>
  Binding b =>
  PerspectRol r b -> IP Unit s p o c r b e
addRol rol = lift $ lift $ modify \s@{domeinFile} -> s {domeinFile = addRolToDomeinFile rol domeinFile }

addContext :: forall s p o c r b e.
  ContextType c =>
  RolType r =>
  Binding b =>
  PerspectContext c -> IP Unit s p o c r b e
addContext ctxt = lift (lift (modify \s@{domeinFile} -> s {domeinFile = addContextToDomeinFile ctxt domeinFile }))
