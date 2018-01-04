module Perspectives.IndentParser where
  -- type Parser s = ParserT s Identity

import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Trans (gets, modify, put, get)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Maybe (Maybe, maybe)
import Data.StrMap (StrMap, empty, insert, lookup)
import Perspectives.Syntax (RoleName)
import Prelude (Unit, bind, pure, (+), (<<<), (<>), (>>=))
import Text.Parsing.Indent (runIndent)
import Text.Parsing.Parser (ParseError, ParserT, runParserT)
import Text.Parsing.Parser.Pos (Position)

-- | A type to keep track of:
-- | - the number of occurrences of a role type, and
-- | - the namespace for context declarations.
type ContextRoleParserState = { rolOccurrences :: StrMap Int, namespace :: String, section :: String}

initialContextRoleParserMonadState :: ContextRoleParserState
initialContextRoleParserMonadState = {rolOccurrences: empty, namespace: "model:Perspectives", section: ""}

-- | This is the monad stack we use for the ContextRoleParser.
-- | The underlying monad is Aff, which we need to access couchdb.
-- | Then we have ContextRoleParserState, which we need to keep track of the number of
-- | instances of a particular role type in a context (to generate unique names).
-- | Finally, the StateT Position part is used by the IndentParser.
type ContextRoleParserMonad e = (StateT Position (StateT ContextRoleParserState (Aff e)))

-- | This is the type that is produced by the ContextRoleParser.
-- | It can also be expressed in terms of the type synonym IndentParser:
-- | type IP a e = IndentParser (StateT ContextRoleParserState (Aff e)) String a
type IP a e = ParserT String (ContextRoleParserMonad e) a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a e. String -> IP a e -> Aff e (Either ParseError a)
runIndentParser s p = evalStateT (runIndent (runParserT s p)) initialContextRoleParserMonadState

-- | As convienience, to lift functions on Aff all the way up:
liftAffToIP :: forall a e. Aff e a -> IP a e
liftAffToIP = lift <<< lift <<< lift

-- | Reach all the way into the stack to retrieve the number of times a particular
-- | role has been instantiated in a context:
getRoleOccurrences :: forall e. RoleName -> IP (Maybe Int) e
getRoleOccurrences roleName = do
  g <- lift (lift (gets (\{rolOccurrences} -> lookup roleName rolOccurrences)))
  pure g

-- | Increment the number of instances of a particular role.
incrementRoleInstances :: forall e. RoleName -> IP Unit e
incrementRoleInstances roleName = lift (lift (modify f))
  where
    f s@{rolOccurrences} = s {rolOccurrences = insert roleName (maybe 1 ((+)1) (lookup roleName rolOccurrences)) rolOccurrences}

getRoleInstances :: forall e. IP (StrMap Int) e
getRoleInstances = lift (lift get) >>= pure <<< (_.rolOccurrences)

setRoleInstances :: forall e. (StrMap Int) -> IP Unit e
setRoleInstances rmap = do
  s <- lift (lift get)
  lift (lift (put s {rolOccurrences = rmap}))

getNamespace :: forall e. IP String e
getNamespace = lift (lift get) >>= pure <<< (_.namespace)

setNamespace :: forall e. String -> IP Unit e
setNamespace ns = do
  s <- lift (lift get)
  lift (lift (put s {namespace = ns}))

extendNamespace :: forall a e. String -> IP a e -> IP a e
extendNamespace extension p = do
  namespace <- getNamespace
  _ <- setNamespace (namespace <> extension)
  result <- p
  _ <- setNamespace namespace
  pure result

setSection :: forall e. String -> IP Unit e
setSection propertyName = do
  s <- lift (lift get)
  lift (lift (put s {section = propertyName}))

getSection :: forall e. IP String e
getSection = lift (lift get) >>= pure <<< (_.section)
