-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.IndentParser where
  -- type Parser s = ParserT s Identity

import Control.Monad.State (StateT, evalStateT, runStateT)
import Control.Monad.State.Trans (gets, modify, put, get)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, fromFoldable, insert, lookup) as F
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.Identifiers (Prefix, QualifiedName(..))
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (+), (<<<), (<>), (>>=))
import Text.Parsing.Indent (runIndent)
import Text.Parsing.Parser (ParseError, ParserT, runParserT)
import Text.Parsing.Parser.Pos (Position)

-- | A type to keep track of:
-- | - the number of occurrences of a role type, and
-- | - the namespace for context declarations. The namespace does not terminate on a $!
-- | - the section, i.e. the current role that should be used to stick a role in a context;
-- | - the prefixes: a map of prefixes to namespaces.
type ContextRoleParserState =
  { rolOccurrences :: F.Object Int
  , namespace :: String
  , typeNamespace :: String
  , section :: QualifiedName
  , prefixes :: F.Object String
  , nameCounter :: Int
  , contextInstances :: F.Object PerspectContext
  , roleInstances :: F.Object PerspectRol
}

defaultPrefixes :: F.Object String
defaultPrefixes = F.fromFoldable [Tuple "psp:" "model:Perspectives", Tuple "usr:" "model:User"]

initialContextRoleParserMonadState :: ContextRoleParserState
initialContextRoleParserMonadState =
  { rolOccurrences: F.empty
  , namespace: "model:Perspectives"
  , typeNamespace: "model:Perspectives"
  , section: (QualifiedName "model:Perspectives" "rolInContext")
  , prefixes: defaultPrefixes
  , nameCounter: 0
  , contextInstances: F.empty
  , roleInstances: F.empty
}

-- | This is the monad stack we use for the ContextRoleParser.
-- | The underlying monad is MonadPerspectives, which we need to access couchdb.
-- | Then we have ContextRoleParserState, which we need to keep track of the number of
-- | instances of a particular role type in a context (to generate unique names).
-- | Finally, the StateT Position part is used by the IndentParser.
type ContextRoleParserMonad = (StateT Position (StateT ContextRoleParserState (MonadPerspectives)))

-- | This is the type that is produced by the ContextRoleParser.
-- | It can also be expressed in terms of the type synonym IndentParser:
-- | type IP a = IndentParser (StateT ContextRoleParserState (Aff e)) String a
type IP a = ParserT String ContextRoleParserMonad a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a. String -> IP a -> MonadPerspectives (Either ParseError a)
runIndentParser s p = evalStateT (runIndent (runParserT s p)) initialContextRoleParserMonadState

-- | Apply a parser, keeping both state and the parsed result.
runIndentParser' :: forall a. String -> IP a -> MonadPerspectives (Tuple (Either ParseError a) ContextRoleParserState)
runIndentParser' s p = runStateT (runIndent (runParserT s p)) initialContextRoleParserMonadState

-- | As convienience, to lift functions on Aff all the way up:
liftAffToIP :: forall a. MonadPerspectives a -> IP a
liftAffToIP = lift <<< lift <<< lift

-----------------------------------------------------------
-- RoleOccurrences
-----------------------------------------------------------
-- | Reach all the way into the stack to retrieve the number of times a particular
-- | role has been instantiated in a context:
getRoleOccurrences :: RolName -> IP (Maybe Int)
getRoleOccurrences roleName = do
  lift (lift (gets (\{rolOccurrences} -> F.lookup roleName rolOccurrences)))

-- | Increment the number of instances of a particular role.
incrementRoleInstances :: RolName -> IP Unit
incrementRoleInstances roleName = void $ lift (lift (modify f))
  where
    f s@{rolOccurrences} = s {rolOccurrences = F.insert roleName (maybe 1 ((+)1) (F.lookup roleName rolOccurrences)) rolOccurrences}

getAllRoleOccurrences :: IP (F.Object Int)
getAllRoleOccurrences = lift (lift get) >>= pure <<< (_.rolOccurrences)

setRoleOccurrences :: (F.Object Int) -> IP Unit
setRoleOccurrences rmap = do
  s <- lift (lift get)
  lift (lift (put s {rolOccurrences = rmap}))

-----------------------------------------------------------
-- Namespace
-----------------------------------------------------------
getNamespace :: IP String
getNamespace = lift (lift get) >>= pure <<< (_.namespace)

generatedNameCounter :: IP Int
generatedNameCounter = do
  s <- lift (lift get)
  n <- pure s.nameCounter
  lift (lift (put s {nameCounter = n + 1}))
  pure n

setNamespace :: String -> IP Unit
setNamespace ns = do
  s <- lift (lift get)
  lift (lift (put s {namespace = ns}))

-- Note that the namespace in state is not terminated by a $.
withExtendedNamespace :: forall a. String -> IP a -> IP a
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
withNamespace :: forall a. String -> IP a -> IP a
withNamespace ns p = do
  namespace <- getNamespace
  _ <- setNamespace ns
  result <- p
  _ <- setNamespace namespace
  pure result

-----------------------------------------------------------
-- TypeNamespace
-----------------------------------------------------------
getTypeNamespace :: IP String
getTypeNamespace = lift (lift get) >>= pure <<< (_.typeNamespace)

setTypeNamespace :: String -> IP Unit
setTypeNamespace ns = do
  s <- lift (lift get)
  lift (lift (put s {typeNamespace = ns}))

-- Note that the namespace in state is not terminated by a $.
withExtendedTypeNamespace :: forall a. String -> IP a -> IP a
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
withTypeNamespace :: forall a. String -> IP a -> IP a
withTypeNamespace ns p = do
  namespace <- getTypeNamespace
  _ <- setTypeNamespace ns
  result <- p
  _ <- setTypeNamespace namespace
  pure result

-----------------------------------------------------------
-- Section
-----------------------------------------------------------
setSection :: QualifiedName -> IP Unit
setSection propertyName = do
  s <- lift (lift get)
  lift (lift (put s {section = propertyName}))

getSection :: IP QualifiedName
getSection = lift (lift get) >>= pure <<< (_.section)

-----------------------------------------------------------
-- Prefixes
-----------------------------------------------------------
setPrefix :: Prefix -> String -> IP Unit
setPrefix pre exp = do
  (s@{prefixes}) <- lift (lift get)
  lift (lift (put s {prefixes = F.insert pre exp prefixes}))

getPrefix :: Prefix -> IP (Maybe String)
getPrefix pre = lift (lift (gets (\{prefixes} -> F.lookup pre prefixes)))

-----------------------------------------------------------
-- ContextInstances
-----------------------------------------------------------
addContextInstance :: ContextInstance -> PerspectContext -> IP Unit
addContextInstance contextId context = void $ lift (lift (modify f))
  where
    f s@{contextInstances} = s {contextInstances = F.insert (unwrap contextId) context contextInstances}

getContextInstances :: IP (F.Object PerspectContext)
getContextInstances = lift (lift get) >>= pure <<< (_.contextInstances)

modifyContextInstance :: (PerspectContext -> PerspectContext) -> ContextInstance -> IP Unit
modifyContextInstance modifier contextId = void $ lift (lift (modify f))
  where
    f s@{contextInstances} = case F.lookup (unwrap contextId) contextInstances of
      Nothing -> s
      Just context -> s {contextInstances = F.insert (unwrap contextId) (modifier context) contextInstances}
-----------------------------------------------------------
-- RoleInstances
-----------------------------------------------------------
addRoleInstance :: RoleInstance -> PerspectRol -> IP Unit
addRoleInstance roleId role = void $ lift (lift (modify f))
  where
    f s@{roleInstances} = s {roleInstances = F.insert (unwrap roleId) role roleInstances}

getRoleInstances :: IP (F.Object PerspectRol)
getRoleInstances = lift (lift get) >>= pure <<< (_.roleInstances)

setRoleInstances :: F.Object PerspectRol -> IP Unit
setRoleInstances r = void $ lift $ lift (modify (_ {roleInstances = r}))
