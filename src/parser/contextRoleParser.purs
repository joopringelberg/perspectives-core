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

module Perspectives.ContextRoleParser where

import Control.Alt ((<|>))
import Control.Monad.State (get, gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, many, snoc, length, fromFoldable, insert) as AR
import Data.Array (dropEnd, intercalate)
import Data.Char.Unicode (isLower)
import Data.Either (Either(..))
import Data.Foldable (elem, fold)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, fromFoldable, insert, lookup) as FO
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, rol_padOccurrence)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.EntiteitAndRDFAliases (Comment, ID, RolName, ContextID)
import Perspectives.Identifiers (ModelName(..), PEIdentifier, QualifiedName(..), buitenRol)
import Perspectives.IndentParser (IP, addContextInstance, addRoleInstance, generatedNameCounter, getNamespace, getPrefix, getRoleInstances, getRoleOccurrences, getSection, getTypeNamespace, incrementRoleInstances, liftAffToIP, runIndentParser', setNamespace, setPrefix, setRoleInstances, setSection, setTypeNamespace, withExtendedTypeNamespace, withNamespace, withTypeNamespace)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Representation.Class.Persistent (cacheEntiteitPreservingVersion)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Perspectives.Syntax (ContextDeclaration(..), EnclosingContextDeclaration(..))
import Perspectives.Token (token)
import Prelude (class Show, Unit, bind, discard, identity, pure, show, unit, ($), ($>), (*>), (+), (-), (/=), (<$>), (<*), (<*>), (<>), (==), (>), (>>=), map, (<<<))
import Text.Parsing.Indent (block, checkIndent, indented, sameLine, withPos)
import Text.Parsing.Parser (ParseState(..), fail, ParseError)
import Text.Parsing.Parser.Combinators (choice, option, optionMaybe, sepBy, try, (<?>), (<??>))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (anyChar, oneOf, string) as STRING
import Text.Parsing.Parser.String (char, satisfy, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum, upper)

-----------------------------------------------------------
-- Comments
-----------------------------------------------------------

inLineComment ::  IP (Array Comment)
inLineComment = option [] do
  sameLine
  _ <- (STRING.string "--")
  chars <- AR.many (satisfy (_ /= '\n'))
  _ <- whiteSpace
  pure [fromCharArray chars]

manyOneLineComments ::  IP (Array Comment)
manyOneLineComments = AR.many
    (fromCharArray <$> ((STRING.string "--") *> (AR.many (satisfy (_ /= '\n')) <* whiteSpace)))

-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------

reservedOp ::  String -> IP Unit
reservedOp = token.reservedOp

reserved ::  String -> IP Unit
reserved = token.reserved

identifier ::  IP String
identifier = token.identifier

lexeme :: forall a. IP a -> IP a
lexeme = token.lexeme

int ::  IP String
int = show <$> token.integer

bool ::  IP String
bool = reserved "true" $> "true" <|> reserved "false" $> "false"

string ::  IP String
string = token.stringLiteral

simpleValue ::  IP String
simpleValue = string <|> int <|> bool

identLetter ::  IP Char
identLetter = alphaNum <|> STRING.oneOf ['_', '\'']

identLetterString ::  IP String
identLetterString = f <$> identLetter <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

-- /([A-Z]\w*\b)/
-- /(\p{Uppercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+)/gu
capitalizedString ::  IP String
capitalizedString = f <$> upper <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

lower ::  IP Char
lower = satisfy isLower <?> "uppercase letter"

-- /([a-z]\w*\b)/
-- /(\b\p{Lowercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+\b)/gu
uncapitalizedString ::  IP String
uncapitalizedString = f <$> lower <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

-- domeinName = 'model:' upper alphaNum* '$'
domeinName ::  IP String
domeinName = do
  _ <- STRING.string "model:"
  domein <- capitalizedString
  pure $ "model:" <> domein

modelName ::  IP ModelName
modelName = lexeme do
  dn <- domeinName
  pure $ ModelName dn

-- segmentedName = aap$noot$mies
segmentedName ::  IP String
segmentedName = f <$> identLetterString <*> AR.many (defaultEmbedded identLetterString) where
  f first rest = fold $ AR.cons first rest

defaultEmbedded ::  IP String -> IP String
defaultEmbedded p = (<>) <$> STRING.string "$" <*> p

-- localContextName = aap | Aap
localContextName ::  IP String
localContextName = identLetterString

-- localPropertyName = lower alphaNum*
localPropertyName ::  IP String
localPropertyName = capitalizedString

-- localPropertyName = lower alphaNum*
localRolName ::  IP String
localRolName = capitalizedString

-- prefix = lower+ ':'
prefix ::  IP String
prefix = (f <$> lower <*> AR.many lower <*> char ':') where
  f fst ca c = fromCharArray $ AR.cons fst (AR.snoc ca c)

prefixedName ::  IP String -> IP QualifiedName
prefixedName localName = lexeme do
  pre <- prefix
  ln <- localName
  namespace <- getPrefix pre
  case namespace of
    Nothing -> fail $ "The prefix '" <> pre <> "' has not been declared!"
    (Just ns) -> pure $ QualifiedName ns ln

-- prefixedContextName = prefix segmentedName
prefixedContextName ::  IP QualifiedName
prefixedContextName = prefixedName segmentedName

-- prefixedPropertyName = prefix localPropertyName
prefixedPropertyName ::  IP QualifiedName
prefixedPropertyName = prefixedContextName

-- qualifiedResourceName = domeinName segmentedName
expandedName ::  IP QualifiedName
expandedName = try $ lexeme $ do
  dn <- domeinName
  _ <- STRING.string "$"
  ln <- segmentedName
  pure $ QualifiedName dn ln

-- expandedPropertyName = domeinName localPropertyName
expandedPropertyName ::  IP QualifiedName
expandedPropertyName =
  try $ lexeme $ do
    dn <- domeinName
    _ <- STRING.string "$"
    ln <- localPropertyName
    pure $ QualifiedName dn ln

contextInstanceIDInCurrentNamespace ::  IP QualifiedName
contextInstanceIDInCurrentNamespace = lexeme do
  namespace <- getNamespace
  ln <- (STRING.string "$") *> localContextName
  pure $ QualifiedName namespace ln

relativeContextTypeName ::  IP QualifiedName
relativeContextTypeName = lexeme do
  namespace <- getTypeNamespace
  ln <- (STRING.string "$") *> localContextName
  pure $ QualifiedName namespace ln

relativeRolTypeName ::  IP QualifiedName
relativeRolTypeName = lexeme do
  namespace <- getTypeNamespace
  ln <- (STRING.string "$") *> localRolName
  pure $ QualifiedName namespace ln

relativePropertyTypeName ::  IP QualifiedName
relativePropertyTypeName = lexeme do
  namespace <- getTypeNamespace
  ln <- (STRING.string "$") *> localPropertyName
  pure $ QualifiedName namespace ln

relativeRolTypeNameOutsideNamespace ::  IP QualifiedName
relativeRolTypeNameOutsideNamespace = lexeme do
  ln <- (STRING.string "?") *> capitalizedString
  pure $ QualifiedName "?" ln

contextName ::  IP QualifiedName
contextName = (expandedName <|> prefixedContextName <|> contextInstanceIDInCurrentNamespace) <?> "the name of a resource (Context or Role)."

typeContextName ::  IP QualifiedName
typeContextName = (expandedName <|> prefixedContextName <|> relativeContextTypeName) <?> "the name of a resource (Context or Role)."

perspectEntiteitIdentifier ::  IP PEIdentifier
perspectEntiteitIdentifier =
  do
    d <- modelName
    pure (show d)
  <|>
  do
    d <- contextName
    pure (show d)

-- propertyName = prefixedPropertyName | expandedPropertyName
propertyName ::  IP QualifiedName
propertyName = (expandedPropertyName <|> prefixedPropertyName <|> relativePropertyTypeName) <?> "a property or role name."

roleName ::  IP QualifiedName
roleName = (expandedPropertyName <|> prefixedPropertyName <|> relativePropertyTypeName <|> relativeRolTypeNameOutsideNamespace) <?> "a role name."

-----------------------------------------------------------
-- Datatypes
-----------------------------------------------------------
-- We need the "SimpleValueDef" 'type' because we want to specify psp:Property's range as precisely that.
dataTypes :: Array String
dataTypes = ["Number", "String", "Boolean", "Date", "SimpleValueDef"]

dataType ::  IP String
dataType = try do
  s <- identifier
  if elem s dataTypes then pure $ s else fail "one of 'Number', 'String', 'Boolean' or 'Date'."

-----------------------------------------------------------
-- Handling position
-----------------------------------------------------------

-- | @ getPosition @ returns current position
-- | should probably be added to Text.Parsing.Parser.Pos
getPosition ::  IP Position
getPosition = gets \(ParseState _ pos _) -> pos

sourceColumn :: Position -> Int
sourceColumn (Position {line: _, column: c}) = c

sourceLine :: Position -> Int
sourceLine (Position {line: l, column: _}) = l

-- | Parses only on the next line as the reference
nextLine ::  IP Unit
nextLine = do
    pos <- getPosition
    s   <- lift get
    if sourceLine s + 1 == sourceLine pos then pure unit else fail "not on next line"

-----------------------------------------------------------
-- Elementary expression types
-----------------------------------------------------------

-- | enclosingContextDeclaration = Context PerspectEntiteitIdentifier
enclosingContextDeclaration ::  IP EnclosingContextDeclaration
enclosingContextDeclaration = (do
  cname <- (reserved "Context" *> perspectEntiteitIdentifier)
  _ <- setNamespace $ cname
  _ <- setTypeNamespace $ "model:Perspectives$Context"
  prfx <- (optionMaybe (reserved "als" *> prefix <* whiteSpace))
  cmt <- inLineComment
  case prfx of
    Nothing -> pure unit
    (Just pre) -> setPrefix pre cname
  pure $ EnclosingContextDeclaration cname cmt) <?> "the context declaration: Context <name>."

-- | contextDeclaration = contextName contextName
contextDeclaration ::  IP ContextDeclaration
contextDeclaration = (ContextDeclaration <$> typeContextName <*> (anonymousContextName <|> contextName) <*> inLineComment) <?> "a type declaration, e.g. :ContextType :ContextName."
  where
    -- matches an underscore
    -- returns "c<x>" (where <x> is an integer), scoped to the current namespace.
    -- x is kept in the parserstate as a counter.
    anonymousContextName :: IP QualifiedName
    anonymousContextName = lexeme do
      namespace <- getNamespace
      reservedOp "_"
      n <- generatedNameCounter
      pure $ QualifiedName namespace ("c" <> (show n))


typedPropertyAssignment ::  IP Unit -> IP (Tuple ID (Array Value))
typedPropertyAssignment scope = go (try (withPos
  (Tuple
    <$> (scope *> (propertyName <* (sameLine *> reservedOp "=")))
    <*> (sameLine *> (simpleValue `sepBy` (STRING.string ",")) >>= pure <<< map Value))))
  where
    go x = do
      (Tuple pname value) <- x
      pure $ Tuple (show pname) (AR.fromFoldable value)

-- | publicContextPropertyAssignment = 'extern' propertyName '=' simpleValue
publicContextPropertyAssignment ::  IP (Tuple ID (Array Value))
publicContextPropertyAssignment = (typedPropertyAssignment (reserved "extern")) <?> "extern propertyname = value"

-- | privateContextPropertyAssignment = 'intern' propertyName '=' simpleValue
privateContextPropertyAssignment ::  IP (Tuple ID (Array Value))
privateContextPropertyAssignment = (typedPropertyAssignment (reserved "intern")) <?> "intern propertyname = value"

-- | rolePropertyAssignment = propertyName '=' simpleValue
rolePropertyAssignment ::  IP (Tuple ID (Array Value))
rolePropertyAssignment = (typedPropertyAssignment (pure unit)) <?> "intern propertyname = value"

isRoleDeclaration ::  IP Unit
isRoleDeclaration = withPos (roleName *> (sameLine *> optionMaybe roleOccurrence) *> (sameLine *> reservedOp "=>") *> pure unit)

roleOccurrence ::  IP Int
roleOccurrence = token.parens token.integer

data Arrow = ContextBinding | RoleBinding
instance showArrow :: Show Arrow where
  show ContextBinding = "=>"
  show RoleBinding = "->"

roleBinding' ::
  QualifiedName ->
  Arrow ->
  IP (Tuple (Array Comment) (Maybe RoleInstance)) ->
  IP (Tuple RolName RoleInstance)
roleBinding' cname arrow p = ("rolename => contextName" <??>
  (try do
    -- Parsing
    cmtBefore <- manyOneLineComments
    withPos do
      rname@(QualifiedName _ localRoleName) <- roleName
      occurrence <- sameLine *> optionMaybe roleOccurrence -- The sequence number in text
      _ <- (sameLine *> reservedOp (show arrow))
      (Tuple cmt bindng) <- p
      props <- withExtendedTypeNamespace localRoleName $
            option Nil (indented *> (block (checkIndent *> rolePropertyAssignment)))
      _ <- incrementRoleInstances (show rname)

      -- Naming
      nrOfRoleOccurrences <- getRoleOccurrences (show rname) -- The position in the sequence.
      rolId <- pure $ RoleInstance ((show cname) <> "$" <> localRoleName <> "_" <> (rol_padOccurrence (roleIndex occurrence nrOfRoleOccurrences)))

      -- Storing
      cacheRol rolId
        (PerspectRol defaultRolRecord
          { _id = rolId
          , occurrence = (roleIndex occurrence nrOfRoleOccurrences)
          , pspType = EnumeratedRoleType $ show rname
          , binding = bindng
          , context = ContextInstance $ show cname
          , properties = FO.fromFoldable ((\(Tuple en cm) -> Tuple en cm) <$> props)
          })

      pure $ Tuple (show rname) rolId))
  where
    -- If there is an index in the text, it prevails.
    roleIndex :: Maybe Int -> Maybe Int -> Int
    roleIndex nrInText nrInSequence = case nrInText of
      (Just n) -> n
      Nothing -> case nrInSequence of
        (Just n) -> n
        Nothing -> 0

cacheRol ::  RoleInstance -> PerspectRol -> IP Unit
cacheRol rolId rol = do
  liftAffToIP $ cacheEntiteitPreservingVersion rolId rol
  addRoleInstance rolId rol

cacheContext ::  ContextInstance -> PerspectContext -> IP Unit
cacheContext contextId ctxt = do
  liftAffToIP $ cacheEntiteitPreservingVersion contextId ctxt
  addContextInstance contextId ctxt

-- | The inline context may itself use a contextInstanceIDInCurrentNamespace to identify the context instance. However,
-- | what is returned from the context parser is the QualifiedName of its buitenRol.
inlineContextBinding ::  QualifiedName
  -> IP (Tuple RolName RoleInstance)
inlineContextBinding cName = roleBinding' cName ContextBinding do
  cmt <- inLineComment
  _ <- nextLine
  (contextBuitenRol :: RoleInstance) <- indented *> context
  pure $ Tuple cmt (Just contextBuitenRol)

emptyBinding ::  QualifiedName
  -> IP (Tuple RolName RoleInstance)
emptyBinding cName = roleBinding' cName ContextBinding do
  _ <- token.parens whiteSpace
  cmt <- inLineComment
  pure $ Tuple cmt Nothing

contextBindingByReference ::  QualifiedName
  -> IP (Tuple RolName RoleInstance)
contextBindingByReference cName = roleBinding' cName ContextBinding do
  ident <- (sameLine *> contextReference)
  cmt <- inLineComment
  pure $ Tuple cmt (Just $ RoleInstance ident)
  where
    contextReference :: IP RolName
    contextReference = do
      qn <- (expandedName <|> prefixedContextName <|> relativeInstanceID)
      pure $ buitenRol (show qn)

roleBindingByReference ::  QualifiedName
  -> IP (Tuple RolName RoleInstance)
roleBindingByReference cName = roleBinding' cName RoleBinding do
  ident <- (sameLine *> relativeRolInstanceID <|> rolReference)
  occurrence <- sameLine *> optionMaybe roleOccurrence -- The sequence number in text
  cmt <- inLineComment
  pure $ Tuple cmt (Just $ RoleInstance (ident <> "_" <> (rol_padOccurrence (maybe 1 identity occurrence))))
  where
    rolReference :: IP RolName
    rolReference = do
      qn <- (expandedName <|> prefixedContextName <|> relativeInstanceID)
      pure (show qn)

    relativeRolInstanceID :: IP RolName
    relativeRolInstanceID = try do
      qn <- rolInHigherContext
      -- i <- roleOccurrence
      pure $ show qn

    rolInHigherContext :: IP QualifiedName
    rolInHigherContext = lexeme do
      namespace <- getNamespace -- not $-terminated!
      namespaceLevels <- AR.length <$> AR.many (STRING.string "$")
      localName <- segmentedName
      namespace' <- (butLastNNamespaceLevels namespace (namespaceLevels - 1))
      pure $ QualifiedName namespace' localName

relativeInstanceID ::  IP QualifiedName
relativeInstanceID = lexeme do
  namespace <- getNamespace -- not $-terminated!
  namespaceLevels <- AR.length <$> AR.many (STRING.string "$")
  sName <- segmentedName
  namespace' <- (butLastNNamespaceLevels namespace (namespaceLevels - 1))
  pure $ QualifiedName namespace' sName

-- Returns a string that is NOT terminated on a "$".
-- NOTE: in order to be able to fail, we need do this in IP.
butLastNNamespaceLevels ::  String -> Int -> IP String
butLastNNamespaceLevels _ -1 = fail "local name starting with '$'."
butLastNNamespaceLevels ns 0 = pure ns
butLastNNamespaceLevels ns n = do
  segments <- pure (split (Pattern "$") ns)
  if (n - 1) > (AR.length segments)
    then fail "too many levels up"
    else pure $ (intercalate "$" (dropEnd n segments))

-- | roleBinding = roleName '=>' (contextName | context) rolePropertyAssignment*
roleBinding ::  QualifiedName
  -> IP (Tuple RolName RoleInstance)
roleBinding cname =
  inlineContextBinding cname
  <|> contextBindingByReference cname
  <|> emptyBinding cname
  <|> roleBindingByReference cname

withRoleCounting :: forall a. IP a -> IP a
withRoleCounting p = do
  roleInstances <- getRoleInstances
  setRoleInstances FO.empty
  r <- p
  setRoleInstances roleInstances
  pure r

-----------------------------------------------------------
-- Context
-----------------------------------------------------------
-- | context = contextDeclaration
-- | publicContextPropertyAssignment*
-- | privateContextPropertyAssignment*
-- | roleBinding*
-- | The parser never backtracks over a Context. This means we can safely perform the side
-- | effect of storing its constituent roles and contexts.
-- | NOTE: we do not construct an inverse binding from an eventual prototype.
context ::  IP RoleInstance
context = withRoleCounting context' where

  context' :: IP RoleInstance
  context' = do
    -- Parsing
    cmtBefore <- manyOneLineComments
    withPos do
      (ContextDeclaration typeName instanceName@(QualifiedName dname localName) cmt) <- contextDeclaration

      -- Naming
      withNamespace (show instanceName) $
        withTypeNamespace (show typeName)
          do
            -- Parsing the body
            (prototype :: Maybe ContextID) <- option Nothing (indented *> prototypeDeclaration)
            (publicProps :: List (Tuple ID (Array Value))) <- option Nil (indented *> withExtendedTypeNamespace "External" (block publicContextPropertyAssignment))
            (rolebindings :: List (Tuple RolName RoleInstance)) <- option Nil (indented *> (block $ roleBinding instanceName))

            -- Storing
            cacheContext (ContextInstance (show instanceName))
              (PerspectContext defaultContextRecord
                { _id = (ContextInstance $ show instanceName)
                , displayName  = localName
                , pspType = ContextType $ show typeName
                , buitenRol = RoleInstance $ buitenRol (show instanceName)
                , rolInContext = collect rolebindings
              })
            cacheRol (RoleInstance $ buitenRol (show instanceName))
              (PerspectRol defaultRolRecord
                { _id = RoleInstance $ buitenRol (show instanceName)
                , pspType = EnumeratedRoleType $ show typeName <> "$External"
                , context = (ContextInstance $ show instanceName)
                , binding = Just $ RoleInstance $ maybe "" buitenRol prototype
                , properties = FO.fromFoldable publicProps
                })
            pure $ RoleInstance $ buitenRol (show instanceName)
  collect :: List (Tuple RolName RoleInstance) -> FO.Object (Array RoleInstance)
  collect Nil = FO.empty
  collect (Cons (Tuple rname id) r) = let map = collect r in
    case FO.lookup rname map of
      Nothing -> FO.insert rname [id] map
      (Just ids) -> FO.insert rname (AR.insert id ids) map

  prototypeDeclaration :: IP (Maybe ID)
  prototypeDeclaration = do
    prototype <- reserved "prototype" *> contextName
    pure $ Just $ show prototype

-- Helper functions for development.
allTheRest ::  IP String
allTheRest = fromCharArray <$> (AR.many STRING.anyChar)

-----------------------------------------------------------
-- Expression
-----------------------------------------------------------

expression ::  IP String
expression = choice
  [ try (enclosingContextDeclaration *> (pure "enclosingContextDeclaration"))
  , try (importExpression *> (pure "importExpression"))
  , try (sectionHeading *> (pure "sectionHeading") )
  , try (contextDeclaration *> (pure "contextDeclaration"))
  , try (publicContextPropertyAssignment *> (pure "publicContextPropertyAssignment"))
  , try (privateContextPropertyAssignment *> (pure "privateContextPropertyAssignment"))
  , try (rolePropertyAssignment *> (pure "rolePropertyAssignment"))
  , try (isRoleDeclaration *> (pure "isRoleDeclaration" ))
  , try ((STRING.string "--") *> (pure "oneLineComment"))
  -- query
  ]

-----------------------------------------------------------
-- Section and definition
-----------------------------------------------------------
section ::  IP (Tuple String (Array RoleInstance))
section = do
  prop <- sectionHeading
  ids <- AR.many definition
  pure $ Tuple prop ids

sectionHeading ::  IP ID
sectionHeading = do
  prop <- reserved "Section" *> propertyName
  setSection prop
  pure $ show prop

definition ::  IP RoleInstance
definition = do
  bindng <- context
  prop@(QualifiedName _ localName) <- getSection
  _ <- incrementRoleInstances (show prop)
  nrOfRoleOccurrences <- getRoleOccurrences (show prop)
  enclContext <- getNamespace
  rolId <- pure $ RoleInstance $ enclContext <> "$" <> localName <> "_" <> rol_padOccurrence (maybe 0 identity nrOfRoleOccurrences)
  cacheRol rolId
    (PerspectRol defaultRolRecord
      { _id = rolId
      , occurrence = maybe 0 identity nrOfRoleOccurrences
      , pspType = EnumeratedRoleType (show prop)
      , binding = Just $ bindng
      , context = ContextInstance enclContext
      })
  pure rolId

-----------------------------------------------------------
-- Import
-----------------------------------------------------------
importExpression ::  IP Unit
importExpression = do
  ns@(ModelName mn) <- reserved "import" *> modelName
  mpre <- (optionMaybe (reserved "als" *> prefix <* whiteSpace))
  case mpre of
    Nothing -> pure unit
    (Just pre) -> setPrefix pre mn

-----------------------------------------------------------
-- User data
-----------------------------------------------------------
userData ::  IP (Array RoleInstance)
userData = do
  cmtBefore <- manyOneLineComments
  withPos do
    _ <- userDataDeclaration
    _ <- AR.many importExpression
    AR.many context
  where

    userDataDeclaration :: IP Unit
    userDataDeclaration = reserved "GebruikerGegevens"

-----------------------------------------------------------
-- ParseAndCache
-----------------------------------------------------------
-- catchError :: forall a. m a -> (e -> m a) -> m a

parseAndCache ::  String -> MonadPerspectives (Either ParseError (Tuple (FO.Object PerspectContext)(FO.Object PerspectRol)))
parseAndCache text = do
  (Tuple parseResult {roleInstances, contextInstances}) <- runIndentParser' text userData
  case parseResult of
    (Left e) -> pure $ Left e
    (Right r) -> pure $ Right $ Tuple contextInstances roleInstances
