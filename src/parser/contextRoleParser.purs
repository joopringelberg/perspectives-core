module Perspectives.ContextRoleParser where

import Perspectives.EntiteitAndRDFAliases

import Control.Alt (void, (<|>))
import Effect.Aff.AVar (AVar, put, take)
import Effect.Exception (error)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (get, gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, many, snoc, length, fromFoldable) as AR
import Data.Array (dropEnd, head, intercalate)
import Data.Char.Unicode (isLower)
import Data.Either (Either(..))
import Data.Foldable (elem, fold, for_)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Foreign.Object (Object, empty, fromFoldable, insert, lookup, values) as FO
import Data.String (Pattern(..), drop, split)
import Data.String.CodeUnits (charAt, fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Perspectives.ContextAndRole (addRol_gevuldeRollen, changeRol_binding, changeRol_type, context_buitenRol, context_changeRolIdentifier, context_id, context_pspType, defaultContextRecord, defaultRolRecord, rol_binding, rol_context, rol_id, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectives, (##>), MP)
import Perspectives.DataTypeObjectGetters (contextType)
import Perspectives.DomeinFile (DomeinFile(..))

import Perspectives.Identifiers (ModelName(..), PEIdentifier, QualifiedName(..), binnenRol, buitenRol)
import Perspectives.IndentParser (IP, addContext, addRol, generatedNameCounter, getNamespace, getPrefix, getRoleInstances, getRoleOccurrences, getSection, getTypeNamespace, incrementRoleInstances, liftAffToIP, runIndentParser', setNamespace, setPrefix, setRoleInstances, setSection, setTypeNamespace, withExtendedTypeNamespace, withNamespace, withTypeNamespace)
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijvingDef, getDefaultPrototype)
import Perspectives.ObjectGetterConstructors (getPrototype, notEmpty, searchUnqualifiedRolDefinition, toBoolean)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectEntiteit (cacheEntiteitPreservingVersion)
import Perspectives.PerspectivesTypes (BuitenRol, ContextDef(..))
import Perspectives.Resource (getAVarRepresentingPerspectEntiteit, getPerspectEntiteit)
import Perspectives.Syntax (Comments(..), ContextDeclaration(..), EnclosingContextDeclaration(..), PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), binding)
import Perspectives.Token (token)
import Prelude (class Show, Unit, bind, discard, identity, ifM, map, pure, show, unit, ($), ($>), (*>), (+), (-), (/=), (<$>), (<*), (<*>), (<<<), (<>), (==), (>), (>=>))
import Text.Parsing.Indent (block, checkIndent, indented, sameLine, withPos)
import Text.Parsing.Parser (ParseState(..), fail, ParseError(..))
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
localPropertyName = uncapitalizedString

-- localPropertyName = lower alphaNum*
localRolName ::  IP String
localRolName = uncapitalizedString

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
  ln <- (STRING.string "?") *> uncapitalizedString
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


-- | Apply to a single line parser. Will parse a block of contiguous line comments before the line and
-- | the comment after the expression on the line.
withComments :: forall a. IP a -> IP (Tuple Comments a)
withComments p = do
  before <- manyOneLineComments
  withPos do
    a <- p
    after <- inLineComment
    pure $ Tuple (Comments{ commentBefore: before, commentAfter: after}) a

typedPropertyAssignment ::  IP Unit -> IP (Tuple ID PropertyValueWithComments)
typedPropertyAssignment scope = go (try (withComments
  (withPos
    (Tuple
      <$> (scope *> (propertyName <* (sameLine *> reservedOp "=")))
      <*> (sameLine *> (simpleValue `sepBy` (STRING.string ",")))))))
  where
    go x = do
      (Tuple (Comments {commentBefore, commentAfter}) (Tuple pname value)) <- x
      pure $ Tuple (show pname) (PropertyValueWithComments {value: AR.fromFoldable value, commentBefore: commentBefore, commentAfter: commentAfter})

-- | publicContextPropertyAssignment = 'extern' propertyName '=' simpleValue
publicContextPropertyAssignment ::  IP (Tuple ID PropertyValueWithComments)
publicContextPropertyAssignment = (typedPropertyAssignment (reserved "extern")) <?> "extern propertyname = value"

-- | privateContextPropertyAssignment = 'intern' propertyName '=' simpleValue
privateContextPropertyAssignment ::  IP (Tuple ID PropertyValueWithComments)
privateContextPropertyAssignment = (typedPropertyAssignment (reserved "intern")) <?> "intern propertyname = value"

-- | rolePropertyAssignment = propertyName '=' simpleValue
rolePropertyAssignment ::  IP (Tuple ID PropertyValueWithComments)
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
  IP (Tuple (Array Comment) (Maybe ID)) ->
  IP (Tuple RolName ID)
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
      rolId <- pure ((show cname) <> "$" <> localRoleName <> "_" <> (show (roleIndex occurrence nrOfRoleOccurrences)))

      -- Storing
      cacheRol rolId
        (PerspectRol defaultRolRecord
          { _id = rolId
          , occurrence = (roleIndex occurrence nrOfRoleOccurrences)
          , pspType = show rname
          , binding = binding (maybe "" identity bindng)
          , context = show cname
          , properties = FO.fromFoldable ((\(Tuple en cm) -> Tuple en cm) <$> props)
          , comments = Comments { commentBefore: cmtBefore, commentAfter: cmt }
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

cacheRol ::  RolID -> PerspectRol -> IP Unit
cacheRol rolId rol = do
  addRol rol
  liftAffToIP $ cacheEntiteitPreservingVersion rolId rol

cacheContext ::  ContextID -> PerspectContext -> IP Unit
cacheContext contextId ctxt = do
  addContext ctxt
  liftAffToIP $ cacheEntiteitPreservingVersion contextId ctxt

-- | The inline context may itself use a contextInstanceIDInCurrentNamespace to identify the context instance. However,
-- | what is returned from the context parser is the QualifiedName of its buitenRol.
inlineContextBinding ::  QualifiedName
  -> IP (Tuple RolName ID)
inlineContextBinding cName = roleBinding' cName ContextBinding do
  cmt <- inLineComment
  _ <- nextLine
  (contextBuitenRol :: ID) <- indented *> context
  pure $ Tuple cmt (Just contextBuitenRol)

emptyBinding ::  QualifiedName
  -> IP (Tuple RolName ID)
emptyBinding cName = roleBinding' cName ContextBinding do
  _ <- token.parens whiteSpace
  cmt <- inLineComment
  pure $ Tuple cmt Nothing

contextBindingByReference ::  QualifiedName
  -> IP (Tuple RolName ID)
contextBindingByReference cName = roleBinding' cName ContextBinding do
  ident <- (sameLine *> contextReference)
  cmt <- inLineComment
  pure $ Tuple cmt (Just ident)
  where
    contextReference :: IP RolName
    contextReference = do
      qn <- (expandedName <|> prefixedContextName <|> relativeInstanceID)
      pure $ buitenRol (show qn)

roleBindingByReference ::  QualifiedName
  -> IP (Tuple RolName ID)
roleBindingByReference cName = roleBinding' cName RoleBinding do
  ident <- (sameLine *> relativeRolInstanceID <|> rolReference)
  occurrence <- sameLine *> optionMaybe roleOccurrence -- The sequence number in text
  cmt <- inLineComment
  pure $ Tuple cmt (Just (ident <> "_" <> (maybe "1" show occurrence)))
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
  -> IP (Tuple RolName ID)
roleBinding cname =
  inlineContextBinding cname
  <|> contextBindingByReference cname
  <|> emptyBinding cname
  <|> roleBindingByReference cname
-- TODO: query

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
context ::  IP ID
context = withRoleCounting context' where

  context' :: IP ID
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
            (publicProps :: List (Tuple ID PropertyValueWithComments)) <- option Nil (indented *> withExtendedTypeNamespace "buitenRolBeschrijving" (block publicContextPropertyAssignment))
            (privateProps :: List (Tuple ID PropertyValueWithComments)) <- option Nil (indented *> withExtendedTypeNamespace "binnenRolBeschrijving" (block privateContextPropertyAssignment))
            (rolebindings :: List (Tuple RolName ID)) <- option Nil (indented *> (block $ roleBinding instanceName))

            -- Storing
            cacheContext (show instanceName)
              (PerspectContext defaultContextRecord
                { _id = (show instanceName)
                , displayName  = localName
                , pspType = show typeName
                , binnenRol = (binnenRol (show instanceName))
                , buitenRol = buitenRol (show instanceName)
                , rolInContext = collect rolebindings
                , comments = Comments { commentBefore: cmtBefore, commentAfter: cmt}
              })
            cacheRol (binnenRol (show instanceName))
              (PerspectRol defaultRolRecord
                { _id = binnenRol (show instanceName)
                , pspType = show typeName <> "$binnenRolBeschrijving"
                , context = (show instanceName)
                , binding = binding $ buitenRol (show instanceName)
                , properties = FO.fromFoldable privateProps
                })
            cacheRol (buitenRol (show instanceName))
              (PerspectRol defaultRolRecord
                { _id = buitenRol (show instanceName)
                , pspType = show typeName <> "$buitenRolBeschrijving"
                , context = (show instanceName)
                , binding = binding $ maybe "" buitenRol prototype
                , properties = FO.fromFoldable publicProps
                })
            pure $ buitenRol (show instanceName)
  collect :: List (Tuple RolName ID) -> FO.Object (Array ID)
  collect Nil = FO.empty
  collect (Cons (Tuple rname id) r) = let map = collect r in
    case FO.lookup rname map of
      Nothing -> FO.insert rname [id] map
      (Just ids) -> FO.insert rname (AR.cons id ids) map

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
section ::  IP (Tuple String (Array ID))
section = do
  prop <- sectionHeading
  ids <- AR.many definition
  pure $ Tuple prop ids

sectionHeading ::  IP ID
sectionHeading = do
  prop <- reserved "Section" *> propertyName
  setSection prop
  pure $ show prop

definition ::  IP ID
definition = do
  bindng <- context
  prop@(QualifiedName _ localName) <- getSection
  _ <- incrementRoleInstances (show prop)
  nrOfRoleOccurrences <- getRoleOccurrences (show prop)
  enclContext <- getNamespace
  rolId <- pure $ enclContext <> "$" <> localName <> maybe "0" show nrOfRoleOccurrences
  cacheRol rolId
    (PerspectRol defaultRolRecord
      { _id = rolId
      , occurrence = maybe 0 identity nrOfRoleOccurrences
      , pspType = (show prop)
      , binding = binding bindng
      , context = enclContext
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
-- Text
-----------------------------------------------------------
enclosingContext ::  IP ParseRoot
enclosingContext = withRoleCounting enclosingContext' where
  enclosingContext' = do
    cmtBefore <- manyOneLineComments
    withPos do
      (EnclosingContextDeclaration textName cmt) <- enclosingContextDeclaration
      _ <- AR.many importExpression
      (publicProps :: List (Tuple PropertyName PropertyValueWithComments)) <- withExtendedTypeNamespace "buitenRolBeschrijving" (block publicContextPropertyAssignment)
      (privateProps :: List (Tuple PropertyName PropertyValueWithComments)) <- withExtendedTypeNamespace "binnenRolBeschrijving" (block privateContextPropertyAssignment)
      (defs :: Array ((Tuple String (Array ID)))) <- AR.many section
      cacheContext textName
        (PerspectContext defaultContextRecord
          { _id = textName
          , displayName  = textName
          , pspType = "model:Perspectives$Context"
          , binnenRol = binnenRol textName
          , buitenRol = buitenRol textName
          , rolInContext = FO.fromFoldable defs
          , comments = Comments { commentBefore: cmtBefore, commentAfter: cmt}
          })
      cacheRol (binnenRol textName)
        (PerspectRol defaultRolRecord
          { _id = binnenRol textName
          , pspType = "model:Perspectives$Context$binnenRolBeschrijving"
          , context = textName
          , binding = binding $ buitenRol textName
          , properties = FO.fromFoldable privateProps
          })
      cacheRol (buitenRol textName)
        (PerspectRol defaultRolRecord
          { _id = buitenRol textName
          , pspType = "model:Perspectives$Context$buitenRolBeschrijving"
          , context = textName
          , properties = FO.fromFoldable publicProps
          , binding = binding "model:Perspectives$ContextPrototype_buitenRol"
          })
      pure $ RootContext textName

-----------------------------------------------------------
-- User data
-----------------------------------------------------------
userData ::  IP ParseRoot
userData = do
  cmtBefore <- manyOneLineComments
  withPos do
    _ <- userDataDeclaration
    _ <- AR.many importExpression
    contexts <- AR.many context
    -- Hier kan ik ook de rollen van de domeinfile nemen.
    pure $ UserData contexts
  where

    userDataDeclaration :: IP Unit
    userDataDeclaration = reserved "GebruikerGegevens"

-----------------------------------------------------------
-- Parse Root
-----------------------------------------------------------
data ParseRoot = UserData (Array ID) | RootContext ID

rootParser ::  IP ParseRoot
rootParser = enclosingContext <|> userData

-----------------------------------------------------------
-- ParseAndCache
-----------------------------------------------------------
-- catchError :: forall a. m a -> (e -> m a) -> m a

parseAndCache ::  String -> MonadPerspectives (Either ParseError (Tuple ParseRoot DomeinFile))
parseAndCache text = do
  (Tuple parseResult {domeinFile}) <- runIndentParser' text rootParser
  case parseResult of
    (Left e) -> pure $ Left e
    (Right r) -> catchError
      do
        let (DomeinFile{roles, contexts}) = domeinFile
        for_ (FO.values contexts) setDefaultPrototype
        -- change value in cache.
        for_ (FO.values roles) \rol -> do
          case rol_binding rol of
            Nothing -> pure unit
            (Just bndg) -> vultRol bndg (rol_pspType rol) (rol_id rol)
        -- this must read the cache, not use the roles themselves, as they have been changed in the cache!
        for_ (FO.values roles) \rol -> do
          if (isRelativeRolTypeNameOutsideNamespace (rol_pspType rol))
            then addNamespaceToLocalName (rol_id rol)
            else pure unit
        -- We must correct the type of the buitenRollen here. This is because the default type set by the parser
        -- may be wrong if the type T of the context has an Aspect and uses the buitenRolBeschrijving of a prototype
        -- of that Aspect. Then the local name "buitenRolBeschrijving" will be in the namespace of that Aspect,
        -- rather than in the namespace T.
        -- The same holds for the type of the binnenRollen.
        -- We cannot find the correct types before all contexts in the entire modelfile have been parsed.
        for_ (FO.values contexts) \ctxt -> do
          setBuitenRolType ctxt
          setBinnenRolType ctxt
        actualisedDomeinFile <- actualiseDomeinFile domeinFile
        pure $ Right (Tuple r actualisedDomeinFile)
      \e -> pure $ Left $ ParseError (show e) (Position {line: 0, column: 0})
  where

    actualiseDomeinFile :: DomeinFile -> MP DomeinFile
    actualiseDomeinFile df@(DomeinFile {_id, _rev, contexts, roles}) = do
      (ac :: FO.Object PerspectContext) <- traverse (getPerspectEntiteit <<< context_id) contexts
      (ar :: FO.Object PerspectRol) <- traverse (getPerspectEntiteit <<< rol_id) roles
      pure $ DomeinFile {_id: _id, _rev: _rev, contexts: ac, roles: ar}

    setDefaultPrototype :: PerspectContext -> MonadPerspectives Unit
    setDefaultPrototype ctxt = ifM (toBoolean (notEmpty getPrototype) (context_id ctxt))
      (pure unit)
      (do
        (mdefprot :: Maybe BuitenRol) <- (context_pspType ctxt) ##> getDefaultPrototype
        case mdefprot of
          Nothing -> pure unit
          (Just defProt) -> if (context_buitenRol ctxt) == (unwrap defProt)
            then pure unit
            else do
              (av :: AVar PerspectRol) <- getAVarRepresentingPerspectEntiteit (context_buitenRol ctxt)
              (brol :: PerspectRol) <- lift $ take av
              lift $ put (changeRol_binding (unwrap defProt) brol) av
        )

    -- True for an uncapitalizedString that starts with a question mark.
    isRelativeRolTypeNameOutsideNamespace :: String -> Boolean
    isRelativeRolTypeNameOutsideNamespace s = case charAt 0 s of
      (Just '?') -> true
      otherwise -> false

    -- Change the type of the rol to the qualified name, and change the key
    -- in the context's rolinContext FO.Object, too.
    -- NOTE. The id of the Role that is constructed, is based on the namespace as constructed during the parse phase.
    -- However, when a direct reference to that Role is made (using $$-syntax), precisely that name must be re-used,
    -- otherwise the binding cannot be constructed. See "testBotActie.crl" for an example
    addNamespaceToLocalName :: RolID -> MonadPerspectives Unit
    addNamespaceToLocalName rolId = do
      (av :: AVar PerspectRol) <- getAVarRepresentingPerspectEntiteit rolId
      (rol :: PerspectRol) <- lift $ take av
      localRolName' <- pure $ drop 2 (rol_pspType rol)
      mrolType <- (rol_context rol) ##> (contextType >=> (pure <<< map ContextDef) /-/ (searchUnqualifiedRolDefinition localRolName'))
      case mrolType of
        Nothing -> throwError (error ("addNamespaceToLocalName: cannot find qualified name for '" <> localRolName' <> "' in the context of '" <> (rol_context rol) <> "'!" ))
        (Just rolType) -> do
          lift $ void $ put (changeRol_type (unwrap rolType) rol) av
          (cav :: AVar PerspectContext) <- getAVarRepresentingPerspectEntiteit (rol_context rol)
          lift $ void $ do
            ctxt <- take cav
            put (context_changeRolIdentifier ctxt (rol_pspType rol) (unwrap rolType)) cav

    -- Ensure we have an AVar for the RolInstance that is represented by vuller.
    -- Take the Rol out of that AVar in a Forked Aff and add rolId to its gevuldeRollen.
    vultRol :: RolID -> RolName -> RolID -> MonadPerspectives Unit
    vultRol vuller rolName gevuldeRol = do
      (av :: AVar PerspectRol) <- getAVarRepresentingPerspectEntiteit vuller
      lift $ void $ do
        vullerRol <- take av
        put (addRol_gevuldeRollen vullerRol rolName gevuldeRol) av

    -- reads BuitenRol from cache before changing it.
    setBuitenRolType :: PerspectContext ->  MonadPerspectives Unit
    setBuitenRolType ctxt = do
      abrtype <- buitenRolBeschrijvingDef (context_pspType ctxt)
      case head abrtype of
        Nothing -> throwError (error ("setBuitenRolType: no buitenRolBeschrijving for '" <> (context_pspType ctxt) <> "'!"))
        (Just brtype) -> do
          (av :: AVar PerspectRol) <- getAVarRepresentingPerspectEntiteit (buitenRol (context_id ctxt))
          lift $ void $ do
            bRol <- take av
            put (changeRol_type (unwrap brtype) bRol) av

    -- TODO Implementeer setBinnenRolType
    setBinnenRolType :: PerspectContext ->  MonadPerspectives Unit
    setBinnenRolType ctxt = pure unit
