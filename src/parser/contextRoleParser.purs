module Perspectives.ContextRoleParser where

import Perspectives.IndentParser
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.State (get, gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, many, snoc) as AR
import Data.Char.Unicode (isLower)
import Data.Either (fromRight)
import Data.Foldable (elem, fold)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fromFoldable, insert, lookup, singleton)
import Data.String (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.Resource (PROPDEFS, getContext, storeContextInResourceDefinitions, storeRoleInResourceDefinitions)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax (BinnenRol(..), Comment, Comments(..), ContextDeclaration(..), ID, PerspectContext(..), PerspectName, PerspectRol(..), PropertyName, RoleName, SimpleValue(..), TextDeclaration(..), PropertyValueWithComments)
import Perspectives.Token (token)
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), ($>), (*>), (+), (/=), (<$>), (<*), (<*>), (<>), (==), (>>=))
import Text.Parsing.Indent (block, checkIndent, indented, sameLine, sameOrIndented, withPos)
import Text.Parsing.Parser (ParseState(..), fail)
import Text.Parsing.Parser.Combinators (choice, option, optionMaybe, try, (<?>), (<??>))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, satisfy, whiteSpace)
import Text.Parsing.Parser.String (anyChar, oneOf, string) as STRING
import Text.Parsing.Parser.Token (alphaNum, upper)

-----------------------------------------------------------
-- Comments
-----------------------------------------------------------

inLineComment :: forall e. IP (Array Comment) e
inLineComment = option [] do
  sameLine
  _ <- (STRING.string "--")
  chars <- AR.many (satisfy (_ /= '\n'))
  _ <- whiteSpace
  pure [fromCharArray chars]

manyOneLineComments :: forall e. IP (Array Comment) e
manyOneLineComments = AR.many
    (fromCharArray <$> ((STRING.string "--") *> (AR.many (satisfy (_ /= '\n')) <* whiteSpace)))

-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------

reservedOp :: forall e. String -> IP Unit e
reservedOp = token.reservedOp

reserved :: forall e. String -> IP Unit e
reserved = token.reserved

identifier :: forall e. IP String e
identifier = token.identifier

lexeme :: forall a e. IP a e -> IP a e
lexeme = token.lexeme

int :: forall e. IP SimpleValue e
int = Int <$> token.integer

bool :: forall e. IP SimpleValue e
bool = reserved "true" $> Bool true <|> reserved "false" $> Bool false

string :: forall e. IP SimpleValue e
string = String <$> token.stringLiteral

simpleValue :: forall e. IP SimpleValue e
simpleValue = string <|> int <|> bool

identLetter :: forall e. IP Char e
identLetter = alphaNum <|> STRING.oneOf ['_', '\'']

identLetterString :: forall e. IP String e
identLetterString = fromCharArray <$> AR.many identLetter

-- /([A-Z]\w*\b)/
-- /(\p{Uppercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+)/gu
capitalizedString :: forall e. IP String e
capitalizedString = f <$> upper <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

lower :: forall e. IP Char e
lower = satisfy isLower <?> "uppercase letter"

-- /([a-z]\w*\b)/
-- /(\b\p{Lowercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+\b)/gu
uncapitalizedString :: forall e. IP String e
uncapitalizedString = f <$> lower <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

-- domeinName = 'model:' upper alphaNum* '$'
domeinName :: forall e. IP String e
domeinName = do
  _ <- STRING.string "model:"
  domein <- capitalizedString
  _ <- char '$'
  pure $ "model:" <> domein <> "$"

-- localContextName = upper alphaNum*
localContextName :: forall e. IP String e
localContextName = f <$> capitalizedString <*> AR.many (defaultEmbedded capitalizedString) where
  f first rest = fold $ AR.cons first rest

defaultEmbedded :: forall e. IP String e -> IP String e
defaultEmbedded p = (<>) <$> STRING.string "$" <*> p

-- localPropertyName = lower alphaNum*
localPropertyName :: forall e. IP String e
localPropertyName = uncapitalizedString

-- prefix = lower+ ':'
prefix :: forall e. IP String e
prefix = (f <$> lower <*> AR.many lower <*> char ':') where
  f fst ca c = fromCharArray $ AR.cons fst (AR.snoc ca c)

-- prefixedContextName = prefix localContextName
prefixedContextName :: forall e. IP String e
prefixedContextName = lexeme $ (<>) <$> prefix <*> localContextName

-- prefixedPropertyName = prefix localPropertyName
prefixedPropertyName :: forall e. IP String e
prefixedPropertyName = lexeme ((<>) <$> prefix <*> localPropertyName)

-- qualifiedResourceName = domeinName localContextName
qualifiedContextName :: forall e. IP String e
qualifiedContextName = lexeme ((<>) <$> domeinName <*> localContextName)

-- qualifiedPropertyName = domeinName localPropertyName
qualifiedPropertyName :: forall e. IP String e
qualifiedPropertyName = lexeme ((<>) <$> domeinName <*> localPropertyName)

-- contextName = prefixedContextName | qualifiedContextName
contextName :: forall e. IP String e
contextName = (qualifiedContextName <|> prefixedContextName <|> lexeme (defaultEmbedded localContextName)) <?> "the name of a resource (Context or Role)."

-- propertyName = prefixedPropertyName | qualifiedPropertyName
propertyName :: forall e. IP String e
propertyName = (qualifiedPropertyName <|> prefixedPropertyName <|> lexeme (defaultEmbedded localPropertyName)) <?> "a property or role name."

roleName :: forall e. IP String e
roleName = propertyName

-----------------------------------------------------------
-- Datatypes
-----------------------------------------------------------
dataTypes :: Array String
dataTypes = ["Number", "String", "Bool", "Date"]

dataType :: forall e. IP SimpleValue e
dataType = try do
  s <- identifier
  if elem s dataTypes then pure $ String s else fail "one of 'Number', 'String', 'Bool' or 'Date'."

-----------------------------------------------------------
-- Handling position
-----------------------------------------------------------

-- | @ getPosition @ returns current position
-- | should probably be added to Text.Parsing.Parser.Pos
getPosition :: forall e. IP Position e
getPosition = gets \(ParseState _ pos _) -> pos

sourceColumn :: Position -> Int
sourceColumn (Position {line: _, column: c}) = c

sourceLine :: Position -> Int
sourceLine (Position {line: l, column: _}) = l

-- | Parses only on the next line as the reference
nextLine :: forall e. IP Unit e
nextLine = do
    pos <- getPosition
    s   <- lift get
    if sourceLine s + 1 == sourceLine pos then pure unit else fail "not on next line"
-----------------------------------------------------------
-- Elementary expression types
-----------------------------------------------------------

-- | contextDeclaration = contextName contextName
textDeclaration :: forall e. IP TextDeclaration e
textDeclaration = (TextDeclaration <$> (reserved "Text" *> contextName) <*> inLineComment) <?> "the text declaration: Text <name>."

-- | contextDeclaration = contextName contextName
contextDeclaration :: forall e. IP ContextDeclaration e
contextDeclaration = (ContextDeclaration <$> contextName <*> contextName <*> inLineComment) <?> "a type declaration, e.g. :ContextType :ContextName."

-- | Apply to a single line parser. Will parse a block of contiguous line comments before the line and
-- | the comment after the expression on the line.
withComments :: forall e a. IP a e -> IP (Tuple (Comments ()) a) e
withComments p = do
  before <- manyOneLineComments
  a <- withPos p
  after <- inLineComment
  pure $ Tuple (Comments{ commentBefore: before, commentAfter: after}) a

typedPropertyAssignment :: forall e. IP Unit e -> IP (Tuple PropertyName PropertyValueWithComments) e
typedPropertyAssignment scope = go (try (withComments
  (withPos
    (Tuple
      <$> (scope *> (propertyName <* (sameOrIndented *> reservedOp "=")))
      <*> (sameOrIndented *> (simpleValue <|> dataType))))) )
  where
    go x = do
      (Tuple (Comments {commentBefore, commentAfter}) (Tuple pname value)) <- x
      pure $ Tuple pname (Comments {value: [show value], commentBefore: commentBefore, commentAfter: commentAfter})

-- | publicContextPropertyAssignment = 'public' propertyName '=' simpleValue
publicContextPropertyAssignment :: forall e. IP (Tuple PropertyName PropertyValueWithComments) e
publicContextPropertyAssignment = (typedPropertyAssignment (reserved "public")) <?> "public propertyname = value"

-- | privateContextPropertyAssignment = 'private' propertyName '=' simpleValue
privateContextPropertyAssignment :: forall e. IP (Tuple PropertyName PropertyValueWithComments) e
privateContextPropertyAssignment = (typedPropertyAssignment (reserved "private")) <?> "private propertyname = value"

-- | rolePropertyAssignment = propertyName '=' simpleValue
rolePropertyAssignment :: forall e. IP (Tuple PropertyName PropertyValueWithComments) e
rolePropertyAssignment = (typedPropertyAssignment (pure unit)) <?> "private propertyname = value"

isRoleDeclaration :: forall e. IP Unit (DomeinFileEffects e)
isRoleDeclaration = withPos (roleName *> (sameLine *> optionMaybe roleOccurrence) *> (sameLine *> reservedOp "=>") *> pure unit)

roleOccurrence :: forall e. IP Int e
roleOccurrence = token.parens token.integer

roleBinding' :: forall e.
  PerspectName
  -> IP (Tuple (Array Comment) ID) (DomeinFileEffects e)
  -> IP (Tuple RoleName ID) (DomeinFileEffects e)
roleBinding' contextID p = ("rolename => contextName" <??>
  (withPos $ try do
    cmtBefore <- manyOneLineComments
    withPos do
      rname <- roleName
      occurrence <- sameLine *> optionMaybe roleOccurrence -- The sequence number in text
      _ <- (sameLine *> reservedOp "=>")
      (Tuple cmt ident) <- p
      props <- option Nil (indented *> (block (checkIndent *> rolePropertyAssignment)))
      _ <- incrementRoleInstances rname
      nrOfRoleOccurrences <- getRoleOccurrences rname -- The position in the sequence.
      rolId <- pure $ contextID <> "_" <> rname <> "_" <> roleIndex occurrence nrOfRoleOccurrences
      liftAffToIP $ storeRoleInResourceDefinitions rolId
        (PerspectRol
          { id: rolId
          , occurrence: nrOfRoleOccurrences
          , pspType: rname
          , binding: Just ident
          , context: contextID
          , properties: fromFoldable props
          , gevuldeRollen: empty
          , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt }
          })
      pure $ Tuple rname rolId))
  where
    -- If there is an index in the text, it prevails.
    roleIndex :: Maybe Int -> Maybe Int -> String
    roleIndex nrInText nrInSequence = case nrInText of
      (Just n) -> show n
      Nothing -> case nrInSequence of
        (Just n) -> show n
        Nothing -> "0"

roleBindingWithInlineContext :: forall e. PerspectName -> IP (Tuple RoleName ID) (DomeinFileEffects e)
roleBindingWithInlineContext id = roleBinding' id do
  cmt <- inLineComment
  _ <- nextLine
  (contextBuitenRol :: String) <- indented *> context
  pure $ Tuple cmt contextBuitenRol

roleBindingWithReference :: forall e. PerspectName -> IP (Tuple RoleName ID) (DomeinFileEffects e)
roleBindingWithReference id = roleBinding' id do
  ident <- (sameLine *> contextName)
  cmt <- inLineComment
  pure $ Tuple cmt ident

-- | roleBinding = roleName '=>' (contextName | context) rolePropertyAssignment*
roleBinding :: forall e. PerspectName -> IP (Tuple RoleName ID) (DomeinFileEffects e)
roleBinding contextID = roleBindingWithInlineContext contextID <|> roleBindingWithReference contextID -- TODO: query

withRoleCounting :: forall a e. IP a e -> IP a e
withRoleCounting p = do
  roleInstances <- getRoleInstances
  setRoleInstances empty
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
context :: forall e. IP String (DomeinFileEffects e)
context = withRoleCounting context' where
  context' = withPos do
    cmtBefore <- manyOneLineComments
    withPos do
      (ContextDeclaration typeName instanceName cmt) <- contextDeclaration
      do
        (publicProps :: List (Tuple PropertyName PropertyValueWithComments)) <- option Nil (indented *> (block publicContextPropertyAssignment))
        (privateProps :: List (Tuple PropertyName PropertyValueWithComments)) <- option Nil (indented *> (block privateContextPropertyAssignment))
        (rolebindings :: List (Tuple RoleName ID)) <- option Nil (indented *> (block $ roleBinding instanceName))
        liftAffToIP $ storeContextInResourceDefinitions instanceName
          (PerspectContext
            { id: instanceName
            , displayName : instanceName
            , pspType: typeName
            , binnenRol:
              BinnenRol
                { id: instanceName <> "_binnenRol"
                , pspType: "model:Perspectives$BinnenRol"
                , binding: Just $ instanceName <> "_buitenRol"
                , properties: fromFoldable privateProps
                }
            , buitenRol: instanceName <> "_buitenRol"
            , rolInContext: collect rolebindings
            , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt}
          })
        liftAffToIP $ storeRoleInResourceDefinitions (instanceName <> "_buitenRol")
          (PerspectRol
            { id: instanceName <> "_buitenRol"
            , occurrence: Nothing
            , pspType: "model:Perspectives$BuitenRol"
            , binding: Nothing
            , context: instanceName
            , properties: fromFoldable publicProps
            , gevuldeRollen: empty
            , comments: Comments { commentBefore: [], commentAfter: []}
            })
        pure $ instanceName <> "_buitenRol"
  collect :: List (Tuple RoleName ID) -> StrMap (Array ID)
  collect Nil = empty
  collect (Cons (Tuple rname id) r) = let map = collect r in
    case lookup rname map of
      Nothing -> insert rname [id] map
      (Just ids) -> insert rname (AR.cons id ids) map


-- Helper functions for development.
allTheRest :: forall e. IP String e
allTheRest = fromCharArray <$> (AR.many STRING.anyChar)

emptyPropertyComments :: StrMap (Comments ())
emptyPropertyComments = empty

-----------------------------------------------------------
-- Expression
-----------------------------------------------------------

expression :: forall e. IP String (DomeinFileEffects e)
expression = choice
  [ try (textDeclaration *> (pure "textDeclaration"))
  , try (contextDeclaration *> (pure "contextDeclaration"))
  , try (publicContextPropertyAssignment *> (pure "publicContextPropertyAssignment"))
  , try (privateContextPropertyAssignment *> (pure "privateContextPropertyAssignment"))
  , try (rolePropertyAssignment *> (pure "rolePropertyAssignment"))
  , try (isRoleDeclaration *> (pure "isRoleDeclaration" ))
  , try ((STRING.string "--") *> (pure "oneLineComment"))
  -- query
  ]

-----------------------------------------------------------
-- Text
-----------------------------------------------------------
sourceText :: forall e. IP ID (DomeinFileEffects e)
sourceText = withRoleCounting sourceText' where
  sourceText' = withPos do
    cmtBefore <- manyOneLineComments
    withPos do
      (TextDeclaration textName cmt) <- textDeclaration
      (publicProps :: List (Tuple PropertyName PropertyValueWithComments)) <- (block publicContextPropertyAssignment)
      (privateProps :: List (Tuple PropertyName PropertyValueWithComments)) <- (block privateContextPropertyAssignment)
      defs <- AR.many context
      (roleBindings :: Array String) <- (traverse (\buitenRolId ->
        do
          _ <- incrementRoleInstances "psp:text_Item"
          nrOfRoleOccurrences <- getRoleOccurrences "psp:text_Item"
          rolId <- pure $ textName <> "_psp:text_Item_" <> show nrOfRoleOccurrences
          -- let roleId = guid unit
          liftAffToIP $ storeRoleInResourceDefinitions rolId
            (PerspectRol
              { id: rolId
              , occurrence: nrOfRoleOccurrences
              , pspType: "psp:text_Item"
              , binding: Just buitenRolId
              , context: textName
              , properties: empty
              , gevuldeRollen: empty
              , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt}
              })
          pure rolId) defs)
      liftAffToIP $ storeContextInResourceDefinitions textName
        (PerspectContext
          { id: textName
          , displayName : textName
          , pspType: "model:Perspectives$SourceText"
          , binnenRol:
            BinnenRol
              { id: textName <> "_binnenRol"
              , pspType: "model:Perspectives$BinnenRol"
              , binding: Just $ textName <> "_buitenRol"
              , properties: fromFoldable privateProps
              }
          , buitenRol: textName <> "_buitenRol"
          , rolInContext: singleton "psp:text_Item" roleBindings
          , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt}
          })

      liftAffToIP $ storeRoleInResourceDefinitions (textName <> "_buitenRol")
        (PerspectRol
          { id: textName <> "_buitenRol"
          , occurrence: Nothing
          , pspType: "model:Perspectives$BuitenRol"
          , binding: Nothing
          , context: textName
          , properties: fromFoldable publicProps
          , gevuldeRollen: empty
          , comments: Comments { commentBefore: [], commentAfter: []}
          })
      pure textName

-----------------------------------------------------------
-- Tests
-----------------------------------------------------------

-- runAff_ (\_->pure unit) ((runIndentParser test1 context) >>= (\r -> log (show r)))

runTest :: forall e a. Show a => Aff (console :: CONSOLE | e) a -> Eff (console :: CONSOLE | e) Unit
runTest t =
  runAff_ (\_->pure unit) (t >>= (\r -> log (show r)))

getContextDef :: forall e. String -> Aff (DomeinFileEffects (prd :: PROPDEFS | e)) (Maybe PerspectContext)
getContextDef id = do
  getContext id

runAndShowContext :: forall e. String -> Eff (DomeinFileEffects (prd :: PROPDEFS, console :: CONSOLE | e)) Unit
runAndShowContext text = runTest ((runIndentParser text context) >>= (\x -> getContextDef (unsafePartial (fromRight x))) >>= (\r -> log (show r)))
-- x = runTest (runIndentParser test1 context)
-- x = runTest ((runIndentParser test22 context) >>= (\x -> getContextDef (unsafePartial (fromRight x))) >>= (\r -> log (show r)))

test1 :: String
test1 = """:Aangifte :Aangifte1
"""
-- runIndentParser test1 contextDeclaration

test2 :: String
test2 = """:Aangifte :Aangifte1 -- Commentaar bij :Aangifte1
  public :status = "voltooid\n" -- Commentaar bij status
"""
-- runIndentParser test2 context

-- runIndentParser "public :propname = 1" publicContextPropertyAssignment
-- runIndentParser "private :propname = 1" publicContextPropertyAssignment
-- runIndentParser "private :propname = 1" privateContextPropertyAssignment
-- runIndentParser "public :status = \"voltooid\"" publicContextPropertyAssignment

-- import Text.Parsing.Parser.String
-- runIndentParser "  public status = \"voltooid\"" (withPos (whiteSpace *> indented *> publicContextPropertyAssignment))

test3 :: String
test3 = """:Aangifte :Aangifte1
  :aangever (0) => :Jansen\n"""
-- runIndentParser test3 context

test3a :: String
test3a = """:Aangifte :Aangifte1
  :aangever (0) => :Jansen\n
  :aangever (1) => :Pietersen\n"""

-- runIndentParser test3 context
-- runIndentParser ":aangever => Jansen" (roleBinding "")
-- runIndentParser ":aangever => Jansen\n  prop = 1" (roleBinding "")

test4 :: String
test4 = """:Aangifte :Aangifte1 -- Commentaar bij :Aangifte1
  public :status = "voltooid" -- Commentaar bij :status
  :aangever => :Jansen -- commentaar bij :aangever"""  -- zodra een newline volgt komt de binding niet mee, geindenteerd
-- runIndentParser test4 context

test5 :: String
test5 = """--Commentaar voor :Aangifte1
:Aangifte :Aangifte1 --Commentaar achter :Aangifte1
  public :status = "voltooid"
  private :aantekening = "bla die bla"
  :aangever => :Jansen"""
-- runIndentParser test5 context

test6 :: String
test6 = """:Aangifte :Aangifte1
  public :status = "voltooid"
  private :aantekening = "bla die bla"
  -- Commentaar voor :aangever
  :aangever => :Jansen -- Commentaar bij :aangever
    :betrouwbaarheid = 6 -- Commentaar bij :betrouwbaarheid"""

-- runIndentParser test6 context

test6a :: String
test6a = """-- Commentaar voor :aangever
:aangever => :Jansen -- Commentaar bij :aangever
  :betrouwbaarheid = 6 -- Commentaar bij :betrouwbaarheid
"""
-- runIndentParser test6a (roleBinding ":Aangifte1")

test6b :: String
test6b = """private :aantekening = "bla die bla"
-- Commentaar voor :aangever
"""
-- runIndentParser test6b privateContextPropertyAssignment
-- Het commentaar wordt geparseerd en genegeerd!

test7 :: String
test7 = """:aangever => :Jansen
    :betrouwbaarheid = 6"""
-- runIndentParser test7 (roleBinding "")

test8 :: String
test8 = """:aangever => :Jansen
    :betrouwbaarheid = 6
"""
-- runIndentParser test8 (roleBinding "")

test9 :: String
test9 = """:ContextType :Aangifte -- Commentaar op de regel.
    public :isFunctioneel = true --Commentaar 1
    -- commentaar boven :isVerplicht
    public :isVerplicht = true --Commentaar 1
"""
-- runIndentParser test9 context

test10 :: String
test10 = """:ContextType :Aangifte
	-- Commentaar voor rolProperty
  :rolProperty =>
		:Property :Betrouwbaarheid
			public :isFunctioneel = true
"""
-- runIndentParser test10 context

test10a :: String
test10a = """-- Commentaar voor rolProperty
:rolProperty =>
	:Property :Betrouwbaarheid
		public :isFunctioneel = true
"""
-- runIndentParser test10a (roleBinding ":Aangifte")

test10b :: String
test10b = """:rolProperty =>
	:Property :Betrouwbaarheid
		public :isFunctioneel = true
"""
-- runIndentParser test10b (roleBinding ":Aangifte")

test10c :: String
test10c = """:ContextType :Aangifte
	:rolProperty =>
		:Property :Betrouwbaarheid
			public :isFunctioneel = true
"""
-- runIndentParser test10c context

test11 :: String
test11 = """:Aangifte :A
	--Commentaar voor de properties
	public :urgentie = 1 -- Commentaar achter de property
  """

test12 :: String
test12 = """Text :Mysource
:Aangifte :A1
  :aangever => :Jansen
:Aangifte :A2"""

test13 :: String
test13 = """:Aangifte :A
	:aangever =>
		:RolDef :R"""

test14 :: String
test14 = """:Aangifte :A
	:aangever =>
		:RolDef :Pietersen
		:prop = 1"""

test15 :: String
test15 = """:Aangifte :A
	:aangever =>
		:RolDef :Pietersen
		  :prop = 1"""

test16 :: String
test16 = """Text :Mytext
:Aangifte :A
	:aangever =>
		:RolDef :Pietersen
			public :betrouwbaarheid = 1"""

test17 :: String
test17 = """Text :A
:Aangifte :B12"""
-- runTest (runIndentParser test17 sourceText)

test18 :: String
test18 = """Text :T
:Aangifte :A1
	:aangever (0) => :Jansen
	:aangever (1) => :Pietersen
"""

test19 :: String
test19 = """:ContextType :Aangifte
	:publicProperty =>
		:Property $Urgentie
			public :prop = 3"""

test20 :: String
test20 = """:publicProperty =>
	:Property $Urgentie
		public :prop = 3"""

test21 :: String
test21 = """:ContextType :Aangifte
	:publicProperty => $Urgentie
		:rolprop = 3"""

test22 :: String
test22 = """$Property $Urgentie
	public $isFunctioneel = true
	-- Commentaar boven $isVerplicht
	public $isVerplicht = false"""
