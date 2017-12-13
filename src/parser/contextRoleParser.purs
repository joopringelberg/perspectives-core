module Perspectives.ContextRoleParser where

import Perspectives.IndentParser
import Control.Alt ((<|>))
import Control.Monad.State (get, gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, fromFoldable, many, snoc) as AR
import Data.Char.Unicode (isLower)
import Data.Foldable (elem)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fromFoldable)
import Data.String (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Perspectives.Guid (guid)
import Perspectives.Resource (storeContextInResourceDefinitions, storeRoleInResourceDefinitions)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Perspectives.Syntax (BinnenRol(..), Comment, Comments(..), ContextDeclaration(..), PerspectContext(..), PerspectName, PerspectRol(..), PropertyComments, PropertyName, SimpleValue(..), TextDeclaration(..), ID)
import Perspectives.Token (token)
import Prelude (Unit, bind, discard, pure, show, unit, ($), ($>), (*>), (+), (/=), (<$>), (<*), (<*>), (<>), (==))
import Text.Parsing.Indent (block, checkIndent, indented, sameLine, sameOrIndented, withPos)
import Text.Parsing.Parser (ParseState(..), fail)
import Text.Parsing.Parser.Combinators (choice, option, try, (<?>), (<??>))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, satisfy, whiteSpace)
import Text.Parsing.Parser.String (anyChar, oneOf, string) as STRING
import Text.Parsing.Parser.Token (alphaNum, upper)

-----------------------------------------------------------
-- Comments
-----------------------------------------------------------

inLineComment :: forall e. IP (Array Comment) e
inLineComment = try $ option [] do
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

-- domeinName = 'model:' upper alphaNum* '#'
domeinName :: forall e. IP String e
domeinName = do
  _ <- STRING.string "model:"
  domein <- capitalizedString
  _ <- char '#'
  pure $ "model:" <> domein <> "#"

-- localResourceName = upper alphaNum*
localResourceName :: forall e. IP String e
localResourceName = capitalizedString

-- localPropertyName = lower alphaNum*
localPropertyName :: forall e. IP String e
localPropertyName = uncapitalizedString

-- prefix = lower* ':'
prefix :: forall e. IP String e
prefix = (f <$> AR.many lower <*> char ':') where
  f ca c = fromCharArray $ AR.snoc ca c

-- prefixedResourceName = prefix localResourceName
prefixedResourceName :: forall e. IP String e
prefixedResourceName = lexeme $ (<>) <$> prefix <*> localResourceName

-- prefixedPropertyName = prefix localPropertyName
prefixedPropertyName :: forall e. IP String e
prefixedPropertyName = lexeme ((<>) <$> prefix <*> localPropertyName)

-- qualifiedResourceName = domeinName localResourceName
qualifiedResourceName :: forall e. IP String e
qualifiedResourceName = lexeme ((<>) <$> domeinName <*> localResourceName)

-- qualifiedPropertyName = domeinName localPropertyName
qualifiedPropertyName :: forall e. IP String e
qualifiedPropertyName = lexeme ((<>) <$> domeinName <*> localPropertyName)

-- resourceName = prefixedResourceName | qualifiedResourceName
resourceName :: forall e. IP String e
resourceName = (qualifiedResourceName <|> prefixedResourceName) <?> "the name of a resource (Context or Role)."

-- propertyName = prefixedPropertyName | qualifiedPropertyName
propertyName :: forall e. IP String e
propertyName = (qualifiedPropertyName <|> prefixedPropertyName) <?> "a property or role name."

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

-- | contextDeclaration = resourceName resourceName
textDeclaration :: forall e. IP TextDeclaration e
textDeclaration = (TextDeclaration <$> (reserved "Text" *> resourceName) <*> inLineComment) <?> "the text declaration: Text <name>."

-- | contextDeclaration = resourceName resourceName
contextDeclaration :: forall e. IP ContextDeclaration e
contextDeclaration = (ContextDeclaration <$> resourceName <*> resourceName <*> inLineComment) <?> "a type declaration, e.g. :ContextType :ContextName."

typedPropertyAssignment :: forall e. String -> IP (Tuple PropertyComments (Tuple PropertyName (Array String))) e
typedPropertyAssignment scope = try $ withPos $ (\cb pn pv cmt -> Tuple (Comments {commentBefore: cb, commentAfter: cmt}) (Tuple pn [show pv]))
    <$> manyOneLineComments <* reserved scope
    <*> propertyName <* (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))
    <*> inLineComment

-- | publicContextPropertyAssignment = 'public' propertyName '=' simpleValue
publicContextPropertyAssignment :: forall e. IP (Tuple PropertyComments (Tuple PropertyName (Array String))) e
publicContextPropertyAssignment = (typedPropertyAssignment "public") <?> "public propertyname = value"

-- | privateContextPropertyAssignment = 'private' propertyName '=' simpleValue
privateContextPropertyAssignment :: forall e. IP (Tuple PropertyComments (Tuple PropertyName (Array String))) e
privateContextPropertyAssignment = (typedPropertyAssignment "private") <?> "private propertyname = value"

-- | rolePropertyAssignment = propertyName '=' simpleValue
rolePropertyAssignment :: forall e. IP (Tuple PropertyComments (Tuple PropertyName (Array String))) e
rolePropertyAssignment = try (withPos $ (\cb pn pv cmt -> Tuple (Comments {commentBefore: cb, commentAfter: cmt}) (Tuple pn [show pv]))
    <$> manyOneLineComments
    <*> propertyName <* (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))
    <*> inLineComment) <?> "propertyname = value"

-- | roleBinding = roleName '=>' (resourceName | context) rolePropertyAssignment*
roleBinding :: forall e. PerspectName -> IP String (DomeinFileEffects e)
roleBinding contextID = ("'rolename =>' followed by context declaration on next line' " <??>
  (withPos $ try do
    cmtBefore <- manyOneLineComments
    withPos do
      rname <- (roleName <* (sameLine *> reservedOp "=>"))
      cmt <- inLineComment
      _ <- nextLine
      (contextBuitenRol :: String) <- indented *> context
      props <- option Nil (indented *> (block (checkIndent *> rolePropertyAssignment)))
      rolId <- pure (guid unit)
      lift $ lift $ storeRoleInResourceDefinitions rolId
        (PerspectRol
          { id: rolId
          , pspType: rname
          , binding: Just contextBuitenRol
          , context: contextID
          , properties: fromFoldable (snd <$> props)
          , gevuldeRollen: empty
          , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt, propertyComments: emptyPropertyComments }
          })
      pure rolId))

  <|>
  ("rolename => resourcename" <??> (withPos $ try do
    cmtBefore <- manyOneLineComments
    withPos do
      rname <- (roleName <* (sameOrIndented *> reservedOp "=>"))
      ident <- (sameLine *> resourceName)
      cmt <- inLineComment
      props <- option Nil (indented *> (block (checkIndent *> rolePropertyAssignment)))
      rolId <- pure (guid unit)
      lift $ lift $ storeRoleInResourceDefinitions rolId
        (PerspectRol
          { id: rolId
          , pspType: rname
          , binding: Just ident
          , context: contextID
          , properties: fromFoldable (snd <$> props)
          , gevuldeRollen: empty
          , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt, propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> props }
          })
      pure rolId))

-- TODO: query

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
context = withPos do
  cmtBefore <- manyOneLineComments
  withPos do
    (ContextDeclaration typeName instanceName cmt) <- contextDeclaration
    do
      (publicProps :: List (Tuple PropertyComments (Tuple PropertyName (Array String)))) <- option Nil (indented *> (block publicContextPropertyAssignment))
      (privateProps :: List (Tuple PropertyComments (Tuple PropertyName (Array String)))) <- option Nil (indented *> (block privateContextPropertyAssignment))
      (rolebindings :: List String) <- option Nil (indented *> (block $ roleBinding instanceName))
      lift $ lift $ storeContextInResourceDefinitions instanceName
        (PerspectContext
          { id: instanceName
          , pspType: typeName
          , binnenRol:
            BinnenRol
              { id: instanceName <> "_binnenRol"
              , pspType: ":BinnenRol"
              , binding: Just $ instanceName <> "_buitenRol"
              , properties: fromFoldable (snd <$> privateProps)
              , comments: Comments { commentBefore: [], commentAfter: [], propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> privateProps }
              }
          , buitenRol: instanceName <> "_buitenRol"
          , rolInContext: AR.fromFoldable rolebindings
          , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt, propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> privateProps }
        })
      lift $ lift $ storeRoleInResourceDefinitions (instanceName <> "_buitenRol")
        (PerspectRol
          { id: instanceName <> "_buitenRol"
          , pspType: ":BuitenRol"
          , binding: Nothing
          , context: instanceName
          , properties: fromFoldable (snd <$> publicProps)
          , gevuldeRollen: empty
          , comments: Comments { commentBefore: [], commentAfter: [], propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> publicProps }
          })
      pure $ instanceName <> "_buitenRol"


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
  , try (roleBinding "" *> (pure "roleBinding" ))
  , try ((STRING.string "--") *> (pure "oneLineComment"))
  -- query
  ]

-----------------------------------------------------------
-- Text
-----------------------------------------------------------
sourceText :: forall e. IP ID (DomeinFileEffects e)
sourceText = withPos do
  cmtBefore <- manyOneLineComments
  withPos do
    (TextDeclaration textName cmt) <- textDeclaration
    (publicProps :: List (Tuple PropertyComments (Tuple PropertyName (Array String)))) <- (block publicContextPropertyAssignment)
    (privateProps :: List (Tuple PropertyComments (Tuple PropertyName (Array String)))) <- (block privateContextPropertyAssignment)
    defs <- AR.many context
    (roleBindings :: Array String) <- lift $ lift $ traverse (\buitenRolId ->
      do
        let roleId = guid unit
        storeRoleInResourceDefinitions roleId
          (PerspectRol
            { id: roleId
            , pspType: "psp:text_Item"
            , binding: Just buitenRolId
            , context: textName
            , properties: empty
            , gevuldeRollen: empty
            , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt, propertyComments: emptyPropertyComments }
            })
        pure roleId) defs
    lift $ lift $ storeContextInResourceDefinitions textName
      (PerspectContext
        { id: textName
        , pspType: "psp:SourceText"
        , binnenRol:
          BinnenRol
            { id: textName <> "_binnenRol"
            , pspType: ":BinnenRol"
            , binding: Just $ textName <> "_buitenRol"
            , properties: fromFoldable (snd <$> privateProps)
            , comments: Comments { commentBefore: [], commentAfter: [], propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> privateProps }
            }
        , buitenRol: textName <> "_buitenRol"
        , rolInContext: roleBindings
        , comments: Comments { commentBefore: cmtBefore, commentAfter: cmt, propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> privateProps }
        })

    lift $ lift $ storeRoleInResourceDefinitions (textName <> "_buitenRol")
      (PerspectRol
        { id: textName <> "_buitenRol"
        , pspType: ":BuitenRol"
        , binding: Nothing
        , context: textName
        , properties: fromFoldable (snd <$> publicProps)
        , gevuldeRollen: empty
        , comments: Comments { commentBefore: [], commentAfter: [], propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> publicProps }
        })
    pure textName

-----------------------------------------------------------
-- Tests
-----------------------------------------------------------
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
  :aangever => :Jansen\n""" -- zodra een newline volgt komt de binding niet mee, geindenteerd of niet.
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
