module Perspectives.ContextRoleParser where

import Perspectives.IndentParser
import Control.Alt ((<|>))
import Data.Array (cons, fromFoldable, many, snoc) as AR
import Data.Char.Unicode (isLower)
import Data.Foldable (elem)
import Data.List.Types (List)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fromFoldable, insert, singleton, unions)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..), snd)
import Perspectives.Guid (guid)
import Perspectives.Syntax2 (BinnenRol(..), Comments(..), EntityCollection(..), NamedEntityCollection(..), OptionalComment, PerspectContext(..), PerspectEntity(..), PerspectName, PerspectRol(..), PropertyComments, PropertyName, SimpleValue(..), TypeDeclaration(..), Comment)
import Perspectives.Token (token)
import Prelude (Unit, bind, pure, show, unit, ($), ($>), (*>), (/=), (<$>), (<*), (<*>), (<>))
import Text.Parsing.Indent (block, indented, sameLine, sameOrIndented, withPos)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (choice, optionMaybe, try, (<?>))
import Text.Parsing.Parser.String (char, satisfy, whiteSpace)
import Text.Parsing.Parser.String (anyChar, oneOf, string) as STRING
import Text.Parsing.Parser.Token (alphaNum, upper)

-----------------------------------------------------------
-- Comments
-----------------------------------------------------------

oneLineComment :: IP OptionalComment
oneLineComment = optionMaybe $ try
    (fromCharArray <$> ((STRING.string "--") *> (AR.many (satisfy (_ /= '\n')) <* whiteSpace)))

inLineComment :: IP OptionalComment
inLineComment = optionMaybe $ try
    (sameLine *> (fromCharArray <$> ((STRING.string "--") *> (AR.many (satisfy (_ /= '\n')) <* whiteSpace))))

manyOneLineComments :: IP (Array Comment)
manyOneLineComments = AR.many
    (fromCharArray <$> ((STRING.string "--") *> (AR.many (satisfy (_ /= '\n')) <* whiteSpace)))

-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------

reservedOp :: String -> IP Unit
reservedOp = token.reservedOp

reserved :: String -> IP Unit
reserved = token.reserved

identifier :: IP String
identifier = token.identifier

lexeme :: forall a. IP a -> IP a
lexeme = token.lexeme

int :: IP SimpleValue
int = Int <$> token.integer

bool :: IP SimpleValue
bool = reserved "true" $> Bool true <|> reserved "false" $> Bool false

string :: IP SimpleValue
string = String <$> token.stringLiteral

simpleValue :: IP SimpleValue
simpleValue = string <|> int <|> bool

identLetter :: IP Char
identLetter = alphaNum <|> STRING.oneOf ['_', '\'']

identLetterString :: IP String
identLetterString = fromCharArray <$> AR.many identLetter

-- /([A-Z]\w*\b)/
-- /(\p{Uppercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+)/gu
capitalizedString :: IP String
capitalizedString = f <$> upper <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

lower :: IP Char
lower = satisfy isLower <?> "uppercase letter"

-- /([a-z]\w*\b)/
-- /(\b\p{Lowercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+\b)/gu
uncapitalizedString :: IP String
uncapitalizedString = f <$> lower <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

-- domeinName = 'model:' upper alphaNum* '#'
domeinName :: IP String
domeinName = do
  _ <- STRING.string "model:"
  domein <- capitalizedString
  _ <- char '#'
  pure $ "model:" <> domein <> "#"

-- localResourceName = upper alphaNum*
localResourceName :: IP String
localResourceName = capitalizedString

-- localPropertyName = lower alphaNum*
localPropertyName :: IP String
localPropertyName = uncapitalizedString

-- prefix = lower* ':'
prefix :: IP String
prefix = (f <$> AR.many lower <*> char ':') where
  f ca c = fromCharArray $ AR.snoc ca c

-- prefixedResourceName = prefix localResourceName
prefixedResourceName :: IP String
prefixedResourceName = lexeme $ (<>) <$> prefix <*> localResourceName

-- prefixedPropertyName = prefix localPropertyName
prefixedPropertyName :: IP String
prefixedPropertyName = lexeme ((<>) <$> prefix <*> localPropertyName)

-- qualifiedResourceName = domeinName localResourceName
qualifiedResourceName :: IP String
qualifiedResourceName = lexeme ((<>) <$> domeinName <*> localResourceName)

-- qualifiedPropertyName = domeinName localPropertyName
qualifiedPropertyName :: IP String
qualifiedPropertyName = lexeme ((<>) <$> domeinName <*> localPropertyName)

-- resourceName = prefixedResourceName | qualifiedResourceName
resourceName :: IP String
resourceName = qualifiedResourceName <|> prefixedResourceName

-- propertyName = prefixedPropertyName | qualifiedPropertyName
propertyName :: IP String
propertyName = qualifiedPropertyName <|> prefixedPropertyName

roleName :: IP String
roleName = propertyName

-----------------------------------------------------------
-- Datatypes
-----------------------------------------------------------
dataTypes :: Array String
dataTypes = ["Number", "String", "Bool", "Date"]

dataType :: IP SimpleValue
dataType = try do
  s <- identifier
  if elem s dataTypes then pure $ String s else fail "Expected one of 'Number', 'String', 'Bool' or 'Date'."
-----------------------------------------------------------
-- Elementary expression types
-----------------------------------------------------------

-- | typeDeclaration = resourceName resourceName
typeDeclaration :: IP TypeDeclaration
typeDeclaration = TypeDeclaration <$> resourceName <*> resourceName <*> inLineComment

typedPropertyAssignment :: String -> IP (Tuple PropertyComments (Tuple PropertyName (Array String)))
typedPropertyAssignment scope = try $ withPos $ (\cb pn pv cmt -> Tuple (Comments {commentBefore: cb, commentAfter: cmt}) (Tuple pn [show pv]))
    <$> manyOneLineComments <* reserved scope
    <*> propertyName <* (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))
    <*> inLineComment

-- | publicContextPropertyAssignment = 'public' propertyName '=' simpleValue
publicContextPropertyAssignment :: IP (Tuple PropertyComments (Tuple PropertyName (Array String)))
publicContextPropertyAssignment = typedPropertyAssignment "public"

-- | privateContextPropertyAssignment = 'private' propertyName '=' simpleValue
privateContextPropertyAssignment :: IP (Tuple PropertyComments (Tuple PropertyName (Array String)))
privateContextPropertyAssignment = typedPropertyAssignment "private"

-- | rolePropertyAssignment = propertyName '=' simpleValue
rolePropertyAssignment :: IP (Tuple PropertyComments (Tuple PropertyName (Array String)))
rolePropertyAssignment = withPos $ (\cb pn pv cmt -> Tuple (Comments {commentBefore: cb, commentAfter: cmt}) (Tuple pn [show pv]))
    <$> manyOneLineComments
    <*> propertyName <* (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))
    <*> inLineComment

-- | roleBinding = roleName '=>' (resourceName | context) rolePropertyAssignment*
roleBinding :: PerspectName -> IP NamedEntityCollection
roleBinding contextID =
  withPos $ try do
    cmtBefore <- manyOneLineComments
    withPos do
      rname <- (roleName <* (sameOrIndented *> reservedOp "=>"))
      cmt <- inLineComment
      (NamedEntityCollection pname (EntityCollection entities)) <- indented *> (context <|> role)
      -- props <- indented *> (block rolePropertyAssignment)
      pure $ (NamedEntityCollection pname
        (EntityCollection
          (insert pname
            (Rol (PerspectRol
              { id: pname
              , pspType: rname
              , binding: Just pname
              , context: contextID
              , properties: empty
              , gevuldeRollen: empty
              , comments: Just $ Comments { commentBefore: cmtBefore, commentAfter: cmt, propertyComments: emptyPropertyComments }
              }))
              entities)))
  <|>
  (withPos $ try do
    cmtBefore <- manyOneLineComments
    withPos do
      rname <- (roleName <* (sameOrIndented *> reservedOp "=>"))
      ident <- (sameOrIndented *> resourceName)
      cmt <- inLineComment
      props <- indented *> (block rolePropertyAssignment)
      rolId <- pure (guid unit)
      pure $ (NamedEntityCollection rolId
        (EntityCollection
          (singleton
            rolId
            (Rol (PerspectRol
              { id: rolId
              , pspType: rname
              , binding: Just ident
              , context: contextID
              , properties: fromFoldable (snd <$> props)
              , gevuldeRollen: empty
              , comments: Just $ Comments { commentBefore: cmtBefore, commentAfter: cmt, propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> props }
              }))))))

-- TODO: query

-----------------------------------------------------------
-- Context, Role and Expression
-----------------------------------------------------------

-- | context = typeDeclaration
-- | publicContextPropertyAssignment*
-- | privateContextPropertyAssignment*
-- | roleBinding*
context :: IP NamedEntityCollection
context = withPos do
  cmtBefore <- manyOneLineComments
  withPos do
    (TypeDeclaration typeName instanceName cmt) <- typeDeclaration
    indented *> do
      (publicProps :: List (Tuple PropertyComments (Tuple PropertyName (Array String)))) <- (block publicContextPropertyAssignment)
      (privateProps :: List (Tuple PropertyComments (Tuple PropertyName (Array String)))) <- (block privateContextPropertyAssignment)
      (rolebindings :: List (NamedEntityCollection)) <- (block $ roleBinding instanceName)
      let
        ctxt = PerspectContext
          { id: instanceName
          , pspType: typeName
          , binnenRol: binnenRol
          , buitenRol: instanceName <> "_buitenRol"
          , rolInContext: AR.fromFoldable $ (\(NamedEntityCollection id _) -> id) <$> rolebindings
          , comments: Just $ Comments { commentBefore: cmtBefore, commentAfter: cmt, propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> privateProps }
        }

        binnenRol = BinnenRol
          { id: instanceName <> "_binnenRol"
          , pspType: ":BinnenRol"
          , binding: Just $ instanceName <> "_buitenRol"
          , properties: fromFoldable (snd <$> privateProps)
          }

        buitenRol = PerspectRol
          { id: instanceName <> "_buitenRol"
          , pspType: ":BuitenRol"
          , binding: Nothing
          , context: instanceName
          , properties: fromFoldable (snd <$> publicProps)
          , gevuldeRollen: empty
          , comments: Just $ Comments { commentBefore: [], commentAfter: Nothing, propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> publicProps }
          }

        (entities :: StrMap PerspectEntity) =
          unions $ (\(NamedEntityCollection _ (EntityCollection m)) -> m) <$> rolebindings

      pure
        (NamedEntityCollection instanceName
          (EntityCollection (insert instanceName (Context ctxt)
            (insert (instanceName <> "_buitenRol") (Rol buitenRol) entities))))

-- | role = typeDeclaration
-- | rolePropertyAssignment*
-- | query?
-- TODO: query
-- TODO: moet er geen binding bij?
role :: IP NamedEntityCollection
role = withPos do
  cmtBefore <- manyOneLineComments
  withPos do
    (TypeDeclaration typeName instanceName _) <- typeDeclaration
    indented *> do
      props <- (block rolePropertyAssignment)
      pure
        (NamedEntityCollection instanceName
          (EntityCollection
            (singleton instanceName
              (Rol (PerspectRol
                { id: instanceName
                , pspType: typeName
                , binding: Nothing
                , context: instanceName
                , properties: fromFoldable $ snd <$> props
                , gevuldeRollen: empty
                , comments: Just (Comments { commentBefore: cmtBefore, commentAfter: Nothing, propertyComments: fromFoldable $ (\(Tuple cmts (Tuple pn _)) -> Tuple pn cmts) <$> props})
                })))))

-- | expression = context | role
-- expression :: IP NamedEntityCollection
-- expression = NamedEntityCollection <$> pure "dummy" <*> (pure $ EntityCollection empty)
-- expression = context <|> role

-- Helper functions for development.
allTheRest :: IP String
allTheRest = fromCharArray <$> (AR.many STRING.anyChar)

emptyPropertyComments :: StrMap (Comments ())
emptyPropertyComments = empty

-----------------------------------------------------------
-- Expression
-----------------------------------------------------------

expression :: IP String
expression = choice
  [ try (typeDeclaration *> (pure "typeDeclaration"))
  , try (publicContextPropertyAssignment *> (pure "publicContextPropertyAssignment"))
  , try (privateContextPropertyAssignment *> (pure "privateContextPropertyAssignment"))
  , try (rolePropertyAssignment *> (pure "rolePropertyAssignment"))
  , try (roleBinding "" *> (pure "roleBinding" ))
  , try ((STRING.string "--") *> (pure "oneLineComment"))
  -- query
  ]
-----------------------------------------------------------
-- Tests
-----------------------------------------------------------
test1 :: String
test1 = """:Aangifte :Aangifte1
"""
-- runIndentParser test1 typeDeclaration

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
