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
import Data.Tuple (Tuple(..))
import Perspectives.Guid (guid)
import Perspectives.Syntax2 (BinnenRol(..), EntityCollection(..), NamedEntityCollection(..), PerspectContext(..), PerspectEntity(..), PerspectRol(..), PropertyName, SimpleValue(..), TypeDeclaration(..), PerspectName)
import Perspectives.Token (token)
import Prelude (Unit, bind, pure, show, unit, ($), ($>), (*>), (<$>), (<*), (<*>), (<>))
import Text.Parsing.Indent (block, indented, sameOrIndented, withPos)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.String (char, satisfy)
import Text.Parsing.Parser.String (anyChar, oneOf, string) as STRING
import Text.Parsing.Parser.Token (alphaNum, upper)

-----------------------------------------------------------
-- Lexemes
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
prefixedPropertyName = lexeme $ (<>) <$> prefix <*> localPropertyName

-- qualifiedResourceName = domeinName localResourceName
qualifiedResourceName :: IP String
qualifiedResourceName = lexeme $ (<>) <$> domeinName <*> localResourceName

-- qualifiedPropertyName = domeinName localPropertyName
qualifiedPropertyName :: IP String
qualifiedPropertyName = lexeme $ (<>) <$> domeinName <*> localPropertyName

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
typeDeclaration = TypeDeclaration <$> resourceName <*> resourceName

typedPropertyAssignment :: String -> IP (Tuple PropertyName (Array String))
typedPropertyAssignment scope = withPos $ (\pn pv -> Tuple pn [show pv])
    <$> identifier <* (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))

-- | publicContextPropertyAssignment = 'public' propertyName '=' simpleValue
publicContextPropertyAssignment :: IP (Tuple PropertyName (Array String))
publicContextPropertyAssignment = reserved "public" *> typedPropertyAssignment "public"

-- | privateContextPropertyAssignment = 'private' propertyName '=' simpleValue
privateContextPropertyAssignment :: IP (Tuple PropertyName (Array String))
privateContextPropertyAssignment =reserved "private" *> typedPropertyAssignment "private"

-- | rolePropertyAssignment = propertyName '=' simpleValue
rolePropertyAssignment :: IP (Tuple PropertyName (Array String))
rolePropertyAssignment = withPos $ (\pn pv -> Tuple pn [show pv])
    <$> propertyName <* (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))

-- | roleBinding = roleName '=>' (resourceName | expression) rolePropertyAssignment*
roleBinding :: PerspectName -> IP NamedEntityCollection
roleBinding contextID =
  withPos $ try do
    rname <- (roleName <* (sameOrIndented *> reservedOp "=>"))
    (NamedEntityCollection pname (EntityCollection entities)) <- indented *> (context <|> role)
    props <- indented *> (block rolePropertyAssignment)
    -- pure $ RolBinding rname pname entities props
    rolId <- pure (guid unit)
    pure $ (NamedEntityCollection rolId
      (EntityCollection
        (insert rolId
          (Rol (PerspectRol
            { id: rolId
            , pspType: rname
            , binding: Just pname
            , context: contextID
            , properties: fromFoldable props
            , gevuldeRollen: empty
            }))
            entities)))
  <|>
  (withPos $ try do
    rname <- (roleName <* (sameOrIndented *> reservedOp "=>"))
    ident <- (sameOrIndented *> resourceName)
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
            , properties: fromFoldable props
            , gevuldeRollen: empty
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
  (TypeDeclaration typeName instanceName) <- typeDeclaration
  indented *> do
    (publicProps :: List (Tuple PropertyName (Array String))) <- (block publicContextPropertyAssignment)
    (privateProps :: List (Tuple PropertyName (Array String))) <- (block privateContextPropertyAssignment)
    (rolebindings :: List (NamedEntityCollection)) <- (block $ roleBinding instanceName)
    let
      ctxt = PerspectContext
        { id: instanceName
        , pspType: typeName
        , binnenRol: binnenRol
        , buitenRol: instanceName <> "_buitenRol"
        , rolInContext: AR.fromFoldable $ (\(NamedEntityCollection id _) -> id) <$> rolebindings }

      binnenRol = BinnenRol
        { id: instanceName <> "_binnenRol"
        , pspType: ":BinnenRol"
        , binding: Just $ instanceName <> "_buitenRol"
        , properties: fromFoldable privateProps
        }

      buitenRol = PerspectRol
        { id: instanceName <> "_buitenRol"
        , pspType: ":BuitenRol"
        , binding: Nothing
        , context: instanceName
        , properties: fromFoldable publicProps
        , gevuldeRollen: empty
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
  (TypeDeclaration typeName instanceName) <- typeDeclaration
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
              , properties: fromFoldable props
              , gevuldeRollen: empty
              })))))

-- | expression = context | role
-- expression :: IP NamedEntityCollection
-- expression = NamedEntityCollection <$> pure "dummy" <*> (pure $ EntityCollection empty)
-- expression = context <|> role

-- Helper functions for development.
allTheRest :: IP String
allTheRest = fromCharArray <$> (AR.many STRING.anyChar)

-----------------------------------------------------------
-- Tests
-----------------------------------------------------------
test1 :: String
test1 = """Aangifte Aangifte1
"""

test2 :: String
test2 = """Aangifte Aangifte1
  public status = "voltooid"
"""

-- runIndentParser "public propname = 1" publicContextPropertyAssignment
-- runIndentParser "private propname = 1" publicContextPropertyAssignment
-- runIndentParser "private propname = 1" privateContextPropertyAssignment
-- runIndentParser "public status = \"voltooid\"" publicContextPropertyAssignment

-- import Text.Parsing.Parser.String
-- runIndentParser "  public status = \"voltooid\"" (withPos (whiteSpace *> indented *> publicContextPropertyAssignment))

test3 :: String
test3 = """:Aangifte :Aangifte1
  :aangever => :Jansen""" -- zodra een newline volgt komt de binding niet mee, geindenteerd of niet.

-- runIndentParser "aangever => Jansen" (roleBinding "")
-- runIndentParser "aangever => Jansen\n  prop = 1" (roleBinding "")

test4 :: String
test4 = """:Aangifte :Aangifte1
  public :status = "voltooid"
  :aangever => :Jansen
"""  -- zodra een newline volgt komt de binding niet mee, geindenteerd

test5 :: String
test5 = """Aangifte Aangifte1
  public status = "voltooid"
  private aantekening = "bla die bla"
  aangever => Jansen"""

test6 :: String
test6 = """Aangifte Aangifte1
  public status = "voltooid"
  private aantekening = "bla die bla"
  aangever => Jansen
    betrouwbaarheid = 6
""" -- Dit werkt weer alleen als er een newline volgt!

test7 :: String
test7 = """aangever => Jansen
    betrouwbaarheid = 6"""
-- runIndentParser test7 (roleBinding "")

test8 :: String
test8 = """aangever => Jansen
    betrouwbaarheid = 6
"""
-- runIndentParser test8 (roleBinding "")
