module Perspectives.Parser where

import Perspectives.IndentParser
import Control.Alt ((<|>))
import Data.Foldable (elem)
import Data.List (List(..), many)
import Data.Maybe (Maybe(..))
import Perspectives.Syntax (Context(..), ContextDefinition(..), PropertyAssignment(..), PropertyDefinition(..), RolAssignmentWithPropertyAssignments(..), SimpleValue(..))
import Perspectives.Token (token)
import Prelude (Unit, bind, discard, pure, ($), ($>), (*>), (<$>), (<*>), (<<<), (>>=))
import Text.Parsing.Indent (block, checkIndent, indented, sameLine, sameOrIndented, withPos)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (option, optionMaybe, try)

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

-----------------------------------------------------------
-- Datatypes
-----------------------------------------------------------
dataTypes :: Array String
dataTypes = ["Number", "String", "Bool", "Date"]

dataType :: IP SimpleValue
dataType = do
  s <- try identifier
  if elem s dataTypes then pure $ String s else fail "Expected one of 'Number', 'String', 'Bool' or 'Date'."
-----------------------------------------------------------
-- Assignment
-----------------------------------------------------------

-- | identifier = simpleValue
propertyAssignment :: IP PropertyAssignment
propertyAssignment = withPos $
  {name: _, op: _, value: _}
    <$> (sameLine *> identifier)
    <*> (sameLine *> reservedOp "=")
    <*> (sameLine *> (simpleValue <|> dataType))
  >>= (pure <<< PropertyAssignment)

-- rolAssignment = type '=>' identifier BLOCK propertyAssignment*
-- Consumes a rol binding expression followed by zero or more property assignment expressions.
rolAssignment :: IP RolAssignmentWithPropertyAssignments
rolAssignment = withPos do
  typeName <- sameLine *> identifier
  sameLine *> reservedOp "=>"
  ident <- sameLine *> identifier
  props <- withPos (block $ checkIndent *> propertyAssignment)
  pure (RolAssignmentWithPropertyAssignments {name: typeName, binding: ident, properties: props})

propertyAssignmentList :: String -> IP (List PropertyAssignment)
propertyAssignmentList keyword = withPos do
  reserved keyword
  first <- sameOrIndented *> propertyAssignment
  rest <- option Nil (indented *> (withPos $ block (checkIndent *> propertyAssignment)))
  pure (Cons first rest)

-- propertyDefinition = ('public' | 'private') identifier BLOCK propertyAssignment*
propertyDefinition :: IP PropertyDefinition
propertyDefinition = withPos do
  private <- optionMaybe (reserved "private")
  scope <- case private of
    Nothing -> do
      public <- optionMaybe (reserved "public")
      case public of
        Nothing -> fail "Expected 'private' or 'public'"
        otherwise -> pure "public"
    otherwise -> pure "private"
  name <- sameLine *> identifier
  props <- indented *> withPos (block (try propertyAssignment))
  pure $ PropertyDefinition {scope: scope, name: name, properties: props}

-----------------------------------------------------------
-- Context
-----------------------------------------------------------

-- context = type identifier BLOCK propertyAssignment* rolAssignment*
context :: IP Context
context = withPos do
  typeName <- sameLine *> identifier
  instanceName <- sameLine *> identifier
  indented *> withPos do
    props <- (block (try propertyAssignment))
    roles <- (block (try rolAssignment))
    pure $ Context {id: instanceName
                  ,contextType: typeName
                  , properties: props
                  , roles: roles }

-- contextDefinition = type identifier BLOCK
-- 	| privatePropertyDefinition
-- 	| publicPropertyDefinition
-- 	( rolDefinition
-- 	| RolAssignment
-- 	)*
contextDefinition :: IP ContextDefinition
contextDefinition = withPos do
  typeName <- sameLine *> identifier
  definedTypeName <- sameLine *> identifier
  indented *> withPos do
    privateProps <- option Nil (checkIndent *> propertyAssignmentList "private")
    publicProps <- option Nil (checkIndent *> propertyAssignmentList "public")
    roles <- option Nil (checkIndent *> (many rolAssignment))
    pure $ ContextDefinition {id: definedTypeName
                  ,contextType: typeName
                  , privateProperties: privateProps
                  , publicProperties: publicProps
                  , roles: roles }
-----------------------------------------------------------
-- Tests
-----------------------------------------------------------

test1 :: String
test1 =
  "rol1 => rol2"

test2 :: String
test2 =
  """rol1 => rol2
  a = 1
  b = 2"""

test3 :: String
test3 = "rol1 => rol2\n  a = 1\n  b = 2"

test4 :: String
test4 = "a = 1"

test5 :: String
test5 =  """public a = 1
b = 2
c = 3"""

test6 :: String
test6 = """:Aangifte :aangifte1
  :status = "voltooid"
  :Aangever => :Jansen
"""

test7 :: String
test7 = """:Aangifte :aangifte1
  :status = "voltooid"
  :Aangever => :Jansen
    :betrouwbaarheid = 10
"""
test8 :: String
test8 = """:Aangifte :aangifte1
  :status = "voltooid"
  :urgentie = 5
  :Aangever => :Jansen
    :betrouwbaarheid = 10
"""

test9 :: String
test9 = """private :betrouwbaarheid
  :isFunctional = true
  :isVerplicht = false
"""

test10 :: String
test10 = """public :urgentie
  :isFunctional = true
  :isVerplicht = true
  :range = Number
"""
