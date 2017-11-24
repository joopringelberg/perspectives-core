module Perspectives.Parser where

import Perspectives.IndentParser
import Control.Alt ((<|>))
import Data.Array (cons, fromFoldable) as AR
import Data.Foldable (elem)
import Data.List (filter, foldr, many)
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, insert, fromFoldable)
import Data.Tuple (Tuple(..))
import Perspectives.Guid (guid)
import Perspectives.Syntax (ContextDefinition(..), Expr(..), PropertyAssignment(..), PropertyDefinition(..), RolAssignment(..), RolAssignmentWithPropertyAssignments(..), RolDefinition(..), SimpleValue(..))
import Perspectives.Syntax2 (PerspectEntity(..), BinnenRol(..), ContextCollection(..), PerspectContext(..), PerspectRol(..))
import Perspectives.Token (token)
import Prelude (Unit, bind, discard, pure, show, unit, ($), ($>), (*>), (<$>), (<*), (<*>), (<<<), (<>), (==), (>>=))
import Text.Parsing.Indent (block, checkIndent, indented, sameOrIndented, withPos)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (between, optionMaybe, try)
import Text.Parsing.Parser.String (whiteSpace)
import Text.Parsing.Parser.String (string) as S

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
dataType = try do
  s <- identifier
  if elem s dataTypes then pure $ String s else fail "Expected one of 'Number', 'String', 'Bool' or 'Date'."
-----------------------------------------------------------
-- Assignment
-----------------------------------------------------------

-- | identifier = simpleValue
propertyAssignment :: IP PropertyAssignment
propertyAssignment = privatePropertyAssignment <|> publicPropertyAssignment

typedPropertyAssignment :: String -> IP PropertyAssignment
typedPropertyAssignment scope = withPos $ {name: _, scope: _, value: _}
    <$> identifier
    <*> (sameOrIndented *> reservedOp "=")
    <*> (sameOrIndented *> (simpleValue <|> dataType))
  >>= (pure <<< PropertyAssignment <<< (\r -> r {scope = scope}))

publicPropertyAssignment :: IP PropertyAssignment
publicPropertyAssignment = typedPropertyAssignment "public"

privatePropertyAssignment :: IP PropertyAssignment
privatePropertyAssignment = withPos $ between (S.string "(") (S.string ")") (typedPropertyAssignment "private") <* whiteSpace

-- rolAssignment = type '=>' identifier BLOCK propertyAssignment*
-- Consumes a rol binding expression followed by zero or more property assignment expressions.
rolAssignment :: IP RolAssignment
rolAssignment = withPos do
  typeName <- identifier
  sameOrIndented *> reservedOp "=>"
  ident <- sameOrIndented *> identifier
  pure (RolAssignment {name: typeName, binding: ident})

-- rolAssignmentWithPropertyAssignments = type '=>' identifier BLOCK propertyAssignment*
-- Consumes a rol binding expression followed by zero or more property assignment expressions.
rolAssignmentWithPropertyAssignments :: IP RolAssignmentWithPropertyAssignments
rolAssignmentWithPropertyAssignments = withPos do
  typeName <- sameOrIndented *> identifier
  sameOrIndented *> reservedOp "=>"
  ident <- sameOrIndented *> identifier
  props <- withPos (block $ checkIndent *> propertyAssignment)
  pure (RolAssignmentWithPropertyAssignments {rolInContextType: typeName, binding: ident, properties: props})

propertyAssignmentList :: String -> IP (List PropertyAssignment)
propertyAssignmentList keyword = withPos do
  reserved keyword
  first <- sameOrIndented *> propertyAssignment
  rest <- indented *> block propertyAssignment
  pure (Cons first rest)

-- propertyDefinition = ('public' | 'private') identifier BLOCK propertyAssignment*
propertyDefinition :: IP PropertyDefinition
propertyDefinition = withPos do
  scope <- theScope
  name <- sameOrIndented *> identifier
  props <- indented *> (block propertyAssignment)
  pure $ PropertyDefinition {scope: scope, name: name, properties: props}

theScope :: IP String
theScope = try do
  private <- optionMaybe (sameOrIndented *> reserved "private")
  case private of
    Nothing -> do
      public <- optionMaybe (sameOrIndented *> reserved "public")
      case public of
        Nothing -> fail "Expected 'private' or 'public'"
        otherwise -> pure "public"
    otherwise -> pure "private"
-----------------------------------------------------------
-- RolDefinition
-----------------------------------------------------------

-- rolDefinition = type identifier BLOCK
-- 	rolAssignmentWithPropertyAssignments
-- 	propertyDefinition*
rolDefinition :: IP RolDefinition
rolDefinition = withPos do
  typeName <- identifier
  definedTypeName <- sameOrIndented *> identifier
  binding <- indented *> rolAssignment
  propDefs <- indented *> (block propertyDefinition)
  pure $ RolDefinition {id: definedTypeName, rolType: typeName, binding: binding, properties: propDefs}

-----------------------------------------------------------
-- Context and ContextDefinition
-----------------------------------------------------------

-- context = type identifier BLOCK propertyAssignment* rolAssignmentWithPropertyAssignments*
context :: IP ContextCollection
context = withPos do
  typeName <- sameOrIndented *> identifier
  instanceName <- sameOrIndented *> identifier
  indented *> withPos do
    props <- (block propertyAssignment)
    roles <- (block rolAssignmentWithPropertyAssignments)
    let
      rollen = (constructRoles instanceName roles)
      ctxt = PerspectContext
        { id: instanceName
        , pspType: typeName
        , binnenRol: binnenRol
        , buitenRol: instanceName <> "_buitenRol"
        , rolInContext: (\(PerspectRol{id}) -> id) <$> rollen }

      binnenRol = BinnenRol
        { id: instanceName <> "_binnenRol"
        , pspType: ":BinnenRol"
        , binding: Just $ instanceName <> "_buitenRol"
        , properties: constructProperties "private" props
        }

      buitenRol = PerspectRol
        { id: instanceName <> "_buitenRol"
        , pspType: ":BuitenRol"
        , binding: Nothing
        , context: instanceName
        , properties: constructProperties "public" props
        , gevuldeRollen: empty
        }
    pure $ ContextCollection $ fromFoldable $
      AR.cons (Tuple instanceName (Context ctxt))
      ((\rol@(PerspectRol r) -> Tuple r.id (Rol rol)) <$> AR.cons buitenRol rollen)

    where
      constructProperties :: String ->  List PropertyAssignment -> StrMap (Array String)
      constructProperties scope assignments = foldr addProp empty assignments where
        addProp :: PropertyAssignment -> StrMap (Array String) -> StrMap (Array String)
        addProp (PropertyAssignment{name, scope: s, value}) strmap =
          case scope == s of
            true -> insert name [show value] strmap
            false -> strmap

      constructRoles :: String -> List RolAssignmentWithPropertyAssignments -> Array PerspectRol
      constructRoles contextID rpas = rpaToRol <$> AR.fromFoldable rpas where
        rpaToRol :: RolAssignmentWithPropertyAssignments -> PerspectRol
        rpaToRol (RolAssignmentWithPropertyAssignments{ rolInContextType, binding, properties}) = PerspectRol
          { id: guid unit
          , pspType: rolInContextType
          , binding: Just binding
          , context: contextID
          , properties: constructProperties "public" properties
          , gevuldeRollen: empty
          }


-- contextDefinition = DEF type identifier BLOCK
-- 	privatePropertyDefinition*
-- 	publicPropertyDefinition*
-- 	rolDefinition*
contextDefinition :: IP ContextDefinition
contextDefinition = withPos do
  _ <- reserved "DEF"
  typeName <- sameOrIndented *> identifier
  definedTypeName <- sameOrIndented *> identifier
  indented *> withPos do
    propDefs <- many (checkIndent *> propertyDefinition)
    roles <- many rolDefinition
    pure $ ContextDefinition {id: definedTypeName
                  ,contextType: typeName
                  , privateProperties: filter (\(PropertyDefinition{scope}) -> scope == "private") propDefs
                  , publicProperties: filter (\(PropertyDefinition{scope}) -> scope == "public") propDefs
                  , rolDefinitions: roles }

-----------------------------------------------------------
-- File and expr
-----------------------------------------------------------
expr :: IP Expr
expr = CtxtDef <$> contextDefinition <|> Ctxt <$> context

file :: IP (List Expr)
file = many expr

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
  (:urgentie = 5)
  :Aangever => :Jansen
    :betrouwbaarheid = 10
"""
-- runIndentParser test8 context

test8a :: String
test8a = """:Aangifte :aangifte1
  (:status = "voltooid")
  :urgentie = 5
  :Aangever => :Jansen
    :betrouwbaarheid = 10
"""
-- runIndentParser test8a context

test9 :: String
test9 = """private :betrouwbaarheid
  :isFunctional = true
    :isVerplicht = false
"""
-- runIndentParser test9 propertyDefinition

test10 :: String
test10 = """public :urgentie
  :isFunctional = true
  :isVerplicht = true
  :range = Number
"""
test11 :: String
test11 = """public :urgentie
  :range = Number
"""

-- runIndentParser (test9 <> test10 <> test11) (block propertyDefinition)
test12 :: String
test12 = """:RolInContextType :Aangever
  :MogelijkeBinding => :Gebruiker
  public :displayName
    :range = String
  private :betrouwbaarheid
    :isFunctional = true
    :isVerplicht = true
    :range = Number
etc
"""
-- runIndentParser test12 rolDefinition

test13 :: String
test13 = """DEF :ContextType :Aangifte
	private :status
		:isFunctional = true
		:isVerplicht = true
		:range = "String"
	private :comment
		:isFunctional = true
		:isVerplicht = false
		:range = "String"
	public :urgentie
		:isFunctional = true
		:isVerplicht = true
		:range = "Int"
  :RolInContextType :Aangever
    :MogelijkeBinding => :Gebruiker
    public :displayName
      :range = String
    private :betrouwbaarheid
      :isFunctional = true
      :isVerplicht = true
      :range = Number

"""
-- runIndentParser test13 contextDefinition

test14 :: String
test14 = test13 <> test8
-- runIndentParser test14 file
