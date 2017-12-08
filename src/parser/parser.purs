module Perspectives.Parser where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Perspectives.ContextRoleParser (context, expression)
import Perspectives.IndentParser (runIndentParser)
import Perspectives.Syntax (NamedEntityCollection(..))
import Prelude (show, (*>), (-))
import Text.Parsing.Parser (ParseError(..))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (whiteSpace)

type AceError =
  { row :: Int
  , column :: Int
  , text :: String
  , type :: String }

parse :: String -> Either (Array AceError) String
parse s = case runIndentParser s context of
  (Left (ParseError message (Position{line, column}))) -> Left [
    { row: line - 1
    , column: column
    , text: message
    , type: "error"
    }]
  (Right (NamedEntityCollection _ j)) -> Right (show j)

errorsIn :: String -> String -> Maybe (Array AceError)
errorsIn previousLine s = case runIndentParser s (whiteSpace *> expression) of
  (Left (ParseError message (Position{line, column}))) -> Just [
    { row: line - 1
    , column: column
    , text: expressionTypeForNextLine previousLine
    , type: "error"
    }]
  otherwise -> Nothing

expressionTypeForNextLine :: String -> String
expressionTypeForNextLine s = case runIndentParser s (whiteSpace *> expression) of
  (Left _) -> "Verwacht: -- commentaar of: Type Instantie"
  (Right etype) -> case etype of
    "typeDeclaration" -> "Verwacht: (public|private) property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
    "publicContextPropertyAssignment" -> "Verwacht: public|private property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
    "privateContextPropertyAssignment" -> "Verwacht: private property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
    "rolePropertyAssignment" -> "Verwacht: property = value"
    "roleBinding" -> "Verwacht: property = value"
    "oneLineComment" -> "Alles kan."
    otherwise -> "Verwacht: -- commentaar of: Type Instantie"
