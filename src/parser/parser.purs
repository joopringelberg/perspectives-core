module Perspectives.Parser where

import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Perspectives.ContextRoleParser (expression, enclosingContext)
import Perspectives.IndentParser (runIndentParser)
import Perspectives.ResourceTypes (DomeinFileEffects)
import Prelude ((*>), (-), bind, pure, ($))
import Text.Parsing.Parser (ParseError(..))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (whiteSpace)

type AceError =
  { row :: Int
  , column :: Int
  , text :: String
  , type :: String }

parse :: forall e. String -> Aff (DomeinFileEffects e) (Either (Array AceError) String)
parse s = do
  parseResult <- runIndentParser s enclosingContext
  case parseResult of
    (Left (ParseError message (Position{line, column}))) -> pure $ Left [
      { row: line - 1
      , column: column
      , text: message
      , type: "error"
      }]
    otherwise -> pure $ Right "Parse succeeded, resources stored!"

errorsIn :: forall e. String -> String -> Aff (DomeinFileEffects e) (Maybe (Array AceError))
errorsIn previousLine s = do
  parseResult <- runIndentParser s (whiteSpace *> expression)
  case parseResult of
    (Left (ParseError _ (Position{line, column}))) -> do
      message <- expressionTypeForNextLine previousLine
      pure $ Just [
      { row: line - 1
      , column: column
      , text: message
      , type: "error"
      }]
    otherwise -> pure Nothing

expressionTypeForNextLine :: forall e. String -> Aff (DomeinFileEffects e) String
expressionTypeForNextLine s = do
  parseResult <- runIndentParser s (whiteSpace *> expression)
  case parseResult of
    (Left _) -> pure "Verwacht: -- commentaar of: Type Instantie"
    (Right etype) -> pure $ case etype of
      "textDeclaration" -> "Verwacht: context."
      "contextDeclaration" -> "Verwacht: (public|private) property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
      "publicContextPropertyAssignment" -> "Verwacht: public|private property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
      "privateContextPropertyAssignment" -> "Verwacht: private property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
      "rolePropertyAssignment" -> "Verwacht: property = value"
      "isRoleDeclaration" -> "Verwacht: property = value"
      "oneLineComment" -> "Alles kan."
      otherwise -> "Verwacht: -- commentaar of: Type Instantie"
