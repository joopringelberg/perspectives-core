module Perspectives.IndentParser where
  -- type Parser s = ParserT s Identity

import Control.Monad.State (State)
import Data.Either (Either)
import Prelude (($))
import Text.Parsing.Indent (IndentParser, runIndent)
import Text.Parsing.Parser (ParseError, Parser, ParserT, runParserT)
import Text.Parsing.Parser.Pos (Position)

-- type Parser s = ParserT s Identity
type P a = Parser String a

-- type IndentParser s a = ParserT s (State Position) a
-- type IP a = IndentParser String a
type IP a = ParserT String (State Position) a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a. String -> IndentParser String a -> (Either ParseError a)
runIndentParser s p = runIndent $ runParserT s p
