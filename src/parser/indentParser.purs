module Perspectives.IndentParser where
  -- type Parser s = ParserT s Identity

import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Either (Either)
import Data.Identity (Identity)
import Prelude (($))
import Text.Parsing.Indent (IndentParser, runIndent)
import Text.Parsing.Parser (ParseError, Parser, ParserT, runParserT)
import Text.Parsing.Parser.Pos (Position)

-- type Parser s = ParserT s Identity
type P a = Parser String a

-- type IndentParser s a = ParserT s (State Position) a
-- type IP a = IndentParser String a
type IP a e = ParserT String (StateT Position (Aff e)) a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a e. String -> IndentParser (Aff e) String a -> Aff e (Either ParseError a)
runIndentParser s p = runIndent $ runParserT s p
