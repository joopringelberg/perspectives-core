module Perspectives.Parsing.Arc.IndentParser where
  -- type Parser s = ParserT s Identity

import Data.Either (Either)
import Data.Identity (Identity)
import Prelude (class Monad, (<*))
import Text.Parsing.Indent (IndentParser, runIndent, sameLine, withPos)
import Text.Parsing.Parser (ParseError, runParserT)

-- | This is the type that is produced by Perspectives.Parsing.Arc.Simple.
-- type IndentParser m s a = ParserT s (StateT Position m) a
type IP a = IndentParser Identity String a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a. String -> IP a -> Identity (Either ParseError a)
runIndentParser s p = runIndent (runParserT s p)

onSameLine :: forall m s a. Monad m => IndentParser m s a -> IndentParser m s a
onSameLine p = withPos (p <* sameLine)
