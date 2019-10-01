module Perspectives.Parsing.Arc.IndentParser where
  -- type Parser s = ParserT s Identity

import Control.Monad.State (gets)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity)
import Kishimen (genericSumToVariant)
import Prelude (class Monad, class Show, (<*), class Eq, (<<<))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Text.Parsing.Indent (IndentParser, runIndent, sameLine, withPos)
import Text.Parsing.Parser (ParseError, ParseState(..), runParserT)
import Unsafe.Coerce (unsafeCoerce)

-- | This is the type that is produced by Perspectives.Parsing.Arc.Simple.
-- type IndentParser m s a = ParserT s (StateT Position m) a
type IP a = IndentParser Identity String a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a. String -> IP a -> Identity (Either ParseError a)
runIndentParser s p = runIndent (runParserT s p)

onSameLine :: forall m s a. Monad m => IndentParser m s a -> IndentParser m s a
onSameLine p = withPos (p <* sameLine)

getPosition :: IP ArcPosition
getPosition = gets \(ParseState _ pos _) -> unsafeCoerce pos

derive instance genericArcPosition :: Generic ArcPosition _
instance showArcPosition :: Show ArcPosition where show = genericShow
derive instance eqArcPosition :: Eq ArcPosition
instance writeForeignArcPosition :: WriteForeign ArcPosition where
  writeImpl = writeImpl <<< genericSumToVariant
instance readForeignArcPosition :: ReadForeign ArcPosition where
  readImpl f = readImpl f


-- | `Position` represents the position of the parser in the input.
-- |
-- | - `line` is the current line in the input
-- | - `column` is the column of the next character in the current line that will be parsed
newtype ArcPosition = ArcPosition
  { line :: Int
  , column :: Int
  }
