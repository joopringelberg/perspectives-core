-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.Parsing.Arc.IndentParser where

import Control.Alt ((<|>))
import Control.Monad.State (gets, lift)
import Control.Monad.State.Trans (get, put)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity)
import Data.List (List(..), many, singleton)
import Kishimen (genericSumToVariant)
import Prelude (class Eq, class Monad, class Show, Unit, bind, discard, pure, ($), (*>), (<), (>), (<*), (<<<), (<=), (>>=), unit)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Text.Parsing.Indent (IndentParser, checkIndent, runIndent, sameLine, withPos)
import Text.Parsing.Parser (ParseError, ParseState(..), fail, runParserT)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (eof)
import Unsafe.Coerce (unsafeCoerce)

-- | This is the type that is produced by Perspectives.Parsing.TransferFile.
-- type IndentParser m s a = ParserT s (StateT Position m) a
type IP a = IndentParser Identity String a

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a. String -> IP a -> Identity (Either ParseError a)
runIndentParser s p = runIndent (runParserT s p)

onSameLine :: forall m s a. Monad m => IndentParser m s a -> IndentParser m s a
onSameLine p = withPos (p <* sameLine)

-- | Returns the position as recorded in `ParseState`
getPosition :: IP ArcPosition
getPosition = gets \(ParseState _ pos _) -> unsafeCoerce pos

-- | Returns the position as recorded by the IndentParser.
get' :: IP ArcPosition
get' = do
  g <- lift get
  pure $ unsafeCoerce g

-- | Records the position with the IndentParser.
put' :: ArcPosition -> IP Unit
put' p = lift (put $ unsafeCoerce p)

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

arcPosition2Position :: ArcPosition -> Position
arcPosition2Position = unsafeCoerce

position2ArcPosition :: Position -> ArcPosition
position2ArcPosition = unsafeCoerce

-- | `withBlock f a p` parses `a`
-- | followed by an indented block of `p`, where the entire block must have been consumed,
-- | combining them with `f`.
withEntireBlock :: forall a b c. (a -> List b -> c) -> IP a -> IP b -> IP c
withEntireBlock f a p = withPos $ do
    r1 <- a
    isIndented' <- isIndented
    r2 <- if isIndented' then entireBlock p else pure Nil
    pure (f r1 r2)

-- | Parses a block of lines at the same indentation level.
-- | If not every line has been parsed, fails by applying p to the next line
entireBlock :: forall x. IP x -> IP (List x)
entireBlock pr = (withPos (many $ checkIndent *> pr) >>= unlessOutdented (pr >>= pure <<< singleton))

-- | Applies parser `p` iff the current location of the parser is not outdented with respect
-- | to the reference position stored previously by the IndentParser (and the end of the input
-- | has not been reached). Otherwise returns a.
unlessOutdented :: forall a. IP a -> a -> IP a
unlessOutdented p a = do
    (indentParserPosition :: ArcPosition) <- get'
    (parserPosition :: ArcPosition) <- getPosition
    if sourceColumn parserPosition < sourceColumn indentParserPosition then pure a else (eof *> pure a <|> p)

-- | True iff the Parser Position is further to the right than the reference position.
-- | Sets the reference position to the current Parser Position.
isIndented :: IP Boolean
isIndented = do
    (indentParserPosition :: ArcPosition) <- get'
    (parserPosition :: ArcPosition) <- getPosition
    if sourceColumn parserPosition <= sourceColumn indentParserPosition
      then pure false
      else do
        put' $ setSourceLine parserPosition (sourceLine indentParserPosition)
        pure true

sourceColumn :: ArcPosition -> Int
sourceColumn (ArcPosition {line: _, column: c}) = c

sourceLine :: ArcPosition -> Int
sourceLine (ArcPosition {line: l, column: _}) = l

setSourceLine :: ArcPosition -> Int -> ArcPosition
setSourceLine (ArcPosition {line: _, column: c}) l = ArcPosition {line: l, column: c}

-- | Parses only on lines that are lower than the reference (unless the end of the input stream has been reached).
nextLine :: IP Unit
nextLine = do
    pos <- getPosition
    s   <- get'
    if sourceLine pos > sourceLine s then pure unit else (eof <|> fail "not on the next line")
