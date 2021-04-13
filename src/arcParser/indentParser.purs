-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Parsing.Arc.IndentParser where

import Prim.Row

import Control.Alt (void, (<|>))
import Control.Monad.State (StateT, evalStateT, gets, lift, modify)
import Control.Monad.State.Trans (get, put)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List(..), many, singleton)
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Perspectives.Parsing.Arc.AST (StateTransitionE(..))
import Perspectives.Parsing.Arc.Expression.AST (Step)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Representation.State (StateIdentifier(..))
import Prelude (class Monad, Unit, bind, discard, flip, pure, unit, ($), (*>), (<), (<*), (<<<), (<=), (>), (>>=), (<>))
import Record (get, set) as Record
import Text.Parsing.Indent (IndentParser, checkIndent, runIndent, sameLine, withPos)
import Text.Parsing.Parser (ParseError, ParseState(..), fail, runParserT)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (eof)
import Unsafe.Coerce (unsafeCoerce)

-- | This is the type that is produced by Perspectives.Parsing.TransferFile.
-- type IndentParser m s a = ParserT s (StateT Position m) a
-- type IP a = IndentParser Identity String a

-- | The StateIdentifier holds a fully qualified context name directly after parsing the
-- | context declaration.
-- | Directly after parsing the state declaration, the StateIdentifier holds a fully qualified name
-- | consisting of the surrounding context name and the state identifier given in the declaration.
-- | (if we allow States to be nested, their names will be nested, too, providing a kind of scope).
-- | Subject may be a qualified name (from the role declaration). However, it may also be unqualified or partly
-- | qualified (from the "perspective of" expression).
type ArcParserState =
  { subject :: Maybe String
  , object :: Maybe Step
  , state :: StateIdentifier
  , onEntry :: Maybe StateTransitionE
  , onExit :: Maybe StateTransitionE
  }

type ArcParser = StateT ArcParserState Identity
type IP a = IndentParser ArcParser String a

initialArcParserState :: ArcParserState
initialArcParserState = {subject: Nothing, object: Nothing, state: AllStates "", onEntry: Nothing, onExit: Nothing}

getArcParserState :: IP ArcParserState
getArcParserState = lift $ lift $ get

modifyArcParserState :: (ArcParserState -> ArcParserState) -> IP ArcParserState
modifyArcParserState f = lift $ lift $ modify f

withArcParserState :: forall a. StateIdentifier -> IP a -> IP a
withArcParserState stateId a = do
  oldState@{state} <- getArcParserState
  lift $ lift $ put initialArcParserState {state = state <> stateId}
  result <- a
  lift $ lift $ put oldState
  pure result

setLabel :: forall a l r'. IsSymbol l =>
  Cons l (Maybe a) r' (subject :: Maybe String
  , object :: Maybe Step
  , state :: StateIdentifier
  , onEntry :: Maybe StateTransitionE
  , onExit :: Maybe StateTransitionE
  ) =>
  SProxy l -> a -> IP Unit
setLabel l newValue = do
  (oldValue :: Maybe a) <- getArcParserState >>= pure <<< Record.get l
  if isNothing oldValue
    then void $ modifyArcParserState \s -> Record.set l (Just newValue) s
    else fail (reflectSymbol l <> " is already specified in the context")

setSubject :: String -> IP Unit
setSubject = setLabel (SProxy :: SProxy "subject")

setObject :: Step -> IP Unit
setObject = setLabel (SProxy :: SProxy "object")

protectLabel :: forall a l r' b. IsSymbol l =>
  Cons l (Maybe a) r' (subject :: Maybe String
  , object :: Maybe Step
  , state :: StateIdentifier
  , onEntry :: Maybe StateTransitionE
  , onExit :: Maybe StateTransitionE
  ) =>
  SProxy l -> IP b -> IP b
protectLabel l f = do
  (oldValue :: Maybe a) <- getArcParserState >>= pure <<< Record.get l
  void $ lift $ lift $ (modify \s -> Record.set l Nothing s)
  result <- f
  void $ lift $ lift $ modify \s -> Record.set l oldValue s
  pure result

protectSubject :: forall a. IP a -> IP a
protectSubject = protectLabel (SProxy :: SProxy "subject")

protectObject :: forall a. IP a -> IP a
protectObject = protectLabel (SProxy :: SProxy "object")

protectOnEntry :: forall a. IP a -> IP a
protectOnEntry = protectLabel (SProxy :: SProxy "onEntry")

protectOnExit :: forall a. IP a -> IP a
protectOnExit = protectLabel (SProxy :: SProxy "onExit")

-- | Given a local state name, create and store in state a Transition with a
--  fully qualified state identifier.
setOnEntry :: String -> IP Unit
setOnEntry localStateName = do
  {onEntry, state} <- getArcParserState
  if isNothing onEntry then void $ modifyArcParserState \s -> s {onEntry = Just (Entry (state <> (State_ localStateName)))} else fail "on entry is already specified"

-- | Given a local state name, create and store in state a Transition with ado
--  fully qualified state identifier.
setOnExit :: String -> IP Unit
setOnExit localStateName = do
  {onExit, state} <- getArcParserState
  if isNothing onExit then void $ modifyArcParserState \s -> s {onExit = Just (Exit (state <> (State_ localStateName)))} else fail "on exit is already specified"

-- | Apply a parser, keeping only the parsed result.
runIndentParser :: forall a. String -> IP a -> Identity (Either ParseError a)
runIndentParser s p = flip evalStateT initialArcParserState (runIndent (runParserT s p))

onSameLine :: forall m s a. Monad m => IndentParser m s a -> IndentParser m s a
onSameLine p = withPos (p <* sameLine)

-- | Returns the position as recorded in `ParseState`
getPosition :: IP ArcPosition
getPosition = gets \(ParseState _ pos _) -> position2ArcPosition pos

-- | Returns the position as recorded by the IndentParser.
get' :: IP ArcPosition
get' = do
  g <- lift get
  pure $ position2ArcPosition g

-- | Records the position with the IndentParser.
put' :: ArcPosition -> IP Unit
put' p = lift (put $ arcPosition2Position p)

upperLeft :: ArcPosition
upperLeft = ArcPosition{line: 0, column: 0}

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
