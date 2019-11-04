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

module Perspectives.Parsing.Arc.Expression where

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array (many)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.JSDate (JSDate, parse, toDateTime)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits as SCU
import Effect.Unsafe (unsafePerformEffect)
import Perspectives.Parsing.Arc.Expression.AST (Assignment(..), AssignmentOperator(..), BinaryStep(..), Operator(..), SimpleStep(..), Step(..), UnaryStep(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, boolean, reserved)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..), IP, getPosition)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Prelude ((<$>), (<*>), ($), pure, (*>), bind, discard, (<*), (>), (+), (>>=), (<<<), show)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (between, optionMaybe, try, (<?>))
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (alphaNum)

step :: IP Step
step = do
  start <- getPosition
  left <- token.parens step <|> leftSide
  mop <- optionMaybe (try operator)
  case mop of
    Nothing -> pure left
    (Just op) -> do
      right <- step
      end <- getPosition
      case right of
        (Binary (BinaryStep {left: leftOfRight, operator:opOfRight, right: rightOfRight, end: endOfRight})) -> if (operatorPrecedence op) > (operatorPrecedence opOfRight)
          then pure $ Binary $ BinaryStep
            { start
            , end -- equals endOfRight.
            , left: Binary (BinaryStep {start, end: endOf(leftOfRight), operator: op, left: left, right: leftOfRight})
            , operator: opOfRight
            , right: rightOfRight
          }
          else pure $ Binary $ BinaryStep {start, end, left, operator: op, right}
        otherwise -> pure $ Binary $ BinaryStep {start, end, left, operator: op, right}
leftSide :: IP Step
leftSide = defer \_ -> reserved "filter" *> step <|> defer \_ -> unaryStep <|> simpleStep

simpleStep :: IP Step
simpleStep = try
  (Simple <$> (ArcIdentifier <$> getPosition <*> arcIdentifier)
  <|>
  Simple <$> (Binding <$> (getPosition <* reserved "binding"))
  <|>
  Simple <$> (Binder <$> (getPosition <* reserved "binder") <*> arcIdentifier)
  <|>
  Simple <$> (Context <$> (getPosition <* reserved "context"))
  <|>
  Simple <$> (Extern <$> (getPosition <* reserved "extern"))
  <|>
  Simple <$> (Value <$> getPosition <*> pure PDate <*> (parseDate >>= pure <<< show))
  <|>
  Simple <$> (Value <$> getPosition <*> pure PString <*> token.stringLiteral)
  <|>
  Simple <$> (Value <$> getPosition <*> pure PBool <*> boolean)
  <|>
  Simple <$> (Value <$> getPosition <*> pure PNumber <*> (token.integer >>= pure <<< show))
  -- TODO: date
  <|>
  Simple <$> (CreateContext <$> getPosition <*> (reserved "createContext" *> (defer \_ -> arcIdentifier)))
  <|>
  Simple <$> (CreateEnumeratedRole <$> getPosition <*> (reserved "createRole" *> (defer \_ -> arcIdentifier)))
  <|>
  Simple <$> (SequenceFunction <$> getPosition <*> sequenceFunction)
  <|>
  Simple <$> (Identity <$> getPosition <* reserved "this")
  ) <?> "binding, binder, context, extern, this, a valid identifier or a number, boolean, string (between double quotes), date (between single quotes) or a monoid function (sum, product, minimum, maximum) or count"

sequenceFunction :: IP String
sequenceFunction = (token.symbol "sum" <|> token.symbol "product" <|> token.symbol "minimum" <|> token.symbol "maximum" <|> token.symbol "count") <?> "sum, product, minimum,\
\ maximum or count"


-- | Parse a date. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse#Date_Time_String_Format for the supported string format of the date.
parseDate :: IP DateTime
parseDate = try do
  s <- dateTimeLiteral
  (d :: JSDate) <- pure $ unsafePerformEffect $ parse s
  case toDateTime d of
    Nothing -> fail "Not a date"
    (Just (dt :: DateTime)) -> pure dt

unaryStep :: IP Step
unaryStep = try
  (Unary <$> (LogicalNot <$> getPosition <*> (reserved "not" *> (defer \_ -> step)))
  <|>
  Unary <$> (Exists <$> getPosition <*> (reserved "exists" *> (defer \_ -> step)))) <?> "not <expr> or exists <step>."

operator :: IP Operator
operator =
  (Filter <$> (getPosition <* reserved "with"))
  <|>
  (Sequence <$> (getPosition <* token.reservedOp ">>="))
  <|>
  ((Compose <$> (getPosition <* token.reservedOp ">>"))
  <|>
  (Equals <$> (getPosition <* token.reservedOp "=="))
  <|>
  (NotEquals <$> (getPosition <* token.reservedOp "/="))
  <|>
  (LessThan <$> (getPosition <* token.reservedOp "<"))
  <|>
  (LessThanEqual <$> (getPosition <* token.reservedOp "<="))
  <|>
  (GreaterThan <$> (getPosition <* token.reservedOp ">"))
  <|>
  (GreaterThanEqual <$> (getPosition <* token.reservedOp ">="))
  <|>
  (LogicalAnd <$> (getPosition <* token.reservedOp "and"))
  <|>
  (LogicalOr <$> (getPosition <* token.reservedOp "or"))
  <|>
  (Add <$> (getPosition <* token.reservedOp "+"))
  <|>
  (Subtract <$> (getPosition <* token.reservedOp "-"))
  <|>
  (Divide <$> (getPosition <* token.reservedOp "/"))
  <|>
  (Multiply <$> (getPosition <* token.reservedOp "*"))
  ) <?> ">>, ==, /=, <, <=, >, >=, and, or, +, -, /, *, >>="

operatorPrecedence :: Operator -> Int
operatorPrecedence (Compose _) = 8
operatorPrecedence (Equals _) = 0
operatorPrecedence (NotEquals _) = 0
operatorPrecedence (LessThan _) = 1
operatorPrecedence (LessThanEqual _) = 1
operatorPrecedence (GreaterThan _) = 1
operatorPrecedence (GreaterThanEqual _) = 1
operatorPrecedence (LogicalAnd _) = 1
operatorPrecedence (LogicalOr _) = 2
operatorPrecedence (Add _) = 3
operatorPrecedence (Subtract _) = 2
operatorPrecedence (Divide _) = 4
operatorPrecedence (Multiply _) = 5
operatorPrecedence (Sequence _) = 8
operatorPrecedence (Filter _) = 9

startOf :: Step -> ArcPosition
startOf stp = case stp of
  (Simple s) -> startOfSimple s
  (Binary (BinaryStep{start})) -> start
  (Unary us) -> startOfUnary us

  where
    startOfSimple (ArcIdentifier p _) = p
    startOfSimple (Value p _ _) = p
    startOfSimple (Binding p) = p
    startOfSimple (Binder p _) = p
    startOfSimple (Context p) = p
    startOfSimple (Extern p) = p
    startOfSimple (CreateContext p _) = p
    startOfSimple (CreateEnumeratedRole p _) = p
    startOfSimple (SequenceFunction p _) = p
    startOfSimple (Identity p) = p

    startOfUnary (LogicalNot p _) = p
    startOfUnary (Exists p _) = p

endOf :: Step -> ArcPosition
endOf stp = case stp of
  (Simple s) -> endOfSimple s
  (Binary (BinaryStep{end})) -> end
  (Unary us) -> endOfUnary us

  where
    endOfSimple (ArcIdentifier (ArcPosition{line, column}) id) = ArcPosition{line, column: column + length id}
    endOfSimple (Value (ArcPosition{line, column}) _ v) = ArcPosition({line, column: column + length v + 1})
    endOfSimple (Binding (ArcPosition{line, column})) = ArcPosition{line, column: column + 7}
    endOfSimple (Binder (ArcPosition{line, column}) _) = ArcPosition{line, column: column + 6}
    endOfSimple (Context (ArcPosition{line, column})) = ArcPosition{line, column: column + 7}
    endOfSimple (Extern (ArcPosition{line, column})) = ArcPosition{line, column: column + 6}
    endOfSimple (CreateContext (ArcPosition{line, column}) ident) = ArcPosition{ line, column: column + length ident + 7}
    endOfSimple (CreateEnumeratedRole (ArcPosition{line, column}) ident) = ArcPosition{ line, column: column + length ident + 7}
    endOfSimple (SequenceFunction (ArcPosition{line, column}) fname) = ArcPosition{line, column: column + length fname}
    endOfSimple (Identity (ArcPosition{line, column})) = ArcPosition{line, column: column + 4}

    -- Note that this assumes a single whitespace between 'not' and the step.
    endOfUnary (LogicalNot (ArcPosition{line, column}) step') = ArcPosition{line: line_(endOf step'), column: col_(endOf step') + 4}
    endOfUnary (Exists (ArcPosition{line, column}) step') = endOf step'

    col_ :: ArcPosition -> Int
    col_ (ArcPosition{column}) = column

    line_ :: ArcPosition -> Int
    line_ (ArcPosition{line}) = line

assignment :: IP Assignment
assignment = deletion <|> try do
  start <- getPosition
  lhs <- arcIdentifier
  op <- assignmentOperator
  val <- step
  end <- getPosition
  pure $ Assignment {start, end, lhs, operator: op, value: Just val}

assignmentOperator :: IP AssignmentOperator
assignmentOperator = try
  (DeleteFrom <$> (getPosition <* token.reservedOp "=-"))
  <|>
  (AddTo <$> (getPosition <* token.reservedOp "=+"))
  <|>
  ((Set <$> (getPosition <* token.reservedOp "="))
  ) <?> "=, =+, =-"

deletion :: IP Assignment
deletion = try do
  start <- getPosition
  reserved "delete"
  lhs <- arcIdentifier
  end <- getPosition
  pure $ Assignment {start, end, lhs, operator: Delete start, value: Nothing }

-- | Parse between single quotes.
dateTimeLiteral :: IP String
dateTimeLiteral = (go <?> "date-time") <* token.whiteSpace
  where
    go :: IP String
    go = do
        chars <- between (char '\'') (char '\'' <?> "end of string") (many dateChar)
        pure $ SCU.fromCharArray chars

    dateChar :: IP Char
    dateChar = alphaNum <|> char ':' <|> char '+' <|> char '-' <|> char ' ' <|> char '.'
