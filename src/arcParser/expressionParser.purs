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

module Perspectives.Parsing.Arc.Expression where

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array (elemIndex, fromFoldable, many)
import Data.DateTime (DateTime)
import Data.JSDate (JSDate, parse, toDateTime)
import Data.List (List(..))
import Data.Maybe (Maybe(..), isJust)
import Data.String (length)
import Data.String.CodeUnits as SCU
import Effect.Unsafe (unsafePerformEffect)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), Operator(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, boolean, lowerCaseName, reserved)
import Perspectives.Parsing.Arc.IndentParser (IP, entireBlock, getPosition)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Arc.Token (reservedIdentifier, token)
import Perspectives.Representation.QueryFunction (FunctionName(..))
import Perspectives.Representation.Range (Range(..))
import Prelude (bind, not, pure, show, ($), (&&), (*>), (+), (<$>), (<*), (<*>), (<<<), (>), (>>=), (<>))
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (between, lookAhead, manyTill, option, optionMaybe, try, (<?>))
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (alphaNum)

step :: IP Step
step = defer \_ -> step_ false

step_ :: Boolean -> IP Step
step_ parenthesised = do
  start <- getPosition
  left <- (token.parens (step_ true)) <|> leftSide
  mop <- optionMaybe (try operator)
  case mop of
    Nothing -> pure left
    (Just op) -> do
      right <- step
      end <- getPosition
      case right of
        -- The right expression is binary: leftOfRight <opOfRight> rightOfRight.
        (Binary (BinaryStep
          { left: leftOfRight
          , operator:opOfRight
          , right: rightOfRight
          , end: endOfRight
          , parenthesised: protected})) -> if
              not protected &&
              ((operatorPrecedence op) > (operatorPrecedence opOfRight))

          -- Regrouping: the parse tree (a op1 (b op2 c)) becomes ((a op1 b) op2 c).
          -- The expression was: "a op2 b op1 c"
          -- (op1 = operator with precedence 1, op2 = operator with precedence 2)
          -- The right expression is binary and not contained in parenthesis, and
          -- the left operator has higher precedence (than (or equal to) the right operator).
          then pure $ Binary $ BinaryStep
            { start
            , end -- equals endOfRight.
            , left: Binary (BinaryStep {start, end: endOf(leftOfRight), operator: op, left: left, right: leftOfRight, parenthesised: false})
            , operator: opOfRight
            , right: rightOfRight
            , parenthesised: false
          }

          -- No regrouping.
          -- The right expression is binary and is contained in parenthesis, OR
          -- its operator is as precedent as (or more so then) that of the enclosing binary expression.
          -- Hence, we maintain the right-association that is present in the parse tree: (a op (b op c)).
          -- The expression is either:
          --    "a opx (b opy c)"
          -- (opx and opy have any precedence; precedence does not rule, parenthesis prevail), or:
          --    "a op1 b op1 c"
          -- (both operators have equal precedence but we adhere to right-associativity)
          --    "a op1 b op2 c"
          -- (op1 = operator with precedence 1, op2 = operator with precedence 2). The parse tree already respects
          -- the operator precedences.
          else pure $ Binary $ BinaryStep {start, end, left, operator: op, right, parenthesised}

        -- The right expression is not binary. No regrouping.
        otherwise -> pure $ Binary $ BinaryStep {start, end, left, operator: op, right, parenthesised}
  where
    leftSide :: IP Step
    leftSide = do
      keyword <- option "" (lookAhead reservedIdentifier)
      case keyword of
        "filter" -> reserved "filter" *> step
        "letE" -> pureLetStep
        "callExternal" -> computationStep
        u | isUnaryKeyword u -> unaryStep
        _ -> simpleStep

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
  <|>
  Simple <$> (CreateEnumeratedRole <$> getPosition <*> (reserved "createRole" *> arcIdentifier))
  <|>
  Simple <$> (SequenceFunction <$> getPosition <*> sequenceFunction)
  <|>
  Simple <$> (Identity <$> getPosition <* reserved "this")
  <|>
  Simple <$> (Modelname <$> getPosition <* reserved "modelname")
  <|>
  Simple <$> (TypeOfContext <$> (getPosition <* reserved "contextType"))
  <|>
  Simple <$> (RoleTypes <$> (getPosition <* reserved "roleTypes"))
  <|>
  Simple <$> (SpecialisesRoleType <$> (getPosition <* reserved "specialisesRoleType") <*> arcIdentifier)

  -- VARIABLE MUST BE LAST!
  <|>
  Simple <$> (Variable <$> getPosition <*> lowerCaseName)
  ) <?> "binding, binder, context, extern, this, a valid identifier or a number, boolean, string (between double quotes), date (between single quotes) or a monoid function (sum, product, minimum, maximum) or count"

sequenceFunction :: IP FunctionName
sequenceFunction = (token.symbol "sum" *> pure AddF
  <|> token.symbol "product" *> pure MultiplyF
  <|> token.symbol "minimum" *> pure MinimumF
  <|> token.symbol "maximum" *> pure MaximumF
  <|> token.symbol "count" *> pure CountF
  ) <?> "sum, product, minimum,\
\ maximum or count"

propertyRange :: IP String
propertyRange = (reserved "Boolean" *> (pure "Boolean")
  <|> reserved "Number" *> (pure "Number")
  <|> reserved "String" *> (pure "String")
  <|> reserved "DateTime" *> (pure "DateTime")) <?> "Boolean, Number, String or DateTime"

-- | Parse a date. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse#Date_Time_String_Format for the supported string format of the date.
parseDate :: IP DateTime
parseDate = try do
  s <- dateTimeLiteral
  (d :: JSDate) <- pure $ unsafePerformEffect $ parse s
  case toDateTime d of
    Nothing -> fail "Not a date"
    (Just (dt :: DateTime)) -> pure dt

isUnaryKeyword :: String -> Boolean
isUnaryKeyword kw = isJust $ elemIndex kw ["not", "exists", "binds", "boundBy", "available"]

unaryStep :: IP Step
unaryStep = do
  keyword <- lookAhead reservedIdentifier <?> "not, exists, binds or available."
  case keyword of
    "not" -> (Unary <$> (LogicalNot <$> getPosition <*> (reserved "not" *> (defer \_ -> step))))
    "exists" -> Unary <$> (Exists <$> getPosition <*> (reserved "exists" *> (defer \_ -> step)))
    "binds" -> Unary <$> (Binds <$> getPosition <*> (reserved "binds" *> (defer \_ -> step)))
    "boundBy" -> Unary <$> (BoundBy <$> getPosition <*> (reserved "boundBy" *> (defer \_ -> step)))
    "available" -> Unary <$> (Available <$> getPosition <*> (reserved "available" *> (defer \_ -> step)))
    s -> fail ("Expected not, exists, binds or available, but found: '" <> s <> "'.")

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
  <|>
  (Union <$> (getPosition <* token.reservedOp "either"))
  <|>
  (Intersection <$> (getPosition <* token.reservedOp "both"))
  <|>
  (BindsOp <$> (getPosition <* token.reserved "binds"))
  ) <?> "with, >>=, >>, ==, /=, <, <=, >, >=, and, or, +, -, /, *, either, both, binds"

operatorPrecedence :: Operator -> Int
operatorPrecedence (Compose _) = 9

operatorPrecedence (Union _) = 8
operatorPrecedence (Intersection _) = 8
operatorPrecedence (Sequence _) = 7

operatorPrecedence (Multiply _) = 6
operatorPrecedence (Divide _) = 5

operatorPrecedence (Add _) = 4
operatorPrecedence (Subtract _) = 4

operatorPrecedence (Equals _) = 3
operatorPrecedence (NotEquals _) = 3
operatorPrecedence (LessThan _) = 3
operatorPrecedence (LessThanEqual _) = 3
operatorPrecedence (GreaterThan _) = 3
operatorPrecedence (GreaterThanEqual _) = 3
operatorPrecedence (BindsOp _) = 3

operatorPrecedence (LogicalAnd _) = 2
operatorPrecedence (LogicalOr _) = 2

-- not, exists, available, binds, boundBy 1

operatorPrecedence (Filter _) = 0

startOf :: Step -> ArcPosition
startOf stp = case stp of
  (Simple s) -> startOfSimple s
  (Binary (BinaryStep{start})) -> start
  (Unary us) -> startOfUnary us
  (PureLet (PureLetStep {start})) -> start
  (Computation (ComputationStep {start})) -> start

  where
    startOfSimple (ArcIdentifier p _) = p
    startOfSimple (Value p _ _) = p
    startOfSimple (Binding p) = p
    startOfSimple (Binder p _) = p
    startOfSimple (Context p) = p
    startOfSimple (Extern p) = p
    startOfSimple (CreateEnumeratedRole p _) = p
    startOfSimple (SequenceFunction p _) = p
    startOfSimple (Identity p) = p
    startOfSimple (Modelname p) = p
    startOfSimple (Variable p _) = p

    startOfSimple (TypeOfContext p) = p
    startOfSimple (RoleTypes p) = p
    startOfSimple (SpecialisesRoleType p _) = p

    startOfSimple (TypeTimeOnlyContext p _) = p
    startOfSimple (TypeTimeOnlyEnumeratedRole p _) = p
    startOfSimple (TypeTimeOnlyCalculatedRole p _) = p

    startOfUnary (LogicalNot p _) = p
    startOfUnary (Exists p _) = p
    startOfUnary (Binds p _) = p
    startOfUnary (BoundBy p _) = p
    startOfUnary (Available p _) = p

endOf :: Step -> ArcPosition
endOf stp = case stp of
  (Simple s) -> endOfSimple s
  (Binary (BinaryStep{end})) -> end
  (Unary us) -> endOfUnary us
  (PureLet (PureLetStep {end})) -> end
  (Computation (ComputationStep {end})) -> end

  where
    endOfSimple (ArcIdentifier (ArcPosition{line, column}) id) = ArcPosition{line, column: column + length id}
    endOfSimple (Value (ArcPosition{line, column}) _ v) = ArcPosition({line, column: column + length v + 1})
    endOfSimple (Binding (ArcPosition{line, column})) = ArcPosition{line, column: column + 7}
    endOfSimple (Binder (ArcPosition{line, column}) _) = ArcPosition{line, column: column + 6}
    endOfSimple (Context (ArcPosition{line, column})) = ArcPosition{line, column: column + 7}
    endOfSimple (Extern (ArcPosition{line, column})) = ArcPosition{line, column: column + 6}
    endOfSimple (CreateEnumeratedRole (ArcPosition{line, column}) ident) = ArcPosition{ line, column: column + length ident + 7}
    endOfSimple (SequenceFunction (ArcPosition{line, column}) fname) = ArcPosition{line, column: column + length (show fname)}
    endOfSimple (Identity (ArcPosition{line, column})) = ArcPosition{line, column: column + 4}
    endOfSimple (Modelname (ArcPosition{line, column})) = ArcPosition{line, column: column + 9}
    endOfSimple (Variable (ArcPosition{line, column}) v) = ArcPosition{line, column: column + length v}

    endOfSimple (TypeOfContext (ArcPosition{line, column})) = ArcPosition{line, column: column + 11}
    endOfSimple (RoleTypes (ArcPosition{line, column})) = ArcPosition{line, column: column + 9}
    endOfSimple (SpecialisesRoleType (ArcPosition{line, column}) ident) = ArcPosition{line, column: column + 19 + length ident}

    endOfSimple (TypeTimeOnlyContext p _) = p
    endOfSimple (TypeTimeOnlyEnumeratedRole p _) = p
    endOfSimple (TypeTimeOnlyCalculatedRole p _) = p

    -- Note that this assumes a single whitespace between 'not' and the step.
    endOfUnary (LogicalNot (ArcPosition{line, column}) step') = ArcPosition{line: line_(endOf step'), column: col_(endOf step') + 4}
    endOfUnary (Exists (ArcPosition{line, column}) step') = endOf step'
    endOfUnary (Binds (ArcPosition{line, column}) step') = endOf step'
    endOfUnary (BoundBy (ArcPosition{line, column}) step') = endOf step'
    endOfUnary (Available (ArcPosition{line, column}) step') = endOf step'

    col_ :: ArcPosition -> Int
    col_ (ArcPosition{column}) = column

    line_ :: ArcPosition -> Int
    line_ (ArcPosition{line}) = line

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

binding :: IP VarBinding
binding = VarBinding <$> (lowerCaseName <* token.reservedOp "<-") <*> defer \_ -> step

-- | A pure let: letE <binding>+ in <step>).
pureLetStep :: IP Step
pureLetStep = do
  start <- getPosition
  bindings <- reserved "letE" *> entireBlock binding
  body <- reserved "in" *> step
  end <- getPosition
  pure $ PureLet $ PureLetStep {start, end, bindings: fromFoldable bindings, body}

computationStep :: IP Step
computationStep = try do
  start <- getPosition
  functionName <- reserved "callExternal" *> arcIdentifier
  arguments <- token.symbol "(" *>
    (((token.symbol ")") *> pure Nil)
    <|>
    do
      first <- step
      -- By using manyTill we get the errir messages inside arguments to the end user.
      -- sepBy would hide them.
      rest <- manyTill (token.comma *> step) (token.symbol ")")
      pure (Cons first rest))
  computedType <- reserved "returns" *> (arcIdentifier <|> propertyRange)
  end <- getPosition
  pure $ Computation $ ComputationStep {functionName, arguments: (fromFoldable arguments), computedType, start, end}
