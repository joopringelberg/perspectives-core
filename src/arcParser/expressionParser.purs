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
import Data.Array (elemIndex, fold, fromFoldable, many, replicate)
import Data.DateTime (Date, DateTime(..), Hour, Time(..))
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.JSDate (JSDate, parse, toDateTime)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.String (Pattern(..), Replacement(..), length, replaceAll, trim)
import Data.String.CodeUnits as SCU
import Data.String.Regex (parseFlags, regex)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), ComputationStep(..), ComputedType(..), Operator(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.Expression.RegExP (RegExP(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, boolean, email, lowerCaseName, pubParser, regexFlags', reserved)
import Perspectives.Parsing.Arc.IndentParser (IP, entireBlock, getPosition)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Arc.Token (reservedIdentifier, token)
import Perspectives.Representation.QueryFunction (FunctionName(..))
import Perspectives.Representation.Range (Duration_(..), Range(..))
import Perspectives.Time (date2String, dateTime2String, time2String)
import Prelude (bind, not, pure, show, ($), (&&), (*>), (+), (<$>), (<*), (<*>), (<<<), (>), (>>=), (<>), eq, (/=))
import Parsing (fail)
import Parsing.Combinators (between, lookAhead, manyTill, option, optionMaybe, try, (<?>))
import Parsing.String (char, satisfy)
import Parsing.Token (alphaNum)

step :: IP Step
step = defer \_ -> step_ false

step_ :: Boolean -> IP Step
step_ parenthesised = do
  start <- getPosition
  left <- (token.parens (step_ true)) <|> leftSide
  mop <- optionMaybe (unsafePartial operator)
  case mop of
    Nothing -> do 
      mdop <- optionMaybe (unsafePartial durationOperator)
      unsafePartial case mdop of 
        Nothing -> pure left
        Just (Year pos) -> pure $ Unary (DurationOperator start (Year pos) left)
        Just (Month pos) -> pure $ Unary (DurationOperator start (Month pos) left)
        Just (Week pos) -> pure $ Unary (DurationOperator start (Week pos) left)
        Just (Day pos) -> pure $ Unary (DurationOperator start (Day pos) left)
        Just (Hour pos) -> pure $ Unary (DurationOperator start (Hour pos) left)
        Just (Minute pos) -> pure $ Unary (DurationOperator start (Minute pos) left)
        Just (Second pos) -> pure $ Unary (DurationOperator start (Second pos) left)
        Just (Millisecond pos) -> pure $ Unary (DurationOperator start (Millisecond pos) left)
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
          , parenthesised: rightParenthesised})) -> 
            if
              not rightParenthesised &&
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
        "filter" -> reserved "filter" *> step_ parenthesised
        "letE" -> pureLetStep
        "callExternal" -> computationStep
        u | isUnaryKeyword u -> unaryStep
        _ -> simpleStep

simpleStep :: IP Step
simpleStep = do 
  r <- simpleStep'
  case r of 
    id@(Simple (ArcIdentifier _ _)) -> do 
      followedByParen <- optionMaybe (lookAhead (token.symbol "("))
      case followedByParen of
        Just _ -> fail "Did you mean to use `callExternal`, `callEffect` or `callDestructiveEffect`?"
        Nothing -> pure id
    other -> pure other


simpleStep' :: IP Step
simpleStep' = 
  (
  (Simple <$> (ArcIdentifier <$> getPosition <*> arcIdentifier))
  <|>
  -- token brackets seems not to restore parser state when it fails, hence 'try'.
  Simple <$> (ContextTypeIndividual <$> getPosition <*> try (token.brackets ((reserved "context") *> arcIdentifier)))
  <|>
  Simple <$> (RoleTypeIndividual <$> getPosition <*> try (token.brackets ((reserved "role") *> arcIdentifier)))
  <|>
  Simple <$> (Filler <$> (getPosition <* reserved "binding") <*> (optionMaybe (reserved "in" *> arcIdentifier)))
  <|>
  Simple <$> (Filled <$> (getPosition <* reserved "binder") <*> arcIdentifier <*> (optionMaybe (reserved "in" *> arcIdentifier)))
  <|>
  Simple <$> (Context <$> (getPosition <* reserved "context"))
  <|>
  Simple <$> (Extern <$> (getPosition <* reserved "extern"))
  <|>
  Simple <$> (IndexedName <$> (getPosition <* reserved "indexedName"))
  <|>
  Simple <$> (Value <$> getPosition <*> pure PDateTime <*> (parseDateTime >>= pure <<< dateTime2String))
  <|>
  Simple <$> (Value <$> getPosition <*> pure PDate <*> (parseDate >>= pure <<< date2String))
  <|>
  Simple <$> (Value <$> getPosition <*> pure PTime <*> (parseTime >>= pure <<< time2String))
  <|>
  Simple <$> (Value <$> getPosition <*> pure PString <*> token.stringLiteral)
  <|>
  Simple <$> (Value <$> getPosition <*> pure PMarkDown <*> markDownLiteral)
  <|>
  Simple <$> (Value <$> getPosition <*> pure PBool <*> boolean)
  <|>
  Simple <$> (Value <$> getPosition <*> pure PNumber <*> (token.integer >>= pure <<< show))
  <|>
  Simple <$> (Value <$> getPosition <*> pure PEmail <*> (email))
  <|>
  Simple <$> (PublicRole <$> getPosition <*> (reserved "publicrole" *> pubParser))
  <|>
  Simple <$> (PublicContext <$> getPosition <*> (reserved "publiccontext" *> pubParser))
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
  <|>
  Simple <$> (IsInState <$> (getPosition <* reserved "isInState") <*> arcIdentifier)
  <|>
  Simple <$> (RegEx <$> (getPosition <* reserved "regexp") <*> regexExpression)
  -- VARIABLE MUST BE LAST!
  <|>
  Simple <$> (Variable <$> getPosition <*> lowerCaseName)
  ) <?> "binding, binder, context, extern, this, modelname, contextType, roleTypes, specialisesRoleType, a valid variablename (lowercase only) or a number, boolean, string (between double quotes), date (between single quotes), email address or a monoid function (sum, product, minimum, maximum) or count, "

-- | Parses just the regular expression; not "matches", which is interpreted like ">>".
-- | We expect an expression like this: "..."gimyu
-- | where all flags are optional.
regexExpression :: IP RegExP
regexExpression = do
  regexString <- token.stringLiteral
  flags <- option "" regexFlags'
  result <- pure $ regex regexString (parseFlags flags)
  case result of
    Left e -> fail e
    Right r -> pure $ RegExP r

sequenceFunction :: IP FunctionName
sequenceFunction = (token.symbol "sum" *> pure AddF
  <|> token.symbol "product" *> pure MultiplyF
  <|> token.symbol "minimum" *> pure MinimumF
  <|> token.symbol "maximum" *> pure MaximumF
  <|> token.symbol "count" *> pure CountF
  <|> token.symbol "first" *> pure FirstF
  ) <?> "sum, product, minimum,\
\ maximum or count, "

-- propertyRange :: IP String
-- propertyRange = (reserved "Boolean" *> (pure "Boolean")
--   <|> reserved "Number" *> (pure "Number")
--   <|> reserved "String" *> (pure "String")
--   <|> reserved "Time" *> (pure "Time")
--   <|> reserved "Date" *> (pure "Date")
--   <|> reserved "DateTime" *> (pure "DateTime")) <?> "Boolean, Number, String or DateTime, "

propertyRange :: IP Range
propertyRange = (reserved "Boolean" *> (pure $ PBool)
  <|> reserved "Number" *> (pure $ PNumber)
  <|> reserved "String" *> (pure $ PString)
  <|> reserved "DateTime" *> (pure $ PDateTime)
  <|> reserved "Email" *> (pure $ PEmail)
  <|> reserved "File" *> (pure $ PFile)
  <|> reserved "Date" *> (pure $ PDate)
  <|> reserved "Time" *> (pure $ PTime)
  <|> reserved "Year" *> (pure $ (PDuration Year_))
  <|> reserved "Month" *> (pure $ (PDuration Month_))
  <|> reserved "Week" *> (pure $ (PDuration Week_))
  <|> reserved "Day" *> (pure $ (PDuration Day_))
  <|> reserved "Hour" *> (pure $ (PDuration Hour_))
  <|> reserved "Minute" *> (pure $ (PDuration Minute_))
  <|> reserved "Second" *> (pure $ (PDuration Second_))
  <|> reserved "MilliSecond" *> (pure $ (PDuration MilliSecond_))
  <|> reserved "MarkDown" *> (pure $ PMarkDown)
  )

-- | Parse a time. Succeeds if the Time component of parseDateTime is empty.
parseDate :: IP Date
parseDate = do
  (DateTime date time) <- parseDateTime
  if time `eq` Time (unsafePartial fromJust (toEnum 0) :: Hour) (unsafePartial fromJust $ toEnum 0) (unsafePartial fromJust $ toEnum 0) (unsafePartial fromJust $ toEnum 0)
    then pure date
    else fail "a date without a time component."

-- | Only supports the "hh:mm:ss" and "hh:mm:ss.sss" format. See https://tc39.es/ecma262/multipage/numbers-and-dates.html#sec-date-time-string-format.
parseTime :: IP Time
parseTime = between (char '\'') (char '\'' <?> "end of string. ") do
  hh <- token.integer
  _ <- token.colon
  mm <- token.integer
  _ <- token.colon
  ss <- token.integer
  _ <- token.dot
  sss <- option 0 token.integer
  case (toEnum hh), (toEnum mm), (toEnum ss), (toEnum sss) of
    Just hh', Just mm', Just ss', Just sss' -> pure $ Time hh' mm' ss' sss'
    _, _, _, _ -> fail "a time string like hh:mm:ss or hh:mm:ss.sss"

-- | Parse a date-time combination. See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse#Date_Time_String_Format for the supported string format of the date.
-- | Summary: the type must conform to YYYY-MM-DDTHH:mm:ss.sssZ, but time may be left out.
parseDateTime :: IP DateTime
parseDateTime = try do
  s <- dateTimeLiteral
  (d :: JSDate) <- pure $ unsafePerformEffect $ parse s
  case toDateTime d of
    Nothing -> fail "a date-time combination like YYYY-MM-DDTHH:mm:ss.sssZ."
    (Just (dt :: DateTime)) -> pure dt

parseJSDate :: IP JSDate
parseJSDate = try do
  s <- dateTimeLiteral
  pure $ unsafePerformEffect $ parse s

isUnaryKeyword :: String -> Boolean
isUnaryKeyword kw = isJust $ elemIndex kw ["not", "exists", "filledBy", "fills", "available", "roleinstance", "contextinstance"]

unaryStep :: IP Step
unaryStep = do
  keyword <- lookAhead reservedIdentifier <?> "not, exists, filledBy, fills or available. "
  case keyword of
    "not" -> (Unary <$> (LogicalNot <$> getPosition <*> (reserved "not" *> (defer \_ -> step))))
    "exists" -> Unary <$> (Exists <$> getPosition <*> (reserved "exists" *> (defer \_ -> step)))
    "filledBy" -> Unary <$> (FilledBy <$> getPosition <*> (reserved "filledBy" *> (defer \_ -> step)))
    "fills" -> Unary <$> (Fills <$> getPosition <*> (reserved "fills" *> (defer \_ -> step)))
    "available" -> Unary <$> (Available <$> getPosition <*> (reserved "available" *> (defer \_ -> step)))
    "contextinstance" -> Unary <$> (ContextIndividual <$> getPosition <*> (reserved "contextinstance" *> token.parens arcIdentifier) <*> (defer \_ -> step))
    "roleinstance" -> Unary <$> (RoleIndividual <$> getPosition <*> (reserved "roleinstance" *> token.parens arcIdentifier) <*> (defer \_ -> step))
    s -> fail ("Expected not, exists, filledBy, fills or available, but found: '" <> s <> "'. ")

operator :: Partial => IP Operator
operator =
  ((Filter <$> (getPosition <* reserved "with"))
  <|>
  (Sequence <$> (getPosition <* token.reservedOp ">>="))
  <|>
  (Compose <$> (getPosition <* token.reservedOp ">>"))
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
  (LogicalAnd <$> (getPosition <* reserved "and"))
  <|>
  (LogicalOr <$> (getPosition <* reserved "or"))
  <|>
  (Add <$> (getPosition <* token.reservedOp "+"))
  <|>
  (Subtract <$> (getPosition <* token.reservedOp "-"))
  <|>
  (Divide <$> (getPosition <* token.reservedOp "/"))
  <|>
  (Multiply <$> (getPosition <* token.reservedOp "*"))
  <|>
  (Union <$> (getPosition <* reserved "union"))
  <|>
  (OrElse <$> (getPosition <* reserved "orElse"))
  <|>
  (Intersection <$> (getPosition <* reserved "intersection"))
  <|>
  (BindsOp <$> (getPosition <* token.reserved "filledBy"))
  <|>
  -- NOTICE the trick here: we map "matches" to Compose, so we can use it as an infix operator while it
  -- builds on the result of the previous step.
  (Compose <$> (getPosition <* token.reserved "matches"))
  ) <?> "with, >>=, >>, ==, /=, <, <=, >, >=, and, or, +, -, /, *, union, intersection, otherwise, filledBy. "

durationOperator :: Partial => IP Operator
durationOperator =
  ((Year <$> (getPosition <* (reserved "year" <|> reserved "years")))
  <|>
  (Month <$> (getPosition <* (reserved "month" <|> reserved "months")))
  <|>
  (Week <$> (getPosition <* (reserved "week" <|> reserved "weeks")))
  <|>
  (Day <$> (getPosition <* (reserved "day" <|> reserved "days")))
  <|>
  (Hour <$> (getPosition <* (reserved "hour" <|> reserved "hours")))
  <|>
  (Minute <$> (getPosition <* (reserved "minute" <|> reserved "minutes")))
  <|>
  (Second <$> (getPosition <* (reserved "second" <|> reserved "seconds")))
  <|>
  (Millisecond <$> (getPosition <* (reserved "millisecond" <|> reserved "milliseconds"))))
  <?> "year(s), month(s), week(s), day(s), hour(s), minute(s), second(s), second(s), millisecond(s)"

operatorPrecedence :: Operator -> Int
operatorPrecedence (Year _) = 10
operatorPrecedence (Month _) = 10
operatorPrecedence (Week _) = 10
operatorPrecedence (Day _) = 10
operatorPrecedence (Hour _) = 10
operatorPrecedence (Minute _) = 10
operatorPrecedence (Second _) = 10
operatorPrecedence (Millisecond _) = 10
operatorPrecedence (Compose _) = 9

operatorPrecedence (Union _) = 8
operatorPrecedence (Intersection _) = 8
operatorPrecedence (OrElse _) = 8
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
operatorPrecedence (Matches _) = 3

operatorPrecedence (LogicalAnd _) = 2
operatorPrecedence (LogicalOr _) = 2

-- not, exists, available, filledBy, fills 1

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
    startOfSimple (RoleTypeIndividual p _) = p
    startOfSimple (ContextTypeIndividual p _) = p
    startOfSimple (Value p _ _) = p
    startOfSimple (PublicRole p _) = p
    startOfSimple (PublicContext p _) = p
    startOfSimple (Filler p _) = p
    startOfSimple (Filled p _ _) = p
    startOfSimple (Context p) = p
    startOfSimple (Extern p) = p
    startOfSimple (IndexedName p) = p
    startOfSimple (SequenceFunction p _) = p
    startOfSimple (Identity p) = p
    startOfSimple (Modelname p) = p
    startOfSimple (Variable p _) = p

    startOfSimple (TypeOfContext p) = p
    startOfSimple (RoleTypes p) = p
    startOfSimple (SpecialisesRoleType p _) = p
    startOfSimple (IsInState p _) = p
    
    startOfSimple (TypeTimeOnlyContext p _) = p
    startOfSimple (TypeTimeOnlyEnumeratedRole p _ _) = p
    startOfSimple (TypeTimeOnlyCalculatedRole p _) = p
    startOfSimple (RegEx p _) = p

    startOfUnary (LogicalNot p _) = p
    startOfUnary (Exists p _) = p
    startOfUnary (FilledBy p _) = p
    startOfUnary (Fills p _) = p
    startOfUnary (Available p _) = p
    startOfUnary (DurationOperator p _ _) = p
    startOfUnary (ContextIndividual p _ _) = p
    startOfUnary (RoleIndividual p _ _) = p


endOf :: Step -> ArcPosition
endOf stp = case stp of
  (Simple s) -> endOfSimple s
  (Binary (BinaryStep{end})) -> end
  (Unary us) -> endOfUnary us
  (PureLet (PureLetStep {end})) -> end
  (Computation (ComputationStep {end})) -> end

  where
    endOfSimple (ArcIdentifier (ArcPosition{line, column}) id) = ArcPosition{line, column: column + length id}
    endOfSimple (RoleTypeIndividual (ArcPosition{line, column}) id) = ArcPosition{line, column: column + 11 + length id}
    endOfSimple (ContextTypeIndividual (ArcPosition{line, column}) id) = ArcPosition{line, column: column + 14 + length id}
    endOfSimple (Value (ArcPosition{line, column}) _ v) = ArcPosition({line, column: column + length v + 1})
    endOfSimple (PublicRole (ArcPosition{line, column}) url) =  ArcPosition({line, column: column + length url + 1})
    endOfSimple (PublicContext (ArcPosition{line, column}) url) =  ArcPosition({line, column: column + length url + 1})
    endOfSimple (Filler (ArcPosition{line, column}) _) = ArcPosition{line, column: column + 7}
    endOfSimple (Filled (ArcPosition{line, column}) _ _) = ArcPosition{line, column: column + 6}
    endOfSimple (Context (ArcPosition{line, column})) = ArcPosition{line, column: column + 7}
    endOfSimple (Extern (ArcPosition{line, column})) = ArcPosition{line, column: column + 6}
    endOfSimple (IndexedName (ArcPosition{line, column})) = ArcPosition{line, column: column + 11}
    endOfSimple (SequenceFunction (ArcPosition{line, column}) fname) = ArcPosition{line, column: column + length (show fname)}
    endOfSimple (Identity (ArcPosition{line, column})) = ArcPosition{line, column: column + 4}
    endOfSimple (Modelname (ArcPosition{line, column})) = ArcPosition{line, column: column + 9}
    endOfSimple (Variable (ArcPosition{line, column}) v) = ArcPosition{line, column: column + length v}

    endOfSimple (TypeOfContext (ArcPosition{line, column})) = ArcPosition{line, column: column + 11}
    endOfSimple (RoleTypes (ArcPosition{line, column})) = ArcPosition{line, column: column + 9}
    endOfSimple (SpecialisesRoleType (ArcPosition{line, column}) ident) = ArcPosition{line, column: column + 19 + length ident}
    endOfSimple (IsInState (ArcPosition{line, column}) ident) = ArcPosition{line, column: column + 9 + length ident}
    
    endOfSimple (TypeTimeOnlyContext p _) = p
    endOfSimple (TypeTimeOnlyEnumeratedRole p _ _) = p
    endOfSimple (TypeTimeOnlyCalculatedRole p _) = p
    endOfSimple (RegEx (ArcPosition{line, column}) (RegExP r)) = ArcPosition{line, column: column + length (show r) + 1}

    -- Note that this assumes a single whitespace between 'not' and the step.
    endOfUnary (LogicalNot (ArcPosition{line, column}) step') = ArcPosition{line: line_(endOf step'), column: col_(endOf step') + 4}
    endOfUnary (Exists (ArcPosition{line, column}) step') = endOf step'
    endOfUnary (FilledBy (ArcPosition{line, column}) step') = endOf step'
    endOfUnary (Fills (ArcPosition{line, column}) step') = endOf step'
    endOfUnary (Available (ArcPosition{line, column}) step') = endOf step'
    endOfUnary (DurationOperator _ _ step') = endOf step'
    endOfUnary (ContextIndividual _ _ step') = endOf step'
    endOfUnary (RoleIndividual _ _ step') = endOf step'

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
        chars <- between (char '\'') (char '\'' <?> "end of string. ") (many dateChar)
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
computationStep = do
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
  computedType <- reserved "returns" *> ((OtherType <$> arcIdentifier) <|> (ComputedRange <$> propertyRange))
  end <- getPosition
  pure $ Computation $ ComputationStep {functionName, arguments: (fromFoldable arguments), computedType, start, end}

-- | Anything between '<' and '<'
markDownLiteral :: IP String
markDownLiteral = (go <?> "MarkDown") <* token.whiteSpace
  where
    go :: IP String
    go = do
        (ArcPosition {column}) <- getPosition
        chars <- between (char '<') (char '>' <?> "end of MarkDown (>). ") (many markDownChar)
        pure $ fixIndentation column $ trim $ SCU.fromCharArray chars

    markDownChar :: IP Char
    markDownChar = satisfy \c -> c /= '>'

    -- Replace each occurrence of a newline character followed by n spaces by
    -- a newline character followed by (n-startColumn) spaces. 
    -- This allows for the formatting of markdown at any position, where the starting '<' character should be to the left of all markdown lines.
    fixIndentation :: Int -> String -> String
    fixIndentation startColumn s = replaceAll (Pattern $ fold $ replicate startColumn " ") (Replacement "") s
