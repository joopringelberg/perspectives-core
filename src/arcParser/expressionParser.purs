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
import Data.Array (fromFoldable, many)
import Data.DateTime (DateTime)
import Data.JSDate (JSDate, parse, toDateTime)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits as SCU
import Effect.Unsafe (unsafePerformEffect)
import Perspectives.Parsing.Arc.Expression.AST (Assignment(..), AssignmentOperator(..), BinaryStep(..), ComputationStep(..), LetStep(..), Operator(..), PureLetStep(..), SimpleStep(..), Step(..), UnaryStep(..), VarBinding(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, boolean, colon, lowerCaseName, reserved)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..), IP, getPosition, withEntireBlock)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.Representation.QueryFunction (FunctionName(..))
import Perspectives.Representation.Range (Range(..))
import Prelude (bind, discard, not, pure, show, ($), (&&), (*>), (+), (<$>), (<*), (<*>), (<<<), (>), (>>=))
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (between, lookAhead, optionMaybe, try, (<?>))
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
        (Binary (BinaryStep {left: leftOfRight, operator:opOfRight, right: rightOfRight, end: endOfRight, parenthesised: protected})) -> if not protected && ((operatorPrecedence op) > (operatorPrecedence opOfRight))
          then pure $ Binary $ BinaryStep
            { start
            , end -- equals endOfRight.
            , left: Binary (BinaryStep {start, end: endOf(leftOfRight), operator: op, left: left, right: leftOfRight, parenthesised: false})
            , operator: opOfRight
            , right: rightOfRight
            , parenthesised: false
          }
          else pure $ Binary $ BinaryStep {start, end, left, operator: op, right, parenthesised: parenthesised}
        otherwise -> pure $ Binary $ BinaryStep {start, end, left, operator: op, right, parenthesised}
  where
    leftSide :: IP Step
    leftSide = defer \_ -> reserved "filter" *> step <|> letStep <|> computationStep <|> unaryStep <|> simpleStep

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
  Unary <$> (Exists <$> getPosition <*> (reserved "exists" *> (defer \_ -> step)))
  <|>
  Unary <$> (Binds <$> getPosition <*> (reserved "binds" *> (defer \_ -> step)))
  <|>
  Unary <$> (BoundBy <$> getPosition <*> (reserved "boundBy" *> (defer \_ -> step)))
  <|>
  Unary <$> (Available <$> getPosition <*> (reserved "available" *> (defer \_ -> step))))
  <?> "not <expr>, exists <step>, binds <step> or available <step>."

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
  (Join <$> (getPosition <* token.reservedOp "union"))
  ) <?> ">>, ==, /=, <, <=, >, >=, and, or, +, -, /, *, >>=, union"

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
operatorPrecedence (Join _) = 8

startOf :: Step -> ArcPosition
startOf stp = case stp of
  (Simple s) -> startOfSimple s
  (Binary (BinaryStep{start})) -> start
  (Unary us) -> startOfUnary us
  (Let (LetStep {start})) -> start
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
    startOfSimple (Variable p _) = p

    startOfSimple (TypeOfContext p) = p
    startOfSimple (RoleTypes p) = p
    startOfSimple (SpecialisesRoleType p _) = p

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
  (Let (LetStep {end})) -> end
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
    endOfSimple (Variable (ArcPosition{line, column}) v) = ArcPosition{line, column: column + length v}

    endOfSimple (TypeOfContext (ArcPosition{line, column})) = ArcPosition{line, column: column + 11}
    endOfSimple (RoleTypes (ArcPosition{line, column})) = ArcPosition{line, column: column + 9}
    endOfSimple (SpecialisesRoleType (ArcPosition{line, column}) ident) = ArcPosition{line, column: column + 19 + length ident}

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

assignment :: IP Assignment
assignment = defer \_-> propertyAssignment <|> roleAssignment

roleAssignment :: IP Assignment
roleAssignment = defer \_-> removal
  <|> roleCreation
  <|> move
  <|> bind'
  <|> bind_
  <|> unbind
  <|> unbind_
  <|> roleDeletion
  <|> callEffect

removal :: IP Assignment
removal = do
  start <- getPosition
  roleExpression <- (reserved "remove" *> step)
  end <- getPosition
  pure $ Remove {start, end, roleExpression}

roleCreation :: IP Assignment
roleCreation = do
  start <- getPosition
  roleIdentifier <- reserved "createRole" *> arcIdentifier
  contextExpression <- optionMaybe (reserved "in" *> step)
  end <- getPosition
  pure $ CreateRole {start, end, roleIdentifier, contextExpression}

-- createContext ContextType bound to RoleType [ in <contextExpression>]
createContext :: IP Assignment
createContext = do
  start <- getPosition
  contextTypeIdentifier <- reserved "createContext" *> arcIdentifier
  roleTypeIdentifier <- reserved "bound" *> reserved "to" *> arcIdentifier
  contextExpression <- optionMaybe (reserved "in" *> step)
  end <- getPosition
  pure $ CreateContext {start, end, contextTypeIdentifier, roleTypeIdentifier, contextExpression}

-- createContext_ ContextType bound to <roleExpression>
createContext_ :: IP Assignment
createContext_ = do
  start <- getPosition
  contextTypeIdentifier <- reserved "createContext" *> arcIdentifier
  roleExpression <- reserved "bound" *> reserved "to" *> step
  end <- getPosition
  pure $ CreateContext_ {start, end, contextTypeIdentifier, roleExpression}

move :: IP Assignment
move = do
  start <- getPosition
  roleExpression <- (reserved "move" *> step)
  contextExpression <- optionMaybe (reserved "to" *> step)
  end <- getPosition
  pure $ Move {start, end, roleExpression, contextExpression}

bind' :: IP Assignment
bind' = do
  start <- getPosition
  bindingExpression <- (reserved "bind" *> step)
  roleIdentifier <- reserved "to" *> arcIdentifier
  contextExpression <- optionMaybe (reserved "in" *> step)
  end <- getPosition
  pure $ Bind {start, end, bindingExpression, roleIdentifier, contextExpression}

bind_ :: IP Assignment
bind_ = do
  start <- getPosition
  bindingExpression <- (reserved "bind_" *> step)
  binderExpression <- reserved "to" *> step
  end <- getPosition
  pure $ Bind_ {start, end, bindingExpression, binderExpression}

unbind :: IP Assignment
unbind = do
  start <- getPosition
  bindingExpression <- (reserved "unbind" *> step)
  roleIdentifier <- optionMaybe (reserved "from" *> arcIdentifier)
  end <- getPosition
  pure $ Unbind {start, end, bindingExpression, roleIdentifier}

unbind_ :: IP Assignment
unbind_ = do
  start <- getPosition
  bindingExpression <- (reserved "unbind_" *> step)
  binderExpression <- reserved "from" *> step
  end <- getPosition
  pure $ Unbind_ {start, end, bindingExpression, binderExpression}

propertyAssignment :: IP Assignment
propertyAssignment = (propertyDeletion <|> propertyAssignment')
  where
    propertyAssignment' :: IP Assignment
    propertyAssignment' = try do
      start <- getPosition
      propertyIdentifier <- arcIdentifier
      op <- assignmentOperator
      val <- step
      end <- getPosition
      roleExpression <- optionMaybe (reserved "for" *> step)
      pure $ PropertyAssignment {start, end, propertyIdentifier, operator: op, valueExpression: val, roleExpression}

    propertyDeletion :: IP Assignment
    propertyDeletion = try do
      start <- getPosition
      reserved "delete"
      reserved "property"
      propertyIdentifier <- arcIdentifier
      roleExpression <- optionMaybe (reserved "from" *> step)
      end <- getPosition
      pure $ DeleteProperty {start, end, propertyIdentifier, roleExpression}


assignmentOperator :: IP AssignmentOperator
assignmentOperator = try
  (DeleteFrom <$> (getPosition <* token.reservedOp "=-"))
  <|>
  (AddTo <$> (getPosition <* token.reservedOp "=+"))
  <|>
  ((Set <$> (getPosition <* token.reservedOp "="))
  ) <?> "=, =+, =-"

roleDeletion :: IP Assignment
roleDeletion = try do
  start <- getPosition
  reserved "delete"
  roleIdentifier <- arcIdentifier
  contextExpression <- optionMaybe (reserved "from" *> step)
  end <- getPosition
  pure $ DeleteRole {start, end, roleIdentifier, contextExpression}

callEffect :: IP Assignment
callEffect = try do
  start <- getPosition
  effectName <- reserved "callEffect" *> arcIdentifier
  arguments <- token.parens (token.commaSep step)
  end <- getPosition
  pure $ ExternalEffect {start, end, effectName, arguments: (fromFoldable arguments)}

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

letStep :: IP Step
letStep = do
  lookAhead (reserved "let*")
  start <- getPosition
  bindings <- withEntireBlock (\_ bs -> bs) (reserved "let*") binding
  massignments <- optionMaybe $ try $ withEntireBlock (\_ bs -> bs) (reserved "in") assignment
  case massignments of
    (Just assignments) -> do
      end <- getPosition
      pure $ Let $ LetStep {start, end, bindings: fromFoldable bindings, assignments: fromFoldable assignments}
    Nothing -> do
      body <- reserved "in" *> step
      end <- getPosition
      pure $ PureLet $ PureLetStep {start, end, bindings: fromFoldable bindings, body}

  where

    binding :: IP VarBinding
    binding = VarBinding <$> (lowerCaseName <* token.reservedOp "<-") <*> defer \_ -> step

computationStep :: IP Step
computationStep = do
  start <- getPosition
  functionName <- reserved "callExternal" *> arcIdentifier
  arguments <- token.parens (token.commaSep step)
  computedType <- reserved "returns" *> colon *> arcIdentifier
  end <- getPosition
  pure $ Computation $ ComputationStep {functionName, arguments: (fromFoldable arguments), computedType, start, end}
