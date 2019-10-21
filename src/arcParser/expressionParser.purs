module Perspectives.Parsing.Arc.Expression where

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Perspectives.Parsing.Arc.Expression.AST (Assignment(..), AssignmentOperator(..), BinaryStep(..), Operator(..), SimpleStep(..), Step(..), UnaryStep(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, boolean, reserved)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition(..), IP, getPosition)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.Representation.EnumeratedProperty (Range(..))
import Prelude ((<$>), (<*>), ($), pure, (*>), bind, discard, (<*), (>), (+), (>>=), (<<<), show)
import Text.Parsing.Parser.Combinators (try, (<?>))

step :: IP Step
step = step_ <|> token.parens step_
  where
    step_ :: IP Step
    step_ = try (defer \_-> compoundStep <|> defer \_ -> unaryStep <|> simpleStep)

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
  ) <?> "binding, binder, context, extern or a valid identifier"

unaryStep :: IP Step
unaryStep = try
  (Unary <$> (LogicalNot <$> getPosition <*> (reserved "not" *> (defer \_ -> step)))
  <|>
  Unary <$> (Exists <$> getPosition <*> (reserved "exists" *> (defer \_ -> step)))) <?> "not <expr>, createContext  <identifier>, createRole <identifier>, exists <step>"

compoundStep :: IP Step
compoundStep = try (defer \_->binaryStep) <|> (defer \_->filterStep)

filterStep :: IP Step
filterStep = try do
  start <- getPosition
  reserved "filter"
  source <- step
  reserved "with"
  criterium <- step
  end <- getPosition
  pure $ Binary $ BinaryStep {start, end, operator: Filter start, left: source, right: criterium}

binaryStep :: IP Step
binaryStep = try do
  start <- getPosition
  left <- simpleStep <|> token.parens step
  op <- operator
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

operator :: IP Operator
operator = ((Compose <$> (getPosition <* reserved ">>"))
  <|>
  (Equals <$> (getPosition <* reserved "=="))
  <|>
  (NotEquals <$> (getPosition <* reserved "/="))
  <|>
  (LessThan <$> (getPosition <* reserved "<"))
  <|>
  (LessThanEqual <$> (getPosition <* reserved "<="))
  <|>
  (GreaterThan <$> (getPosition <* reserved ">"))
  <|>
  (GreaterThanEqual <$> (getPosition <* reserved ">="))
  <|>
  (LogicalAnd <$> (getPosition <* reserved "and"))
  <|>
  (LogicalOr <$> (getPosition <* reserved "or"))
  <|>
  (Add <$> (getPosition <* reserved "+"))
  <|>
  (Subtract <$> (getPosition <* reserved "-"))
  <|>
  (Divide <$> (getPosition <* reserved "/"))
  <|>
  (Multiply <$> (getPosition <* reserved "*"))) <?> ">>, ==, /=, <, <=, >, >=, and, or, +, -, /, *"

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
    startOfSimple (NoOp p) = p

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
    endOfSimple (NoOp pos) = pos

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
  (DeleteFrom <$> (getPosition <* reserved "=-"))
  <|>
  (AddTo <$> (getPosition <* reserved "=+"))
  <|>
  ((Set <$> (getPosition <* reserved "="))
  ) <?> "=, =+, =-"

deletion :: IP Assignment
deletion = try do
  start <- getPosition
  reserved "delete"
  lhs <- arcIdentifier
  end <- getPosition
  pure $ Assignment {start, end, lhs, operator: Delete start, value: Nothing }
