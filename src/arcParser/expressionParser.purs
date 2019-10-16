module Perspectives.Parsing.Arc.Expression where

import Control.Alt (map, void, (<|>))
import Control.Lazy (defer)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), Step(..), UnaryStep(..), SimpleStep(..), Operator(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier, stringUntilNewline, reserved, colon, lowerCaseName)
import Perspectives.Parsing.Arc.IndentParser (ArcPosition, IP, getPosition, withEntireBlock, nextLine)
import Perspectives.Parsing.Arc.Token (token)
import Text.Parsing.Parser (fail, failWithPosition)
import Text.Parsing.Parser.Combinators (lookAhead, option, optionMaybe, try, (<?>))
import Prelude ((<$>), (<*>), ($), pure, (<<<), (*>), bind, discard)

step :: IP Step
step = simpleStep <|> defer \_ -> unaryStep <|> compoundStep

simpleStep :: IP Step
simpleStep =
  Simple <$> (ArcIdentifier <$> getPosition <*> arcIdentifier)
  <|>
  Simple <$> (Binding <$> (reserved "binding" *> getPosition))
  <|>
  Simple <$> (Binder <$> (reserved "binder" *> getPosition))
  <|>
  Simple <$> (Context <$> (reserved "context" *> getPosition))
  <|>
  Simple <$> (Extern <$> (reserved "extern" *> getPosition))

unaryStep :: IP Step
unaryStep =
  Unary <$> (LogicalNot <$> getPosition <*> (reserved "not" *> (defer \_ -> step)))
  <|>
  Unary <$> (Create <$> getPosition <*> (reserved "create" *> (defer \_ -> arcIdentifier)))
  <|>
  Unary <$> (Exists <$> getPosition <*> (reserved "exists" *> (defer \_ -> arcIdentifier)))

compoundStep :: IP Step
compoundStep = defer \_->filterStep <|> binaryStep <|> token.parens filterStep <|> token.parens binaryStep

filterStep :: IP Step
filterStep = do
  start <- getPosition
  reserved "filter"
  source <- step
  reserved "with"
  criterium <- step
  end <- getPosition
  pure $ Binary $ BinaryStep {start, end, operator: Filter start, left: source, right: criterium}

binaryStep :: IP Step
binaryStep = do
  start <- getPosition
  left <- step
  op <- operator
  right <- step
  end <- getPosition
  pure $ Binary $ BinaryStep {start, end, left, operator: op, right}

operator :: IP Operator
operator = (Compose <$> (reserved ">>" *> getPosition))
  <|>
  (Equals <$> (reserved "==" *> getPosition))
  <|>
  (NotEquals <$> (reserved "/=" *> getPosition))
  <|>
  (LessThen <$> (reserved "<" *> getPosition))
  <|>
  (LessThenEqual <$> (reserved "<=" *> getPosition))
  <|>
  (GreaterThen <$> (reserved ">" *> getPosition))
  <|>
  (GreaterThenEqual <$> (reserved ">=" *> getPosition))
  <|>
  (LogicalAnd <$> (reserved "and" *> getPosition))
  <|>
  (LogicalOr <$> (reserved "or" *> getPosition))
  <|>
  (Add <$> (reserved "+" *> getPosition))
  <|>
  (Subtract <$> (reserved "-" *> getPosition))
  <|>
  (Divide <$> (reserved "/" *> getPosition))
  <|>
  (Multiply <$> (reserved "*" *> getPosition))
