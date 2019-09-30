module Perspectives.Parsing.Arc.Identifiers where

import Control.Alt ((<|>))
import Data.Array (foldl, intercalate)
import Data.Array (many, cons) as AR
import Data.Char.Unicode (isLower)
import Data.String.CodeUnits (fromCharArray)
import Perspectives.Parsing.Arc.IndentParser (IP)
import Perspectives.Parsing.Arc.Token (token)
import Prelude (Unit, bind, pure, show, ($), ($>), (/=), (<$>), (<*>), (<>), (*>))
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.String (char, satisfy, whiteSpace)
import Text.Parsing.Parser.String (oneOf, string) as STRING
import Text.Parsing.Parser.Token (alphaNum, upper)

reservedOp ::  String -> IP Unit
reservedOp = token.reservedOp

reserved ::  String -> IP Unit
reserved = token.reserved

identifier ::  IP String
identifier = token.identifier

lexeme :: forall a. IP a -> IP a
lexeme = token.lexeme

int ::  IP String
int = show <$> token.integer

bool ::  IP String
bool = reserved "true" $> "true" <|> reserved "false" $> "false"

string ::  IP String
string = token.stringLiteral

simpleValue ::  IP String
simpleValue = string <|> int <|> bool

identLetter ::  IP Char
identLetter = alphaNum <|> STRING.oneOf ['_', '\'']

identLetterString ::  IP String
identLetterString = f <$> identLetter <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

-- /([A-Z]\w*\b)/
-- /(\p{Uppercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+)/gu
capitalizedString ::  IP String
capitalizedString = f <$> upper <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

lower ::  IP Char
lower = satisfy isLower <?> "uppercase letter"

-- /([a-z]\w*\b)/
-- /(\b\p{Lowercase}[\p{Alphabetic}\p{Mark}\p{Decimal_Number}\p{Connector_Punctuation}\p{Join_Control}]+\b)/gu
uncapitalizedString ::  IP String
uncapitalizedString = f <$> lower <*> AR.many identLetter where
  f c ca = fromCharArray $ AR.cons c ca

colon :: IP Unit
colon = reservedOp ":"

qualifiedName ::  IP String
qualifiedName = do
  _ <- STRING.string "model:"
  domein <- AR.many (dollar <|> capitalizedString)
  pure $ "model:" <> foldl (<>) "" domein

dollar :: IP String
dollar = reservedOp "$" *> pure "$"

arcIdentifier :: IP String
arcIdentifier = lexeme (try capitalizedString <|> try qualifiedName)

stringUntilNewline :: IP String
stringUntilNewline = do
  (chars :: Array Char) <- AR.many (satisfy (_ /= '\n'))
  _ <- whiteSpace
  pure $ fromCharArray chars
