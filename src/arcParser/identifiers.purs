module Perspectives.Parsing.Arc.Identifiers where

import Control.Alt (void, (<|>))
import Data.Array (many)
import Data.Char.Unicode (isLower)
import Data.String.CodeUnits (fromCharArray)
import Perspectives.Parsing.Arc.IndentParser (IP)
import Perspectives.Parsing.Arc.Token (token)
import Prelude (Unit, bind, discard, pure, ($), (/=), (<>))
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.String (string, satisfy, whiteSpace)

reserved :: String -> IP Unit
reserved = token.reserved

colon :: IP String
colon = token.colon

arcIdentifier :: IP String
arcIdentifier = qualifiedName <|> prefixedName <|> token.identifier
  where
    qualifiedName :: IP String
    qualifiedName = try do
      m <- (string "model:")
      i <- token.identifier
      pure $ m <> i

    prefixedName :: IP String
    prefixedName = try do
      pre <- many lower
      void token.colon
      i <- token.identifier
      pure (fromCharArray pre <> ":" <> i)

    lower ::  IP Char
    lower = satisfy isLower <?> "lowercase letter"

stringUntilNewline :: IP String
stringUntilNewline = do
  (chars :: Array Char) <- many (satisfy (_ /= '\n'))
  _ <- whiteSpace
  pure $ fromCharArray chars
