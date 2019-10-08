module Perspectives.Parsing.Arc.Identifiers where

import Control.Alt (void, (<|>))
import Data.Array (many)
import Data.Char.Unicode (isLower)
import Data.String.CodeUnits (fromCharArray)
import Perspectives.Parsing.Arc.IndentParser (IP)
import Perspectives.Parsing.Arc.Token (token)
import Prelude (Unit, bind, discard, pure, ($), (/=), (<>), (>>=), (<<<), (<*))
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.String (string, satisfy, whiteSpace)

reserved :: String -> IP Unit
reserved = token.reserved

colon :: IP String
colon = token.colon

arcIdentifier :: IP String
arcIdentifier = (qualifiedName <|> prefixedName <|> token.identifier) <?> "a capitalized name, a prefixed name, or a fully qualified name"
  where
    qualifiedName :: IP String
    qualifiedName = try do
      m <- (string "model:")
      i <- token.identifier
      pure $ m <> i

    prefixedName :: IP String
    prefixedName = try do
      pre <- prefix
      void token.colon
      i <- token.identifier
      pure (pre <> ":" <> i)

stringUntilNewline :: IP String
stringUntilNewline = do
  (chars :: Array Char) <- many (satisfy (_ /= '\n'))
  _ <- whiteSpace
  pure $ fromCharArray chars

prefix :: IP String
prefix = many lower <* token.whiteSpace >>= pure <<< fromCharArray

lower ::  IP Char
lower = satisfy isLower <?> "lowercase letter"
