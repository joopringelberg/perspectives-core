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

module Perspectives.Parsing.Arc.Identifiers where

import Control.Alt (void, (<|>))
import Data.Array (cons, elemIndex, intercalate, many)
import Data.Char.Unicode (isLower)
import Data.Maybe (isJust)
import Data.String.CodeUnits (fromCharArray)
import Perspectives.Parsing.Arc.IndentParser (IP)
import Perspectives.Parsing.Arc.Token (token)
import Prelude (Unit, bind, discard, pure, ($), (/=), (<>), (*>))
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.String (string, satisfy, whiteSpace)

reserved :: String -> IP Unit
reserved = token.reserved

colon :: IP String
colon = token.colon

arcIdentifier :: IP String
arcIdentifier = (qualifiedName <|> prefixedName <|> segmentedName) <?> "a capitalized name, a prefixed name, or a fully qualified name"
  where
    qualifiedName :: IP String
    qualifiedName = try do
      m <- (string "model:")
      i <- segmentedName
      pure $ m <> i

    prefixedName :: IP String
    prefixedName = try do
      pre <- lowerCaseName
      void token.colon
      i <- segmentedName
      pure (pre <> ":" <> i)

    segmentedName :: IP String
    segmentedName = try do
      first <- token.identifier
      rest <- many (string "$" *> token.identifier)
      pure (intercalate "$" (cons first rest))

-- | Parses /.../
regexExpression' :: IP String
regexExpression' = do
  void $ string "/"
  s <- stringUntilForwardSlash
  void $ string "/"
  pure s
  where
    stringUntilForwardSlash :: IP String
    stringUntilForwardSlash = do
      (chars :: Array Char) <- many (satisfy (_ /= '/'))
      _ <- whiteSpace
      pure $ fromCharArray chars

-- | Parses gimyu.
regexFlags' :: IP String
regexFlags' = do
  (chars :: Array Char) <- many (satisfy (\c -> isJust $ elemIndex c ['g', 'i', 'm', 'y', 'u']))
  _ <- whiteSpace
  pure $ fromCharArray chars


stringUntilNewline :: IP String
stringUntilNewline = do
  (chars :: Array Char) <- many (satisfy (_ /= '\n'))
  _ <- whiteSpace
  pure $ fromCharArray chars

lowerCaseName :: IP String
lowerCaseName = try do
  f <- lower
  r <- many lower
  void token.whiteSpace
  pure $ fromCharArray (cons f r)

lower ::  IP Char
lower = satisfy isLower <?> "lowercase letter"

boolean :: IP String
boolean = token.symbol "true" <|> token.symbol "false"
