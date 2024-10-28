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

module Perspectives.Parsing.Arc.Identifiers
  where

import Control.Alt (void, (<|>))
import Data.Array (cons, elemIndex, intercalate, many)
import Data.CodePoint.Unicode (isLower, isSpace)
import Data.Maybe (isJust)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Perspectives.Parsing.Arc.IndentParser (IP)
import Perspectives.Parsing.Arc.Token (token)
import Perspectives.ResourceIdentifiers (hasPublicResourceShape)
import Prelude (Unit, bind, discard, not, pure, ($), (*>), (/=), (<<<), (<>))
import Parsing (fail)
import Parsing.Combinators (try, (<?>))
import Parsing.String (string, satisfy)
import Parsing.String.Basic (whiteSpace)

reserved :: String -> IP Unit
reserved = token.reserved

colon :: IP String
colon = token.colon

arcIdentifier :: IP String
arcIdentifier = (qualifiedName <|> prefixedName <|> segmentedName) <?> "a capitalized name, a prefixed name, or a fully qualified name, "

-- Parses model://perspectives.domains#System
qualifiedName :: IP String
qualifiedName = try do
  m <- (string "model://")
  authority <- dottedName
  hash <- string "#"
  segments <- segmentedName
  pure $ m <> authority <> "#" <> segments

dottedName :: IP String
dottedName = try do
  first <- lowerCaseName
  rest <- many (string "." *> lowerCaseName)
  pure (intercalate "." (cons first rest))

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
lower = satisfy (isLower <<< codePointFromChar) <?> "lowercase letter, "

boolean :: IP String
boolean = token.symbol "true" <|> token.symbol "false"

email :: IP String
email = try do
  chars <- many (satisfy (not <<< isSpace <<< codePointFromChar))
  if (test emailRegExp (fromCharArray chars))
    then whiteSpace *> pure (fromCharArray chars)
    else fail "Not a valid email addres. "
  where
    -- See: https://regexlib.com/REDetails.aspx?regexp_id=26.
    emailRegExp :: Regex
    emailRegExp = unsafeRegex "^([a-zA-Z0-9_\\-\\.]+)@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.)|(([a-zA-Z0-9\\-]+\\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\\]?)$" noFlags

-- | Produces a resource identifier in the pub: scheme:
-- | pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains
-- | Only passes strings of this form: https://{authority}/cw_{databasename}/{SegmentedIdentifier}.
pubParser :: IP String
pubParser = try do
  chars <- many (satisfy (not <<< isSpace <<< codePointFromChar))
  void whiteSpace
  s <- pure (fromCharArray chars)
  if hasPublicResourceShape s
    then pure s
    else fail "Not a valid public resource url. " 
