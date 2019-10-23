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

module Perspectives.Token where

import Control.Alt ((<|>))
import Control.Monad (class Monad)
import Perspectives.IndentParser (ContextRoleParserMonad)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, letter, makeTokenParser)

-- type IndentTokenParser e = GenTokenParser String (StateT Position (Aff e))
type IndentTokenParser = GenTokenParser String ContextRoleParserMonad

token :: IndentTokenParser
token = makeTokenParser perspectDef

-- type LanguageDef = GenLanguageDef String Identity

-- type IndentLanguageDef e = GenLanguageDef String (StateT Position (Aff e))
type IndentLanguageDef = GenLanguageDef String ContextRoleParserMonad

perspectDef :: IndentLanguageDef
-- perspectDef = LanguageDef (unGenLanguageDef haskellStyle)
--                 { reservedOpNames = ["=", "=>"]
--                 , reservedNames   = [ "intern","extern"]
--                 }
-- | Even though we have comments, we make the Tokenizer none the wizer. This way it won't skip comments.
-- | We detect comments ourselves and collect them!
perspectDef = LanguageDef
                { commentStart    : ""
                , commentEnd      : ""
                , commentLine     : ""
                , nestedComments:  false
                , identStart      : letter <|> char ':'
                , identLetter:     alphaNum <|> oneOf ['_', '\'']
                , opStart:         op'
                , opLetter:        op'
                , reservedOpNames : ["=", "=>", "->", "_"]
                , reservedNames   : [ "intern","extern", "property", "Section", "Context", "als", "import", "GebruikerGegevens"]
                , caseSensitive:   true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf ['!', '$', '%', '&', '*', '+', '.', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
