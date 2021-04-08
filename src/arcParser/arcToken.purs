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

module Perspectives.Parsing.Arc.Token where

import Control.Alt ((<|>))
import Control.Monad (class Monad)
import Control.Monad.State (StateT)
import Perspectives.Parsing.Arc.IndentParser (ArcParser)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, makeTokenParser, upper)

type IndentTokenParser = GenTokenParser String (StateT Position ArcParser)

token :: IndentTokenParser
token = makeTokenParser perspectDef

type IndentLanguageDef = GenLanguageDef String (StateT Position ArcParser)

perspectDef :: IndentLanguageDef
-- perspectDef = LanguageDef (unGenLanguageDef haskellStyle)
--                 { reservedOpNames = ["=", "=>"]
--                 , reservedNames   = [ "intern","extern"]
--                 }
perspectDef = LanguageDef
                { commentStart    : "{-"
                , commentEnd      : "-}"
                , commentLine     : "--"
                , nestedComments:  false
                , identStart      : upper
                , identLetter:     alphaNum <|> oneOf ['_']
                , opStart:         op'
                , opLetter:        op'
                , reservedOpNames : ["="]
                , reservedNames   :
                  -- Contexts
                  [ "domain"
                  , "case"
                  , "party"
                  , "state"
                  , "activity"
                  , "use"

                  -- Roles
                  , "thing"
                  , "user"
                  , "bot"
                  , "context"
                  , "properties"
                  , "filledBy"
                  , "for"
                  , "callExternal"
                  , "callEffect"

                  -- Properties
                  , "property"
                  , "mandatory"
                  , "functional"
                  , "not"
                  , "number"
                  , "string"
                  , "date"
                  , "boolean"

                  -- Perspectives
                  , "perspective"
                  , "on"
                  , "consults"
                  , "changes"
                  , "deletes"
                  , "creates"
                  , "with"
                  , "indirectObject"
                  , "objectView"
                  , "indirectObjectView"
                  , "subjectView"

                  -- Queries
                  , "Filter"
                  ]
                , caseSensitive:   true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '%', '&', '*', '+', '.', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
