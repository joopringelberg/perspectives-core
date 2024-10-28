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

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (StateT)
import Data.Array (uncons, sort, many) as Array
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.String.CodeUnits as SCU
import Perspectives.Parsing.Arc.IndentParser (ArcParser)
import Parsing (ParserT, fail, Position)
import Parsing.Combinators (try, (<?>))
import Parsing.String.Basic (oneOf)
import Parsing.Token (GenLanguageDef(..), GenTokenParser, alphaNum, makeTokenParser, upper)

type IndentTokenParser = GenTokenParser String (StateT Position ArcParser)

type StringPositionParser = ParserT String (StateT Position ArcParser) String

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
                  , "indexed"
                  , "aspect"
                  , "public"
                  , "private"

                  -- Roles
                  , "thing"
                  , "user"
                  , "context"
                  , "external"
                  , "properties"
                  , "for"
                  , "at"

                  -- Properties
                  , "property"
                  , "mandatory"
                  , "relational"
                  , "functional"
                  , "unlinked"
                  , "not"
                  , "Number"
                  , "String"
                  , "DateTime"
                  , "Date"
                  , "Time"
                  , "Boolean"
                  , "verbs"
                  , "File"
                  , "file"
                  , "Email"
                  , "Year"
                  , "Month"
                  , "Week"
                  , "Day"
                  , "Hour"
                  , "Minute"
                  , "Second"
                  , "Millisecond"
                  , "year"
                  , "month"
                  , "week"
                  , "day"
                  , "hour"
                  , "minute"
                  , "second"
                  , "millisecond"
                  , "years"
                  , "months"
                  , "weeks"
                  , "days"
                  , "hours"
                  , "minutes"
                  , "seconds"
                  , "milliseconds"

                  -- PropertyFacets
                  , "minLength"
                  , "maxLength"
                  , "enumeration"
                  , "pattern"
                  , "whiteSpace"
                  , "maxInclusive"
                  , "maxExclusive"
                  , "minInclusive"
                  , "minExclusive"
                  , "totalDigits"
                  , "fractionDigits"
                  , "regexp"

                  -- Perspectives
                  , "perspective"
                  , "on"
                  , "of"
                  , "view"
                  , "props"
                  , "roleverbs"
                  , "all"
                  , "only"
                  , "except"
                  , "defaults"
                  , "default"
                  , "selfonly"
                  , "authoronly"

                  -- States
                  , "in"
                  , "entry"
                  , "exit"
                  , "notify"
                  , "do"
                  , "action"
                  , "object"
                  , "subject"

                  -- Repeating
                  , "every"
                  , "maximally"
                  , "times"
                  , "Milliseconds"
                  , "Seconds"
                  , "Minutes"
                  , "Hours"
                  , "Days"


                  -- Queries
                  , "filter"

                  -- Expressions
                  , "remove"
                  , "create"
                  , "create_"
                  , "bound"
                  , "named"
                  , "move"
                  , "bind"
                  , "bind_"
                  , "unbind"
                  , "unbind_"
                  , "delete"
                  , "callEffect"
                  , "callDestructiveEffect"
                  , "letE"
                  , "letA"
                  , "callExternal"
                  , "exists"
                  , "filledBy"
                  , "fills"
                  , "available"
                  , "with"
                  , "from"
                  , "role"
                  , "matches"
                  , "publicrole"
                  , "publiccontext"
                  , "isInState"
                  , "contextinstance"
                  , "roleinstance"

                  -- Functions in expressions
                  , "specialisesRoleType"
                  , "roleTypes"
                  , "contextType"
                  , "modelname"
                  , "returns"

                  -- Operators in expressions
                  , "union"
                  , "intersection"
                  , "orElse"
                  , "and"
                  , "or"

                  -- Screens
                  , "screen"
                  , "tab"
                  , "row"
                  , "column"
                  , "form"
                  , "markdown"
                  , "when"
                  , "table"
                  , "chat"
                  , "messages"
                  , "media"

                  ]
                , caseSensitive:   true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '%', '&', '*', '+', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']

-----------------------------------------------------------
-- reservedIdentifier
-----------------------------------------------------------
-- | Parse one of the reserved identifiers.
reservedIdentifier :: StringPositionParser
reservedIdentifier = token.lexeme $ try go
  where
    go :: StringPositionParser
    go = do
        name <- ident
        if (isReservedPerspectivesName name)
           then pure name
           else fail ("not a reserved word " <> show name <> "(or unexpected end of input), ")

    isReservedPerspectivesName :: String -> Boolean
    isReservedPerspectivesName = isReservedName perspectDef


    ident :: StringPositionParser
    ident = go' perspectDef <?> "identifier, "
      where
        go' :: IndentLanguageDef -> StringPositionParser
        go' (LanguageDef languageDef) = do
            cs <- Array.many languageDef.identLetter
            pure $ SCU.fromCharArray cs

-----------------------------------------------------------
-- Identifiers & Reserved words
-- This section is replicated from Parsing.Token.
-- We need isReservedName but it is not exported from Parsing.Token.
-----------------------------------------------------------

isReservedName :: forall m . Monad m => GenLanguageDef String m -> String -> Boolean
isReservedName langDef@(LanguageDef languageDef) name =
    isReserved (theReservedNames langDef) caseName
  where
    caseName | languageDef.caseSensitive  = name
             | otherwise                  = toLower name

isReserved :: Array String -> String -> Boolean
isReserved names name =
    case Array.uncons names of
        Nothing -> false
        Just { head: r, tail: rs } -> case (compare r name) of
                                        LT  -> isReserved rs name
                                        EQ  -> true
                                        GT  -> false

theReservedNames :: forall m . Monad m => GenLanguageDef String m -> Array String
theReservedNames (LanguageDef languageDef)
    | languageDef.caseSensitive = Array.sort languageDef.reservedNames
    | otherwise                 = Array.sort $ map toLower languageDef.reservedNames
