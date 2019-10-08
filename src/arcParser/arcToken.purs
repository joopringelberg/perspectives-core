module Perspectives.Parsing.Arc.Token where

import Control.Alt ((<|>))
import Control.Monad (class Monad)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, makeTokenParser, upper)

type IndentTokenParser = GenTokenParser String (StateT Position Identity)

token :: IndentTokenParser
token = makeTokenParser perspectDef

type IndentLanguageDef = GenLanguageDef String (StateT Position Identity)

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
                , identLetter:     alphaNum <|> oneOf ['_', '$']
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
                  ]
                , caseSensitive:   true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '%', '&', '*', '+', '.', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
