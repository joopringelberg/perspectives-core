module Perspectives.Token where

import Control.Alt ((<|>))
import Control.Monad (class Monad)
import Control.Monad.State (State)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, letter, makeTokenParser)

type IndentTokenParser = GenTokenParser String (State Position)

token :: IndentTokenParser
token = makeTokenParser perspectDef

-- type LanguageDef = GenLanguageDef String Identity

type IndentLanguageDef = GenLanguageDef String (State Position)

perspectDef :: IndentLanguageDef
-- perspectDef = LanguageDef (unGenLanguageDef haskellStyle)
--                 { reservedOpNames = ["=", "=>"]
--                 , reservedNames   = [ "private","public"]
--                 }
perspectDef = LanguageDef
                { commentStart    : "{-"
                , commentEnd      : "-}"
                , commentLine     : "--"
                , nestedComments:  true
                , identStart      : letter <|> char ':'
                , identLetter:     alphaNum <|> oneOf ['_', '\'']
                , opStart:         op'
                , opLetter:        op'
                , reservedOpNames : ["=", "=>"]
                , reservedNames   : [ "private","public", "DEF"]
                , caseSensitive:   true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
