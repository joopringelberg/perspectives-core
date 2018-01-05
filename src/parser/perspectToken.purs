module Perspectives.Token where

import Control.Alt ((<|>))
import Control.Monad (class Monad)
import Perspectives.IndentParser (ContextRoleParserMonad)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, letter, makeTokenParser)

-- type IndentTokenParser e = GenTokenParser String (StateT Position (Aff e))
type IndentTokenParser e = GenTokenParser String (ContextRoleParserMonad e)

token :: forall e. IndentTokenParser e
token = makeTokenParser perspectDef

-- type LanguageDef = GenLanguageDef String Identity

-- type IndentLanguageDef e = GenLanguageDef String (StateT Position (Aff e))
type IndentLanguageDef e = GenLanguageDef String (ContextRoleParserMonad e)

perspectDef :: forall e. IndentLanguageDef e
-- perspectDef = LanguageDef (unGenLanguageDef haskellStyle)
--                 { reservedOpNames = ["=", "=>"]
--                 , reservedNames   = [ "private","public"]
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
                , reservedOpNames : ["=", "=>"]
                , reservedNames   : [ "private","public", "property", "Section", "Context", "als"]
                , caseSensitive:   true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf ['!', '$', '%', '&', '*', '+', '.', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
