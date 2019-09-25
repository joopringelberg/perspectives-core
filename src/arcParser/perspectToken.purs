module Perspectives.Parsing.Arc.Token where

import Control.Alt ((<|>))
import Control.Monad (class Monad)
import Perspectives.Parsing.Arc.IndentParser (ArcParserMonad)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, letter, makeTokenParser)

-- type IndentTokenParser e = GenTokenParser String (StateT Position (Aff e))
type IndentTokenParser = GenTokenParser String ArcParserMonad

token :: IndentTokenParser
token = makeTokenParser perspectDef

-- type LanguageDef = GenLanguageDef String Identity

-- type IndentLanguageDef e = GenLanguageDef String (StateT Position (Aff e))
type IndentLanguageDef = GenLanguageDef String ArcParserMonad

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
                , identStart      : letter
                , identLetter:     alphaNum <|> oneOf ['_']
                , opStart:         op'
                , opLetter:        op'
                , reservedOpNames : [":"]
                , reservedNames   :
                  [ "Context"
                  , "Role"
                  , "Property"
                  , "Agent"
                  , "Action"
                  , "View"
                  , "Perspective"
                  , "Domain"
                  , "ContextRole"
                  , "ExternalRole"
                  , "UserRole"
                  , "Mandatory"
                  , "NonFunctional"
                  , "Calculated"
                  , "BooleanProperty"
                  , "NumberProperty"
                  , "StringProperty"
                  , "DateTimeProperty"
                  , "LocalPerspective"
                  , "ScreenLocationX"
                  , "ScreenLocationY"
                  , "ScreenWidth"
                  , "ScreenHeight"
                  , "ActionOnContextDialog"
                  , "ActionOnContextDialogLocationX"
                  , "ActionOnContextDialogLocationY"
                  , "Consults"
                  , "Creates"
                  , "ActionOnScreen"
                  , "ActionObjectView"
                  , "Object"
                  , "Subject"
                  , "FilledBy"
                  , "False"
                  , "True"
                  ]
                , caseSensitive:   true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf ['!', '$', '%', '&', '*', '+', '.', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
