module Perspectives.Parsing.TransferFile.Token where

import Control.Alt ((<|>))
import Control.Monad (class Monad)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, alphaNum, letter, makeTokenParser)

type IndentTokenParser = GenTokenParser String (StateT Position Identity)

token :: IndentTokenParser
token = makeTokenParser perspectDef

type IndentLanguageDef = GenLanguageDef String (StateT Position Identity)

perspectDef :: IndentLanguageDef
-- perspectDef = LanguageDef (unGenLanguageDef haskellStyle)
--                 { reservedOpNames = ["=", "=>"]
--                 , reservedNames   = [ "intern","extern"]
--                 }
-- TODO: VERANDER DE BEHANDELING VAN COMMENTAAR.
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
                  , "RoleInContext"
                  , "ExternalRole"
                  , "UserRole"
                  , "BotRole"
                  , "Mandatory"
                  , "mandatory"
                  , "Functional"
                  , "functional"
                  , "Calculated"
                  , "BooleanProperty"
                  , "NumberProperty"
                  , "StringProperty"
                  , "DateTimeProperty"
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
                  , "ObjectRef"
                  , "FilledBy"
                  , "False"
                  , "True"
                  , "DefaultObjectViewRef"
                  , "IndirectObjectRef"
                  , "SubjectViewRef"
                  , "ObjectViewRef"
                  , "IndirectObjectViewRef"
                  ]
                , caseSensitive:   true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf ['!', '$', '%', '&', '*', '+', '.', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
