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

module Perspectives.Parser where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Perspectives.ContextRoleParser (expression, userData)
import Perspectives.CoreTypes (MonadPerspectives)

import Perspectives.IndentParser (runIndentParser)
import Prelude ((*>), (-), bind, pure, ($))
import Text.Parsing.Parser (ParseError(..))
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (whiteSpace)

type AceError =
  { row :: Int
  , column :: Int
  , text :: String
  , type :: String }

parse :: String -> MonadPerspectives (Either (Array AceError) String)
parse s = do
  parseResult <- runIndentParser s userData
  case parseResult of
    (Left (ParseError message (Position{line, column}))) -> pure $ Left [
      { row: line - 1
      , column: column
      , text: message
      , type: "error"
      }]
    otherwise -> pure $ Right "Parse succeeded, resources stored!"

errorsIn :: String -> String -> MonadPerspectives (Maybe (Array AceError))
errorsIn previousLine s = do
  parseResult <- runIndentParser s (whiteSpace *> expression)
  case parseResult of
    (Left (ParseError _ (Position{line, column}))) -> do
      message <- expressionTypeForNextLine previousLine
      pure $ Just [
      { row: line - 1
      , column: column
      , text: message
      , type: "error"
      }]
    otherwise -> pure Nothing

expressionTypeForNextLine :: String -> MonadPerspectives String
expressionTypeForNextLine s = do
  parseResult <- runIndentParser s (whiteSpace *> expression)
  case parseResult of
    (Left _) -> pure "Kan de vorige regel niet ontleden."
    (Right etype) -> pure $ case etype of
      "enclosingContextDeclaration" -> "Verwacht: Import of Section of rolbinding."
      "importExpression" -> "Verwacht: Import of Section of rolbinding."
      "sectionHeading" -> "Verwacht: context declaratie"
      "contextDeclaration" -> "Verwacht: (extern|intern) property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
      "publicContextPropertyAssignment" -> "Verwacht: extern|intern property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
      "privateContextPropertyAssignment" -> "Verwacht: intern property = value, rol => (rol|context) of rol => met type declaratie op volgende regel."
      "rolePropertyAssignment" -> "Verwacht: property = value"
      "isRoleDeclaration" -> "Verwacht: property = value"
      "oneLineComment" -> "Alles kan."
      otherwise -> "Verwacht: -- commentaar of: Type Instantie"
