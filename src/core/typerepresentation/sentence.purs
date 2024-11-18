-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2021 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
module Perspectives.Representation.Sentence where

import Prelude

import Control.Alt ((<|>))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

-- The String part is the original sentence where the expressions have been replaced
-- by replacement identifiers. So for example "Hello {Person}, how are you?" has become
-- "Hello $1, how are you?"
-- The Array of SentenceParts contains the computable parts, where a parts position in the array
-- corresponds to the replacement identifier in the string.
newtype Sentence = Sentence {sentence :: String, parts :: Array SentencePart}

derive instance newtypeSentence :: Newtype Sentence _
derive newtype instance showSentence :: Show Sentence
derive newtype instance eqSentence :: Eq Sentence

derive newtype instance ReadForeign Sentence
derive newtype instance WriteForeign Sentence

data SentencePart =
    HR String
  | CP QueryFunctionDescription

derive instance genericSentencePart :: Generic SentencePart _
instance showSentencePart :: Show SentencePart where show = genericShow
instance eqSentencePart :: Eq SentencePart where eq = genericEq

instance WriteForeign SentencePart where
  writeImpl (HR s) = writeImpl { constructor: "HR", s }
  writeImpl (CP calculation) = writeImpl { constructor: "CP", calculation }

instance ReadForeign SentencePart where
  readImpl f = 
    do
      {constructor, s} :: {constructor :: String, s :: String} <- read' f
      pure $ HR s
    <|>
    do
      {constructor, calculation} :: {constructor :: String, calculation :: QueryFunctionDescription} <- read' f
      pure $ CP calculation