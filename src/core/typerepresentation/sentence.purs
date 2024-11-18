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

import Data.Newtype (class Newtype)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Simple.JSON (class ReadForeign, class WriteForeign)

-- The String part is the original sentence where the expressions have been replaced
-- by replacement identifiers. So for example "Hello {Person}, how are you?" has become
-- "Hello $1, how are you?"
-- The Array of SentenceParts contains the computable parts, where a parts position in the array
-- corresponds to the replacement identifier in the string.
newtype Sentence = Sentence {sentence :: String, parts :: Array QueryFunctionDescription}

derive instance newtypeSentence :: Newtype Sentence _
derive newtype instance showSentence :: Show Sentence
derive newtype instance eqSentence :: Eq Sentence

derive newtype instance ReadForeign Sentence
derive newtype instance WriteForeign Sentence
