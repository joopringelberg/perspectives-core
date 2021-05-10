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

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Query.QueryTypes (Calculation)

newtype Sentence = Sentence (Array SentencePart)

derive instance newtypeSentence :: Newtype Sentence _
derive newtype instance showSentence :: Show Sentence
derive newtype instance eqSentence :: Eq Sentence
derive newtype instance encodeSentence :: Encode Sentence
derive newtype instance decodeSentence :: Decode Sentence

data SentencePart =
    HR String
  | CP Calculation

derive instance genericSentencePart :: Generic SentencePart _
instance showSentencePart :: Show SentencePart where show = genericShow
instance eqSentencePart :: Eq SentencePart where eq = genericEq
instance encodeSentencePart :: Encode SentencePart where encode = genericEncode defaultOptions
instance decodeSentencPart :: Decode SentencePart where decode = genericDecode defaultOptions
