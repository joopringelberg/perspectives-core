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

module Perspectives.Parsing.Arc.Position where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

-- | `Position` represents the position of the parser in the input.
-- |
-- | - `line` is the current line in the input
-- | - `column` is the column of the next character in the current line that will be parsed
newtype ArcPosition = ArcPosition
  { line :: Int
  , column :: Int
  }

derive instance genericArcPosition :: Generic ArcPosition _
instance showArcPosition :: Show ArcPosition where show = genericShow

-- | Because we want two expressions to be equal regardless of where in the text they
-- | occur, we make all instances of ArcPosition equal to each other.
instance eqArcPosition :: Eq ArcPosition where eq a b = true
instance ordArcPosition :: Ord ArcPosition where compare a b = EQ

instance decodeArcPosition :: Decode ArcPosition where
  decode = genericDecode defaultOptions
instance encodeArcPosition :: Encode ArcPosition where
  encode = genericEncode defaultOptions

arcParserStartPosition :: ArcPosition
arcParserStartPosition = ArcPosition{line: 1, column: 1}
