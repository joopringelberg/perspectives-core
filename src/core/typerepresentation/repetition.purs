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

module Perspectives.Repetition where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

-- | Types Duration and DurationComponent have been copied from module Data.Interval.Duration,
-- | because they need to be instances of Encode and Decode.
data Duration = 
  Millisecond Number 
  | Second Number 
  | Minute Number 
  | Hour Number 
  | Day Number

derive instance genericDuration :: Generic Duration _
instance showDuration :: Show Duration where show = genericShow
instance eqDuration :: Eq Duration where eq = genericEq
instance encodeDuration :: Encode Duration where encode = genericEncode defaultOptions
instance decodeDuration :: Decode Duration where decode = genericDecode defaultOptions

data Repeater = 
  Never
  | Forever Duration
  | RepeatFor Int Duration

derive instance genericRepeater :: Generic Repeater _
instance showRepeater :: Show Repeater where show = genericShow
instance eqRepeater :: Eq Repeater where eq = genericEq
instance encodeRepeater :: Encode Repeater where encode = genericEncode defaultOptions
instance decodeRepeater :: Decode Repeater where decode = genericDecode defaultOptions

fromDuration :: Duration -> Milliseconds
fromDuration (Millisecond n) = Milliseconds n
fromDuration (Second n) = Milliseconds (1000.0 * n)
fromDuration (Minute n) = Milliseconds (60000.0 * n)
fromDuration (Hour n) = Milliseconds (3600000.0 * n)
fromDuration (Day n) = Milliseconds (86400000.0 * n)