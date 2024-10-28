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

module Perspectives.Time  where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.DateTime (Date, DateTime, Hour, Time(..), date, hour, millisecond, minute, second)
import Data.DateTime.Instant (fromDate, fromDateTime, instant, toDateTime, unInstant)
import Data.Enum (fromEnum, toEnum)
import Data.Number (floor)
import Data.Int as Int
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Perspectives.Instances.Values (parseNumber)
import Simple.JSON (unsafeStringify)

-- | Convert a DateTime value to the string representation of its Instant value.
-- | This is a step in serialisation.
dateTime2String :: DateTime -> String
dateTime2String dt = unsafeStringify $ unwrap $ unInstant $ fromDateTime dt

string2DateTime :: forall m. MonadError Error m => MonadEffect m => String -> m (Maybe DateTime)
string2DateTime s = map toDateTime <<< instant <<< Milliseconds <$> parseNumber s 

date2String :: Date -> String
date2String dt = unsafeStringify $ unwrap $ unInstant $ fromDate dt

string2Date :: forall m. MonadError Error m => MonadEffect m => String -> m (Maybe Date)
string2Date s = map (date <<< toDateTime) <<< instant <<< Milliseconds <$> parseNumber s 

time2String :: Time -> String
time2String t = unsafeStringify $ unwrap $ timeToMillis t

string2Time :: forall m. MonadError Error m => MonadEffect m => String -> m Time
string2Time s = millisToTime <<< Milliseconds <$> parseNumber s

-- | Create a Time that is the full hour of the input. E.g. "16:14" becomes "16:00"
wholeHour :: Hour -> Time
wholeHour h = Time h (unsafePartial $ fromJust $ toEnum 0) (unsafePartial $ fromJust $ toEnum 0) (unsafePartial $ fromJust $ toEnum 0)

---------------------------------------------------------------------------------------------------
---- COPIED FROM Date.Time, as these functions are sadly not exported from that module.
---------------------------------------------------------------------------------------------------
timeToMillis :: Time -> Milliseconds
timeToMillis t = Milliseconds
  $ 3600000.0 * Int.toNumber (fromEnum (hour t))
  + 60000.0 * Int.toNumber (fromEnum (minute t))
  + 1000.0 * Int.toNumber (fromEnum (second t))
  + Int.toNumber (fromEnum (millisecond t))

millisToTime :: Milliseconds -> Time
millisToTime (Milliseconds ms') =
  let
    hourLength = 3600000.0
    minuteLength = 60000.0
    secondLength = 1000.0
    hours = floor (ms' / hourLength)
    minutes = floor ((ms' - hours * hourLength) / minuteLength)
    seconds = floor ((ms' - (hours * hourLength + minutes * minuteLength)) / secondLength)
    milliseconds = ms' - (hours * hourLength + minutes * minuteLength + seconds * secondLength)
  in
    unsafePartial fromJust $
      Time
        <$> toEnum (Int.floor hours)
        <*> toEnum (Int.floor minutes)
        <*> toEnum (Int.floor seconds)
        <*> toEnum (Int.floor milliseconds)
