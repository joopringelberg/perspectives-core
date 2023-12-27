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

module Perspectives.Sync.DateTime where

import Data.DateTime (DateTime(..), Time(..), day, month, year)
import Data.DateTime.Instant (fromDateTime, instant, toDateTime, unInstant)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.Utilities (class PrettyPrint)
import Prelude (class Eq, class Ord, class Show, bind, pure, show, ($), (<>))
import Simple.JSON (class ReadForeign, class WriteForeign, read', readJSON', writeImpl)

-----------------------------------------------------------
-- DATETIME
-- We need a newtype for DateTime in order to be able to serialize and show it.
-----------------------------------------------------------
newtype SerializableDateTime = SerializableDateTime DateTime

instance showSerializableDateTime :: Show SerializableDateTime where
  -- show (SerializableDateTime d) = replace (Pattern " ") (Replacement "_") (show d)
  show (SerializableDateTime (DateTime d (Time hour minute second millisecond))) = replace (Pattern " ") (Replacement "_") ((show $ year d) <> (show $ month d) <> (show $ day d) <> "T" <> (show hour) <> (show minute) <> (show second) <> (show millisecond))

instance prettyPrintSerializableDateTime :: PrettyPrint SerializableDateTime where
  prettyPrint' t = show

derive instance eqSerializableDateTime :: Eq SerializableDateTime
derive instance ordSerializableDateTime :: Ord SerializableDateTime

instance WriteForeign SerializableDateTime where
  writeImpl (SerializableDateTime d) = case unInstant (fromDateTime d) of
    (Milliseconds n) -> writeImpl (show n)

instance ReadForeign SerializableDateTime where
  readImpl d = do
    s :: String <- read' d
    n :: Number <- readJSON' s
    pure $ SerializableDateTime $ toDateTime $ unsafePartial $ fromJust $ instant (Milliseconds n)
