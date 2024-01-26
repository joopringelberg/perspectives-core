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

module Perspectives.Representation.Range where

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Prelude (class Eq, class Ord, class Show, bind, pure, show, ($), (<$>), eq)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- RANGE
-----------------------------------------------------------
-- | PDate is represented as SerializableDateTime.
data Range = PString | PBool | PNumber | PDate | PEmail | PFile | PDuration Duration_

derive instance genericRange :: Generic Range _

instance eqRange :: Eq Range where eq = genericEq
derive instance ordRange :: Ord Range

instance writeForeignRange :: WriteForeign Range where
  writeImpl (PDuration d) = write {range: "PDuration", dtype: show d}
  writeImpl r = write ({ range: show r, dtype: Nothing} :: { range :: String, dtype :: Maybe String})
  -- writeImpl = unsafeToForeign <<< show

instance readForeignRange :: ReadForeign Range where
  readImpl f = do
    (d :: { range :: String, dtype :: Maybe String}) <- readImpl f
    unsafePartial case d.range of 
      -- "PDuration" -> PDuration $ unsafePartial $ fromJust d.dtype
      "PDuration" -> PDuration <$> enumReadForeign (unsafeCoerce (unsafePartial $ fromJust d.dtype))
      "PString" -> pure PString
      "PBool" -> pure PBool
      "PNumber" -> pure PNumber
      "PDate" -> pure PDate
      "PEmail" -> pure PEmail
      "PFile" -> pure PFile
  -- readImpl r = enumReadForeign r

instance rangeShow :: Show Range where
  show = genericShow

data Duration_ = Year_ | Month_ | Week_ | Day_ | Hour_ | Minute_ | Second_ | MilliSecond_

derive instance Generic Duration_ _
instance Show Duration_ where show = genericShow
instance Eq Duration_ where eq = genericEq
derive instance Ord Duration_

isPDate :: Range -> Boolean
isPDate r = r `eq` PDate

isPDuration :: Range -> Boolean
isPDuration (PDuration _) = true
isPDuration _ = false