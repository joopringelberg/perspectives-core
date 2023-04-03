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
import Data.Show.Generic (genericShow)
import Foreign (unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Prelude (class Eq, class Ord, class Show, show, (<<<))
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- RANGE
-----------------------------------------------------------
-- | PDate is represented as SerializableDateTime.
data Range = PString | PBool | PNumber | PDate | PEmail | PFile

derive instance genericRange :: Generic Range _

instance eqRange :: Eq Range where eq = genericEq
derive instance ordRange :: Ord Range

instance encodeRange :: Encode Range where
  encode = genericEncode defaultOptions

instance writeForeignRange :: WriteForeign Range where
  writeImpl = unsafeToForeign <<< show

instance decodeRange :: Decode Range where
  decode = genericDecode defaultOptions

instance readForeignRange :: ReadForeign Range where
  readImpl = enumReadForeign

instance rangeShow :: Show Range where
  show = genericShow
