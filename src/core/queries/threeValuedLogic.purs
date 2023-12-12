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

module Perspectives.Representation.ThreeValuedLogic where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Foreign (unsafeToForeign)
import Perspectives.Representation.Class.EnumReadForeign (enumReadForeign)
import Simple.JSON (class ReadForeign, class WriteForeign)

data ThreeValuedLogic = True | Unknown | False

instance writeForeignThreeValuedLogic :: WriteForeign ThreeValuedLogic where
  writeImpl = unsafeToForeign <<< show

instance readForeignThreeValuedLogic :: ReadForeign ThreeValuedLogic where
  readImpl = enumReadForeign

derive instance genericRepThreeValuedLogic :: Generic ThreeValuedLogic _

instance showThreeValuedLogic :: Show ThreeValuedLogic where
  show = genericShow

instance eqThreeValuedLogic :: Eq ThreeValuedLogic where
  eq = genericEq

derive instance ordThreeValuedLogic :: Ord ThreeValuedLogic

bool2threeValued :: Boolean -> ThreeValuedLogic
bool2threeValued true = True
bool2threeValued false = False

pessimistic :: ThreeValuedLogic -> Boolean
pessimistic True = true
pessimistic _ = false

optimistic :: ThreeValuedLogic -> Boolean
optimistic False = false
optimistic _ = true

and :: ThreeValuedLogic -> ThreeValuedLogic -> ThreeValuedLogic
and True True = True
and Unknown _ = Unknown
and _ Unknown = Unknown
and _ _ = False

or :: ThreeValuedLogic -> ThreeValuedLogic -> ThreeValuedLogic
or True _ = True
or _ True = True
or Unknown _ = Unknown
or _ Unknown = Unknown
or _ _ = False
