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

module Perspectives.Representation.Action where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)

data Action = ContextAction QueryFunctionDescription |
  RoleAction
    { currentContextCalculation :: QueryFunctionDescription
  	, effect :: QueryFunctionDescription
  	}

effectOfAction :: Action -> QueryFunctionDescription
effectOfAction (ContextAction effect) = effect
effectOfAction (RoleAction action) = action.effect

derive instance genericAction :: Generic Action _
instance showAction :: Show Action where show = genericShow
instance eqAction :: Eq Action where eq = genericEq
instance encodeAction :: Encode Action where encode = genericEncode defaultOptions
instance decodeAction :: Decode Action where decode = genericDecode defaultOptions
