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
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)

data AutomaticAction = ContextAction QueryFunctionDescription |
  RoleAction
    { currentContextCalculation :: QueryFunctionDescription
    , effect :: QueryFunctionDescription
    }

effectOfAction :: AutomaticAction -> QueryFunctionDescription
effectOfAction (ContextAction effect) = effect
effectOfAction (RoleAction action) = action.effect

derive instance genericAutomaticAction :: Generic AutomaticAction _
instance showAutomaticAction :: Show AutomaticAction where show = genericShow
instance eqAutomaticAction :: Eq AutomaticAction where eq = genericEq
instance encodeAutomaticAction :: Encode AutomaticAction where encode = genericEncode defaultOptions
instance decodeAutomaticAction :: Decode AutomaticAction where decode = genericDecode defaultOptions

newtype Action = Action QueryFunctionDescription
derive instance genericAction :: Generic Action _
derive instance newtypeAction :: Newtype Action _
instance showAction :: Show Action where show = genericShow
instance eqAction :: Eq Action where eq = genericEq
instance encodeAction :: Encode Action where encode = genericEncode defaultOptions
instance decodeAction :: Decode Action where decode = genericDecode defaultOptions
