-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

module Perspectives.External.HiddenFunctionCache where

import Data.Maybe (Maybe)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, peek, poke, new)
import Perspectives.HiddenFunction (HiddenFunction)
import Prelude ((<$>), unit)

type HiddenFunctionDescription = {func :: HiddenFunction, nArgs :: Int}

type HiddenFunctionCache = GLStrMap HiddenFunctionDescription

hiddenFunctionCache :: HiddenFunctionCache
hiddenFunctionCache = new unit

lookupHiddenFunction :: String -> Maybe HiddenFunction
lookupHiddenFunction name = _.func <$> peek hiddenFunctionCache name

lookupHiddenFunctionNArgs :: String -> Maybe Int
lookupHiddenFunctionNArgs name = _.nArgs <$> peek hiddenFunctionCache name

hiddenFunctionInsert :: String -> HiddenFunction -> Int -> HiddenFunctionCache
hiddenFunctionInsert name func nArgs = poke hiddenFunctionCache name {func, nArgs}
