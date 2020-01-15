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

module Perspectives.External.CoreFunctionsCache where

import Data.Maybe (Maybe)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, peek, poke, new)
import Perspectives.HiddenFunction (HiddenFunction)
import Prelude ((<$>), unit)

type ExternalFunction = {func :: HiddenFunction, nArgs :: Int}

type ExternalFunctionCache = GLStrMap ExternalFunction

externalFunctionCache :: ExternalFunctionCache
externalFunctionCache = new unit

lookupExternalFunction :: String -> Maybe HiddenFunction
lookupExternalFunction name = _.func <$> peek externalFunctionCache name

lookupExternalFunctionNArgs :: String -> Maybe Int
lookupExternalFunctionNArgs name = _.nArgs <$> peek externalFunctionCache name

externalFunctionInsert :: String -> HiddenFunction -> Int -> ExternalFunctionCache
externalFunctionInsert name func nArgs = poke externalFunctionCache name {func, nArgs}
