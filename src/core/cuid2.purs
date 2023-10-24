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
module Perspectives.Cuid2 where

import Effect (Effect)

-- | `createUserIdentifier` is a parameterless javascript function that returns an identifier. 
-- | Hence, it can be seen as an Effect; each time the effect is executed, a new identifier results.
-- | Use it once in an installation to generate the system identifier.
foreign import createSystemIdentifier :: Effect String

-- | Use this function to create unique identifiers with the users' id as fingerprint.
-- | Provide the user's id as argument value.
foreign import cuid2 :: String -> Effect String