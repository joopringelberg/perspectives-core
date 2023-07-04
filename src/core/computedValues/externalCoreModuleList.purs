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

-- | This module doubles information in Perspectives.External.CoreModules.
-- | However, we need `isExternalCoreModule` in Perspectives.Parsing.Arc.PhaseThree.
-- | If we'd import it from Perspectives.External.CoreModules, there would be a module cycle
-- | because of the external module Perspectives.Extern.Parsing, that imports PhaseThree via
-- | Perspectives.TypePersistence.LoadArc.

module Perspectives.External.CoreModuleList

where

import Data.Array (elemIndex)
import Data.Maybe (isJust)

coreModules :: Array String
coreModules = 
  [ "model://perspectives.domains#Couchdb"
  , "model://perspectives.domains#Serialise"
  , "model://perspectives.domains#Parsing"
  , "model://perspectives.domains#Utilities"
  , "model://perspectives.domains#Sensor"
  , "model://perspectives.domains#RabbitMQ"
  , "model://perspectives.domains#Files"
  ]

isExternalCoreModule :: String -> Boolean
isExternalCoreModule n = isJust (elemIndex n coreModules)
