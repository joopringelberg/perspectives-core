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

module Perspectives.External.CoreModules
( addAllExternalFunctions
, addExternalFunctionForModule
)

where

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable, lookup)
import Perspectives.Extern.Couchdb (externalFunctions) as ExternalCouchdb
import Perspectives.Extern.Parsing (externalFunctions) as Parsing
import Perspectives.Extern.RabbitMQ (externalFunctions) as RabbitMQ
import Perspectives.Extern.Sensors (externalFunctions) as Sensor
import Perspectives.Extern.Utilities (externalFunctions) as Utilities
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription, hiddenFunctionInsert)
import Perspectives.Instances.SerialiseAsJson (externalFunctions) as Serialise
import Prelude (class Monad, Unit, discard, pure, unit, ($))

type ExternalFunctions = Array (Tuple String HiddenFunctionDescription)

coreModules :: Object ExternalFunctions
coreModules = fromFoldable
  [ Tuple "model:Couchdb" ExternalCouchdb.externalFunctions
  , Tuple "model:Serialise" Serialise.externalFunctions
  , Tuple "model:Parsing" Parsing.externalFunctions
  , Tuple "model:Utilities" Utilities.externalFunctions
  , Tuple "model:Sensor" Sensor.externalFunctions
  , Tuple "model:RabbitMQ" RabbitMQ.externalFunctions
  ]

addAllExternalFunctions :: forall m. Monad m => m Unit
addAllExternalFunctions = do
  addExternalFunctions ExternalCouchdb.externalFunctions
  addExternalFunctions Serialise.externalFunctions
  addExternalFunctions Parsing.externalFunctions
  addExternalFunctions Utilities.externalFunctions
  addExternalFunctions Sensor.externalFunctions
  addExternalFunctions RabbitMQ.externalFunctions

addExternalFunctions :: forall m. Monad m => Array (Tuple String HiddenFunctionDescription) -> m Unit
addExternalFunctions externalFunctions = for_ externalFunctions \(Tuple n f) -> pure $ hiddenFunctionInsert n f.func f.nArgs

addExternalFunctionForModule :: forall m. Monad m => String -> m Unit
addExternalFunctionForModule moduleName =
  case lookup moduleName coreModules of
    Nothing -> pure unit
    Just functions -> addExternalFunctions functions
