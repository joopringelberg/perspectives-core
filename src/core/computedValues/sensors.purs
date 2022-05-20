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

-- | This module defines External Core functions for model:Couchdb.

module Perspectives.Extern.Sensors where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, singleton)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import LRUCache (size)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectives)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Unsafe.Coerce (unsafeCoerce)

-- | This module gives access to various `devices` with `sensors` that can be read to yield a numerical value.
-- | Each device-sensor combination maps to a specific reading function implementation.
-- | Access all these functions with a single external function in the `model:Sensor`` namespace: ReadSensor.

-- | A Device represents an object in software or hardware that has sensors that can be read.
type Device = String

-- | A sensor is like a dial or gauge that can be read, yielding a numerical value.
type Sensor = String

type SensorFunction = Device -> Sensor -> MonadPerspectives Int

-- | A Map holding all implmented SensorFunctions, identified by the combination of Device and Sensor.
type DeviceSensorMap = Map (Tuple Device Sensor) SensorFunction

sensorFunctions :: DeviceSensorMap
sensorFunctions = fromFoldable
  [ Tuple (Tuple "contextcache" "size") (unsafePartial cacheSize)
  , Tuple (Tuple "rolecache" "size") (unsafePartial cacheSize)
  , Tuple (Tuple "domaincache" "size") (unsafePartial cacheSize)
  ]

cacheSize :: Partial => SensorFunction
cacheSize device sensor = case device, sensor of 
  "contextcache", "size" -> do
    cache <- gets _.contextInstances
    liftEffect $ size cache
  "rolecache", "size" -> do
    cache <- gets _.rolInstances
    liftEffect $ size cache
  "domaincache", "size" -> do
    cache <- gets _.domeinCache
    liftEffect $ size cache

-- | Read a value from a Sensor on a Device.
readSensor :: Array Device -> Array Sensor -> RoleInstance -> MonadPerspectivesQuery String
readSensor device' sensor' _ = case head device', head sensor' of
  Just device, Just sensor -> ArrayT case lookup (Tuple device sensor) sensorFunctions of
    Nothing -> pure []
    Just f -> lift $ singleton <<< show <$> f device sensor
  _, _ -> ArrayT $ pure []


-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:Sensor$ReadSensor" {func: unsafeCoerce readSensor, nArgs: 2}
]
