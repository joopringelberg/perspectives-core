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

-- | This module defines External Core functions for model://perspectives.domains#Couchdb.

module Perspectives.Extern.Sensors where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, singleton)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import LRUCache (size)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectives)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Error.Boundaries (handleExternalFunctionError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

-- | This module gives access to various `devices` with `sensors` that can be read to yield a value.
-- | These values must all be returned as Strings in MonadPerspectivesQuery. 
-- | However, on applying a sensor function using callExternal, one must specify a Range result type and this
-- | need not be PString. 
-- | All arguments are provided as (Arrays of) String. If another type is required, the sensor function must
-- | transform it from String!
-- | Each device-sensor combination maps to a specific reading function implementation.
-- | Access all these functions with a single external function in the `model:Sensor`` namespace: ReadSensor.

-- | A Device represents an object in software or hardware that has sensors that can be read.
type Device = String

-- | A sensor is like a dial or gauge that can be read, yielding a numerical value.
type Sensor = String

type SensorFunction = Device -> Sensor -> MonadPerspectives String

-- | A Map holding all implmented SensorFunctions, identified by the combination of Device and Sensor.
type DeviceSensorMap = Map (Tuple Device Sensor) SensorFunction

sensorFunctions :: DeviceSensorMap
sensorFunctions = fromFoldable
  [ Tuple (Tuple "contextcache" "size") (unsafePartial cacheSize)
  , Tuple (Tuple "rolecache" "size") (unsafePartial cacheSize)
  , Tuple (Tuple "domaincache" "size") (unsafePartial cacheSize)
  , Tuple (Tuple "querycache" "size") (unsafePartial cacheSize)
  , Tuple (Tuple "clock" "now") (unsafePartial currentDate)
  ]

currentDate :: Partial => SensorFunction
currentDate device sensor = case device, sensor of
  "clock", "now" ->do 
    dt <- liftEffect (unsafeCoerce nowDateTime)
    pure $ unsafeCoerce $ write (SerializableDateTime dt)

cacheSize :: Partial => SensorFunction
cacheSize device sensor = show <$> case device, sensor of 
  "contextcache", "size" -> do
    cache <- gets _.contextInstances
    liftEffect $ size cache
  "rolecache", "size" -> do
    cache <- gets _.rolInstances
    liftEffect $ size cache
  "domaincache", "size" -> do
    cache <- gets _.domeinCache
    liftEffect $ size cache
  "querycache", "size" -> do
    cache <- gets _.queryCache
    liftEffect $ size cache

-- | Read a value from a Sensor on a Device.
readSensor :: Array Device -> Array Sensor -> RoleInstance -> MonadPerspectivesQuery String
readSensor device' sensor' _ = try 
  (case head device', head sensor' of
    Just device, Just sensor -> ArrayT case lookup (Tuple device sensor) sensorFunctions of
      Nothing -> pure []
      Just f -> lift $ singleton <$> f device sensor
    _, _ -> ArrayT $ pure [])
  >>= handleExternalFunctionError "model://perspectives.domains#Sensor$ReadSensor"

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Sensor$ReadSensor" {func: unsafeCoerce readSensor, nArgs: 2, isFunctional: True, isEffect: false}
  ]
