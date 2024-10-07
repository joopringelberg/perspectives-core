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

module Perspectives.SystemClocks where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time (Time, adjust, hour, minute)
import Data.Time.Duration (Hours(..), Minutes(..), fromDuration, negateDuration)
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset, nowDate, nowTime)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes (PerspectivesState, MonadPerspectivesTransaction, (##>))
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.ModelDependencies (currentSystemDate, currentSystemHour, sysUser)
import Perspectives.Names (getMySystem)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.Time (date2String, string2Date, string2Time, time2String, wholeHour)

forkedSystemClocks :: AVar PerspectivesState -> Aff Unit 
forkedSystemClocks state = do 
  -- At system startup, always make sure the clocks are correct.
  runPerspectivesWithState 
    (runMonadPerspectivesTransaction 
      (ENR $ EnumeratedRoleType sysUser)
      setClocks)
    state
  -- Do this again on the next full hour.
  n <- liftEffect nowTime
  delay (fromDuration $ Minutes $ toNumber (sub 60 (fromEnum $ minute n)))
  runPerspectivesWithState 
    (runMonadPerspectivesTransaction 
      (ENR $ EnumeratedRoleType sysUser)
      setClocks)
    state
  -- And now repeat every hour.
  runEveryHour
  
  where
    -- Make sure PerspectivesSystem$Extern$CurrentDate equals the current date (with a time component of 00:00, i.e. midnight)
    -- and that PerspectivesSystem$Extern$CurrentHour equals the current hour.
    -- This is a zero-op if that is already the case.
    setClocks :: MonadPerspectivesTransaction Unit
    setClocks = do 
      -- Read the registered hour and date.
      system <- lift $ getMySystem
      msystemHour <- lift (RoleInstance (buitenRol system) ##> getProperty (EnumeratedPropertyType currentSystemHour))
      msystemDate <- lift (RoleInstance (buitenRol system) ##> getProperty (EnumeratedPropertyType currentSystemDate))
      -- read the current time.
      (machineClockTime :: Time) <- liftEffect $ nowTime
      (offset :: Minutes) <- liftEffect $ getTimezoneOffset
      Tuple _ currentTime <- pure $ adjust (negateDuration offset) machineClockTime
      case msystemHour of 
        Just (Value systemHour) -> do 
          systemHour' <- string2Time systemHour
          -- if the hour of the currentTime is not equal to the systemHour', set the currentTime to the hour of the currentTime.
          if (hour currentTime) `notEq` (hour systemHour')
            then setProperty [RoleInstance $ buitenRol system] (EnumeratedPropertyType currentSystemHour)  Nothing
              [Value $ time2String (wholeHour (hour currentTime))]
            else pure unit
        Nothing -> setProperty [RoleInstance $ buitenRol system] (EnumeratedPropertyType currentSystemHour) Nothing [Value $ time2String (wholeHour $ hour currentTime)]
      -- read the current date.
      currentDate <- liftEffect $ nowDate
      case msystemDate of 
        Just (Value systemDate) -> do 
          systemDate' <- unsafePartial fromJust <$> string2Date systemDate
          -- Compare the date component of PerspectivesSystem$CurrentDate with the current date. If it is less, set CurrentDate.
          if systemDate' < currentDate
            then setProperty [RoleInstance $ buitenRol system] (EnumeratedPropertyType currentSystemDate) Nothing
              [Value $ date2String currentDate]
            else pure unit
        Nothing -> setProperty [RoleInstance $ buitenRol system] (EnumeratedPropertyType currentSystemDate) Nothing [Value $ date2String currentDate]
      
      pure unit

    runEveryHour :: Aff Unit
    runEveryHour = do
      delay (fromDuration (Hours 1.0))
      runPerspectivesWithState 
        (runMonadPerspectivesTransaction 
          (ENR $ EnumeratedRoleType sysUser)
          setClocks)
        state
      runEveryHour

