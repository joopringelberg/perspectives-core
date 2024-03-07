-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

-- | A 'data-upgrade' is a procudure that is carried out on stored data of an installation, in order to ensure
-- | that they can be handled by a new version of the PDR.

module Perspectives.DataUpgrade  where


import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import IDBKeyVal (idbGet, idbSet)
import Perspectives.Extern.Utilities (pdrVersion)
import Unsafe.Coerce (unsafeCoerce)

type PDRVersion = String

runDataUpgrades :: Aff Unit
runDataUpgrades = do
  -- Get the current version
  mcurrentVersion <- idbGet "CurrentPDRVersion"
  case mcurrentVersion of
    Just currentVersion -> do
      ----------------------------------------------------------------------------------------
      ------- PDR VERSION 0.23
      ----------------------------------------------------------------------------------------
      runUpgrade (unsafeCoerce currentVersion) "0.23" addFixingUpdates



      -- Add new upgrades above this line and provide the pdr version number in which they were introduced.
      ----------------------------------------------------------------------------------------
      ------- SET CURRENT VERSION
      ----------------------------------------------------------------------------------------
      idbSet "CurrentPDRVersion" (unsafeCoerce pdrVersion)
    _ -> pure unit

runUpgrade :: PDRVersion -> PDRVersion -> Aff Unit -> Aff Unit
runUpgrade currentVersion upgradeVersion upgrade = if upgradeVersion <= currentVersion && pdrVersion < upgradeVersion
    -- Run the upgrade
    then upgrade
    else pure unit

addFixingUpdates :: Aff Unit
addFixingUpdates = pure unit

