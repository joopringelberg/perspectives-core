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

-- | A 'data-upgrade' is a procedure that is carried out on stored data of an installation, in order to ensure
-- | that they can be handled by a new version of the PDR.

module Perspectives.DataUpgrade where


import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Foreign (unsafeToForeign)
import IDBKeyVal (idbGet, idbSet)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Extern.Utilities (pdrVersion)
import Perspectives.Persistent (entitiesDatabaseName)
import Perspectives.SetupCouchdb (setContext2RoleView, setFilled2FillerView, setFiller2FilledView, setRole2ContextView)
import Unsafe.Coerce (unsafeCoerce)

type PDRVersion = String

runDataUpgrades :: MonadPerspectives Unit
runDataUpgrades = do
  -- Get the current version
  mcurrentVersion <- liftAff $ idbGet "CurrentPDRVersion"
  (installedVersion :: String) <- case mcurrentVersion of
    Just installedVersion -> pure (unsafeCoerce installedVersion)
    Nothing -> do 
      -- This mechanism was introduced during development of version 0.25.
      -- Installations existing prior to 0.25 will be brought to heel with these instructions.
      liftAff $ idbSet "CurrentPDRVersion" (unsafeToForeign "0.24")
      pure "0.24"

  ----------------------------------------------------------------------------------------
  ------- PDR VERSION 0.24.1
  ------- In this version, we add views for automatic referential integrity fixing.
  ----------------------------------------------------------------------------------------
  runUpgrade installedVersion "0.24.1" addFixingUpdates 



  -- Add new upgrades above this line and provide the pdr version number in which they were introduced.
  ----------------------------------------------------------------------------------------
  ------- SET CURRENT VERSION
  ----------------------------------------------------------------------------------------
  if installedVersion < pdrVersion
    then liftAff $ idbSet "CurrentPDRVersion" (unsafeToForeign pdrVersion)
    else pure unit

runUpgrade :: PDRVersion -> PDRVersion -> MonadPerspectives Unit -> MonadPerspectives Unit
runUpgrade installedVersion upgradeVersion upgrade = if installedVersion < upgradeVersion && upgradeVersion <= pdrVersion
  -- Run the upgrade
  then upgrade
  else pure unit

addFixingUpdates :: MonadPerspectives Unit
addFixingUpdates = do
  db <- entitiesDatabaseName
  setFiller2FilledView db
  setFilled2FillerView db
  setContext2RoleView db
  setRole2ContextView db

