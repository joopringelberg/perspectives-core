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

import Control.Monad.Except (runExceptT)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (for)
import Effect.Aff.Class (liftAff)
import Foreign (unsafeToForeign)
import IDBKeyVal (idbGet, idbSet)
import Main.RecompileBasicModels (UninterpretedDomeinFile, executeInTopologicalOrder, recompileModel)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (modelsDatabaseName)
import Perspectives.Extern.Utilities (pdrVersion)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (createDatabase, databaseInfo, documentsInDatabase, includeDocs)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (entitiesDatabaseName, getDomeinFile, saveEntiteit_, saveMarkedResources)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.SetupCouchdb (setContext2RoleView, setFilled2FillerView, setFiller2FilledView, setRole2ContextView)
import Perspectives.SetupUser (setupInvertedQueryDatabase)
import Simple.JSON (read)
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
  runUpgrade installedVersion "0.24.2" indexedQueries 



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

indexedQueries :: MonadPerspectives Unit
indexedQueries = do
  addAllExternalFunctions
  user <- getSystemIdentifier
  -- Create the indexedQueries database
  createDatabase $ user <> "_invertedqueries"
  void $ databaseInfo $ user <> "_invertedqueries"
  -- set all the views
  setupInvertedQueryDatabase
  -- Fix the source of model:System
  (DomeinFile dfr) <- getDomeinFile (DomeinFileId "model://perspectives.domains#System")
  void $ saveEntiteit_ (DomeinFileId "model://perspectives.domains#System")
    (DomeinFile dfr {arc = replace (Pattern "    state InitMe = not exists Me") (Replacement "\n    external \n      aspect sys:RootContext$External\n    state InitMe = not exists Me") dfr.arc})
  -- recompile local models.
  modelsDb <- modelsDatabaseName
  {rows:allModels} <- documentsInDatabase modelsDb includeDocs
  -- As doc is still uninterpreted, we can only rely on the rows.id member of the PouchdbAllDocs record. These, however, are DomeinFileIdentifiers.
  -- We do not have a useful test on the form of such identifiers.
  uninterpretedDomeinFiles <- for allModels \({id, doc}) -> case read <$> doc of
    Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
    Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
    Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
  r <- runMonadPerspectivesTransaction'
    false
    (ENR $ EnumeratedRoleType sysUser)
    (runExceptT (executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModel))
  case r of
    Left errors -> logPerspectivesError (Custom ("recompileLocalModels: " <> show errors))
    Right success -> saveMarkedResources
