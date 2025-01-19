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

module Perspectives.DataUpgrade.RecompileLocalModels where

import Control.Monad.Except (runExceptT)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Main.RecompileBasicModels (UninterpretedDomeinFile, executeInTopologicalOrder, recompileModel)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (createDatabase, deleteDatabase, documentsInDatabase, includeDocs)
import Perspectives.Persistent (invertedQueryDatabaseName, saveMarkedResources)
import Perspectives.PerspectivesState (modelsDatabaseName)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.SetupUser (setupInvertedQueryDatabase)
import Prelude (Unit, bind, discard, pure, show, ($), (*>), (<$>), (<>))
import Simple.JSON (read) as JSON

recompileLocalModels :: MonadPerspectives Boolean
recompileLocalModels =
  do
    addAllExternalFunctions
    modelsDb <- modelsDatabaseName
    {rows:allModels} <- documentsInDatabase modelsDb includeDocs
    -- As doc is still uninterpreted, we can only rely on the rows.id member of the PouchdbAllDocs record. These, however, are DomeinFileIdentifiers.
    -- We do not have a useful test on the form of such identifiers.
    uninterpretedDomeinFiles <- for allModels \({id, doc}) -> case JSON.read <$> doc of
      Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
      Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
      Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
    clearInvertedQueriesDatabase
    r <- runMonadPerspectivesTransaction'
      false
      (ENR $ EnumeratedRoleType sysUser)
      (runExceptT (executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModel))
    case r of
      Left errors -> logPerspectivesError (Custom ("recompileLocalModels: " <> show errors)) *> pure false
      Right success -> do 
        saveMarkedResources
        pure success
  where

    clearInvertedQueriesDatabase :: MonadPerspectives Unit
    clearInvertedQueriesDatabase = do
      db <- invertedQueryDatabaseName
      deleteDatabase db
      createDatabase db
      setupInvertedQueryDatabase
