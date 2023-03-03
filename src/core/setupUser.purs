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

module Perspectives.SetupUser where

import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Extern.Couchdb (addModelToLocalStore')
import Perspectives.ModelDependencies (sysUser, systemModelName)
import Perspectives.Persistent (entitiesDatabaseName, modelDatabaseName)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.SetupCouchdb (setContextSpecialisationsView, setContextView, setPendingInvitationView, setRoleFromContextView, setRoleSpecialisationsView, setRoleView)
import Prelude (Unit, void, ($), discard, (>>=))

modelDirectory :: String
modelDirectory = "./src/model" 

-- | Set up by adding model:System and dependencies to the users' models database. This will add the model instances, too.
-- | This function also ensures CURRENTUSER.
-- | Note that the repository should have model://perspectives.domains#Couchdb, model://perspectives.domains#Serialise and model://perspectives.domains#System.
setupUser :: MonadPerspectives Unit
setupUser = do
  -- First, upload model:System to perspect_models.
  void $ runMonadPerspectivesTransaction (ENR $ EnumeratedRoleType sysUser) (addModelToLocalStore' (DomeinFileId systemModelName) true)
  entitiesDatabaseName >>= setRoleView
  entitiesDatabaseName >>= setRoleFromContextView
  entitiesDatabaseName >>= setPendingInvitationView
  entitiesDatabaseName >>= setContextView
  modelDatabaseName >>= setRoleSpecialisationsView
  modelDatabaseName >>= setContextSpecialisationsView