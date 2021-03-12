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
import Perspectives.Extern.Couchdb (addModelToLocalStore)
import Perspectives.Persistent (entitiesDatabaseName)
import Perspectives.PerspectivesState (publicRepository)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.RunMonadPerspectivesTransaction (runSterileTransaction)
import Perspectives.SetupCouchdb (setContextView, setPendingInvitationView, setRoleFromContextView, setRoleView)
import Prelude (Unit, bind, void, ($), discard, (<>), (>>=))

modelDirectory :: String
modelDirectory = "./src/model"

-- | Set up by adding model:System and dependencies to the users' models database. This will add the model instances, too.
-- | This function also ensures CURRENTUSER.
-- | Note that the repository should have model:Couchdb, model:Serialise and model:System.
setupUser :: MonadPerspectives Unit
setupUser = do
  -- First, upload model:System to perspect_models.
  repo <- publicRepository
  void $ runSterileTransaction (addModelToLocalStore [repo <> "model:System"] (RoleInstance ""))
  entitiesDatabaseName >>= setRoleView
  entitiesDatabaseName >>= setRoleFromContextView
  entitiesDatabaseName >>= setPendingInvitationView
  entitiesDatabaseName >>= setContextView
