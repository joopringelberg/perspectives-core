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

import Control.Monad.AvarMonadAsk (modify)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Extern.Couchdb (addModelToLocalStore', createInitialInstances)
import Perspectives.ModelDependencies (bodiesWithAccountsModelName, couchdbManagementModelName, sysUser, systemModelName)
import Perspectives.Persistent (entitiesDatabaseName, getDomeinFile, invertedQueryDatabaseName, modelDatabaseName)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.SetupCouchdb (setContext2RoleView, setContextSpecialisationsView, setContextView, setCredentialsView, setFilled2FillerView, setFiller2FilledView, setModelView, setPendingInvitationView, setRTContextKeyView, setRTFilledKeyView, setRTFillerKeyView, setRTPropertyKeyView, setRTRoleKeyView, setRole2ContextView, setRoleFromContextView, setRoleSpecialisationsView, setRoleView)
import Perspectives.Sync.Transaction (Transaction(..), UninterpretedTransactionForPeer)
import Prelude (Unit, bind, discard, void, ($), (>>=))

modelDirectory :: String
modelDirectory = "./src/model" 

-- | Set up by adding model:System and dependencies to the users' models database. This will add the model instances, too.
-- | This function also ensures CURRENTUSER.
-- | Note that the repository should have model://perspectives.domains#Couchdb, model://perspectives.domains#Serialise and model://perspectives.domains#System.
setupUser :: Maybe UninterpretedTransactionForPeer -> MonadPerspectives Unit
setupUser uninterpretedIdentityDoc = do 
  entitiesDatabaseName >>= setRoleView 
  entitiesDatabaseName >>= setRoleFromContextView 
  -- OBSOLETE. Remove if testing shows the current definitioin of pendingInvitations works.
  entitiesDatabaseName >>= setPendingInvitationView
  entitiesDatabaseName >>= setContextView
  entitiesDatabaseName >>= setCredentialsView
  entitiesDatabaseName >>= setFiller2FilledView
  entitiesDatabaseName >>= setFilled2FillerView
  entitiesDatabaseName >>= setContext2RoleView
  entitiesDatabaseName >>= setRole2ContextView

  setupInvertedQueryDatabase

  modelDatabaseName >>= setRoleSpecialisationsView
  modelDatabaseName >>= setContextSpecialisationsView 
  -- Finally, upload model:System to perspect_models.
  void $ runMonadPerspectivesTransaction 
    (ENR $ EnumeratedRoleType sysUser) 
    (do 
      modify (over Transaction \t -> t { identityDocument = uninterpretedIdentityDoc })
      addModelToLocalStore' (DomeinFileId systemModelName)
    )

setupInvertedQueryDatabase :: MonadPerspectives Unit
setupInvertedQueryDatabase = do
  invertedQueryDatabaseName >>= setModelView
  invertedQueryDatabaseName >>= setRTPropertyKeyView
  invertedQueryDatabaseName >>= setRTRoleKeyView
  invertedQueryDatabaseName >>= setRTContextKeyView
  invertedQueryDatabaseName >>= setRTFillerKeyView
  invertedQueryDatabaseName >>= setRTFilledKeyView  

reSetupUser :: MonadPerspectives Unit
reSetupUser = do
  entitiesDatabaseName >>= setRoleView 
  entitiesDatabaseName >>= setRoleFromContextView 
  -- OBSOLETE. Remove if testing shows the current definitioin of pendingInvitations works.
  entitiesDatabaseName >>= setPendingInvitationView
  entitiesDatabaseName >>= setContextView
  entitiesDatabaseName >>= setCredentialsView
  entitiesDatabaseName >>= setFiller2FilledView
  entitiesDatabaseName >>= setFilled2FillerView
  entitiesDatabaseName >>= setContext2RoleView
  entitiesDatabaseName >>= setRole2ContextView
  DomeinFile {referredModels} <- getDomeinFile (DomeinFileId systemModelName)
  -- INSTALLATION / MODEL DEPENDENCY HERE: we assume these models will have been installed.
  void $ runMonadPerspectivesTransaction (ENR $ EnumeratedRoleType sysUser)
    do
      createInitialInstances systemModelName systemModelName "0" "0" Nothing
      createInitialInstances bodiesWithAccountsModelName bodiesWithAccountsModelName "0" "0" Nothing
      createInitialInstances couchdbManagementModelName couchdbManagementModelName "0" "0" Nothing
