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

module Main.RecompileBasicModels where

-- | Recompiles a number of essential models. Use this function when the definition of the DomeinFile
-- | has changed. The local models directory of the user that is provided, will have the freshly compiled
-- | DomeinFiles, so this user can be booted. The essential models include model:ModelManagement, so that
-- | using this account, all models can be recompiled from the client.
import Prelude

import Data.Array (elemIndex, filterA)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Error, runAff, throwError, error)
import Effect.Aff.AVar (new)
import Foreign (Foreign)
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>), (##>>))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (contextInstancesFromCouchdb, modelsDatabaseName, roleInstancesFromCouchdb)
import Perspectives.Extern.Parsing (uploadToRepository)
import Perspectives.Instances.ObjectGetters (binding, context, getEnumeratedRoleInstances, getProperty)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.State (getCouchdbBaseURL)
import Perspectives.Persistence.Types (Url, UserName, PouchdbUser, decodePouchdbUser')
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.RunPerspectives (runPerspectivesWithState)

basicModels :: Array String
basicModels = ["model:System", "model:ModelManagement"]

-- | Recompiles a number of essential models. Use this function when the definition of the DomeinFile
-- | has changed. The local models directory of the user that is provided, will have the freshly compiled
-- | DomeinFiles, so this user can be booted. The essential models include model:ModelManagement, so that
-- | using this account, all models can be recompiled from the client.
recompileBasicModels :: UserName -> Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
recompileBasicModels usr rawPouchdbUser publicRepo callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left e -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in resetAccount")
      Right (pouchdbUser :: PouchdbUser) -> do
        state <- new $ newPerspectivesState pouchdbUser publicRepo
        runPerspectivesWithState
          (do
            modelsDb <- modelsDatabaseName
            mmodelManagementApp <- (RoleInstance "") ##> contextInstancesFromCouchdb ["model:ModelManagement$Model$ModelManagementApp"]
            case mmodelManagementApp of
              Nothing -> logPerspectivesError $ Custom $ "Cannot find instance of ModelManagementApp!"
              Just modelManagementApp -> do
                (managedModels :: Array RoleInstance) <- modelManagementApp ##= roleInstancesFromCouchdb ["model:ModelManagement$Model$ModelManagementApp$Models"]
                modelsToCompile <- (filterA isBasicModel managedModels)
                for_ modelsToCompile recompileModel
            )
          state
  where
    handler :: Either Error Unit -> Effect Unit
    handler (Left e) = do
      logPerspectivesError $ Custom $ "An error condition in recompileBasicModels: " <> (show e)
      callback false
    handler (Right e) = do
      logPerspectivesError $ Custom $ "Basic models recompiled!" <> usr
      callback true

    -- RoleInstance is of type ModelManagementApp$Models
    recompileModel :: RoleInstance -> MonadPerspectives Unit
    recompileModel rid = do
      arcSource <- rid ##= binding >=> getProperty (EnumeratedPropertyType "model:ModelManagement$ManagedModel$External$ArcSource")
      crlSource <- rid ##= binding >=> getProperty (EnumeratedPropertyType "model:ModelManagement$ManagedModel$External$CrlSource")
      mcouchbaseUrl <- getCouchdbBaseURL
      case mcouchbaseUrl of
        Nothing -> logPerspectivesError $ Custom $ "No CouchdbUrl found!"
        Just couchdbUrl -> void $ runMonadPerspectivesTransaction'
          false
          (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
          (uploadToRepository (unwrap <$> arcSource) (unwrap <$> crlSource) [couchdbUrl] [(RoleInstance "")])

    isBasicModel :: RoleInstance -> MonadPerspectives Boolean
    isBasicModel rid = (rid ##>> context >=> getEnumeratedRoleInstances (EnumeratedRoleType "model:ModelManagement$ManagedModel$ModelDescription") >=> binding >=> getProperty (EnumeratedPropertyType "model:System$Model$ModelIdentification")) >>= isBasicModel'
      where
        isBasicModel' :: Value -> MonadPerspectives Boolean
        isBasicModel' (Value modelId) = pure $ isJust $ elemIndex modelId basicModels
