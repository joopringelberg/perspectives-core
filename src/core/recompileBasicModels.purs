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

import Control.Monad.Trans.Class (lift)
import Data.Array (elemIndex, filterA, head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Error, runAff, throwError, error)
import Effect.Aff.AVar (new)
import Foreign (Foreign)
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>))
import Perspectives.Couchdb.Revision (changeRevision)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (contextInstancesFromCouchdb, modelsDatabaseName, roleInstancesFromCouchdb)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Instances.ObjectGetters (binding, context, getEnumeratedRoleInstances, getProperty, getPropertyFromTelescope)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (retrieveDocumentVersion)
import Perspectives.Persistence.State (getCouchdbBaseURL)
import Perspectives.Persistence.Types (Url, PouchdbUser, decodePouchdbUser')
import Perspectives.Persistent (saveEntiteit_)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.TypePersistence.LoadArc (loadArcAndCrl')

basicModels :: Array String
basicModels = ["model:System", "model:ModelManagement"]

-- | Recompiles a number of essential models. Use this function when the definition of the DomeinFile
-- | has changed. The local models directory of the user that is provided, will have the freshly compiled
-- | DomeinFiles, so this user can be booted. The essential models include model:ModelManagement, so that
-- | using this account, all models can be recompiled from the client.
-- | These functions depend heavily on the Perspectives types in model:System and
-- | model:ModelManagement.
recompileBasicModels :: Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
recompileBasicModels rawPouchdbUser publicRepo callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left e -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in resetAccount")
      Right (pouchdbUser :: PouchdbUser) -> do
        state <- new $ newPerspectivesState pouchdbUser publicRepo
        runPerspectivesWithState
          (do
            addAllExternalFunctions
            mmodelManagementApp <- (RoleInstance "") ##> contextInstancesFromCouchdb ["model:ModelManagement$ModelManagementApp"]
            case mmodelManagementApp of
              Nothing -> logPerspectivesError $ Custom $ "Cannot find instance of ModelManagementApp!"
              Just modelManagementApp -> do
                (managedModels :: Array RoleInstance) <- modelManagementApp ##= roleInstancesFromCouchdb ["model:ModelManagement$ModelManagementApp$Models"]
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
      logPerspectivesError $ Custom $ "Basic models recompiled!"
      callback true

    -- RoleInstance is of type ModelManagementApp$Models
    recompileModel :: RoleInstance -> MonadPerspectives Unit
    recompileModel rid = do
      arcSources <- rid ##= binding >=> getProperty (EnumeratedPropertyType "model:ModelManagement$ManagedModel$External$ArcSource")
      crlSources <- rid ##= binding >=> getProperty (EnumeratedPropertyType "model:ModelManagement$ManagedModel$External$CrlSource")
      mcouchbaseUrl <- getCouchdbBaseURL
      case head arcSources, head crlSources, mcouchbaseUrl of
        Nothing, _, _ -> logPerspectivesError $ Custom $ "No Arc Source found!"
        _, Nothing, _ -> logPerspectivesError $ Custom $ "No Crl Source found!"
        _, _, Nothing -> logPerspectivesError $ Custom $ "No CouchdbUrl found!"
        Just arcSource, Just crlSource, Just couchdbUrl -> void $ runMonadPerspectivesTransaction'
          false
          (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
          do
            r <- lift $ lift $ loadArcAndCrl' (unwrap arcSource) (unwrap crlSource)
            case r of
              Left m -> logPerspectivesError $ Custom ("recompileModel: " <> show m)
              Right df@(DomeinFile drf@{_id}) -> lift $ lift do
                modDb <- modelsDatabaseName
                mrev <- retrieveDocumentVersion modDb _id
                void $ saveEntiteit_ (DomeinFileId _id) (changeRevision mrev df)

    -- | RoleInstance is of type "model:ModelManagement$ModelManagementApp$Models"
    isBasicModel :: RoleInstance -> MonadPerspectives Boolean
    isBasicModel rid = (rid ##>
      binding >=>
      context >=>
      getEnumeratedRoleInstances (EnumeratedRoleType "model:ModelManagement$ManagedModel$ModelDescription") >=>
      getPropertyFromTelescope (EnumeratedPropertyType "model:System$Model$External$ModelIdentification")) >>= isBasicModel'
      where
        isBasicModel' :: Maybe Value -> MonadPerspectives Boolean
        isBasicModel' (Just (Value modelId)) = pure $ isJust $ elemIndex modelId basicModels
        isBasicModel' Nothing = (logPerspectivesError $ Custom $ "No ModelIdentification found for '" <> unwrap rid <> "'.") *> pure false
