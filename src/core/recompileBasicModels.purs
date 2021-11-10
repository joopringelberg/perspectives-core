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
import Data.Array (catMaybes, elemIndex, head, sort)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Error, runAff, throwError, error)
import Effect.Aff.AVar (new)
import Effect.Class.Console (log)
import Foreign (Foreign)
import Foreign.Object (insert)
import Perspectives.CoreTypes (MonadPerspectives, (##=))
import Perspectives.Couchdb.Revision (changeRevision)
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (modelsDatabaseName)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.InstanceRepresentation (PerspectContext(..))
import Perspectives.Instances.Combinators (conjunction, exists', filter, not')
import Perspectives.Instances.ObjectGetters (binding, context, externalRole, getProperty, getRoleBinders, getUnlinkedRoleInstances)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (documentsInDatabase, includeDocs, retrieveDocumentVersion)
import Perspectives.Persistence.State (getCouchdbBaseURL)
import Perspectives.Persistence.Types (Url, PouchdbUser, decodePouchdbUser')
import Perspectives.Persistent (getPerspectContext, removeEntiteit, saveEntiteit_)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.TypePersistence.LoadArc (loadArcAndCrl')
import Simple.JSON (class ReadForeign, read, read')

modelDescription :: EnumeratedRoleType
modelDescription = EnumeratedRoleType "model:ModelManagement$ManagedModel$ModelDescription"

modelsInUse :: EnumeratedRoleType
modelsInUse = EnumeratedRoleType "model:System$PerspectivesSystem$ModelsInUse"

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
            modelsDb <- modelsDatabaseName
            {rows:allModels} <- documentsInDatabase modelsDb includeDocs
            uninterpretedDomeinFiles <- for allModels \({id, doc}) -> case read <$> doc of
              Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
              Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
              Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
            for_ (sort $ catMaybes uninterpretedDomeinFiles) recompileModel
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

    recompileModel :: UninterpretedDomeinFile -> MonadPerspectives Unit
    recompileModel (UninterpretedDomeinFile{_id, _rev, contents}) = void $ runMonadPerspectivesTransaction'
      false
      (ENR $ EnumeratedRoleType "model:System$PerspectivesSystem$User")
      do
        r <- lift $ lift $ loadArcAndCrl' contents.arc contents.crl
        case r of
          Left m -> logPerspectivesError $ Custom ("recompileModel: " <> show m)
          Right df@(DomeinFile drf) -> lift $ lift do
            log $  "Recompiled '" <> _id <> "' succesfully!"
            storeDomeinFileInCouchdbPreservingAttachments df

newtype UninterpretedDomeinFile = UninterpretedDomeinFile
  { _id :: String
  , _rev :: String
  , contents ::
    { referredModels :: Array String
    , crl :: String
    , arc :: String
    }
  }

instance readForeignUninterpretedDomeinFile :: ReadForeign UninterpretedDomeinFile where
  readImpl f = UninterpretedDomeinFile <$> (read' f)

instance eqUninterpretedDomeinFile :: Eq UninterpretedDomeinFile where
  eq (UninterpretedDomeinFile {_id:id1}) (UninterpretedDomeinFile {_id:id2}) = eq id1 id2

instance ordUninterpretedDomeinFile :: Ord UninterpretedDomeinFile where
  compare (UninterpretedDomeinFile {_id:id1, contents:c1}) (UninterpretedDomeinFile {_id:id2, contents:c2}) =
    if isJust $ elemIndex id1 c2.referredModels
      then LT
      else if isJust $ elemIndex id2 c1.referredModels
        then GT
        else EQ

recompileBasicModels' :: Foreign -> Url -> (Boolean -> Effect Unit) -> Effect Unit
recompileBasicModels' rawPouchdbUser publicRepo callback = void $ runAff handler
  do
    case decodePouchdbUser' rawPouchdbUser of
      Left e -> throwError (error "Wrong format for parameter 'rawPouchdbUser' in resetAccount")
      Right (pouchdbUser :: PouchdbUser) -> do
        state <- new $ newPerspectivesState pouchdbUser publicRepo
        runPerspectivesWithState
          (do
            addAllExternalFunctions
            mySystem <- ContextInstance <$> getMySystem
            modelsToKeep <- mySystem ##= filter
              (getUnlinkedRoleInstances modelsInUse)
              (exists' (conjunction (getRoleBinders modelDescription) (binding >=> (getRoleBinders modelDescription))))
            modelsToRemove <- mySystem ##= filter
              (getUnlinkedRoleInstances modelsInUse)
              (not' (exists' (conjunction (getRoleBinders modelDescription) (binding >=> (getRoleBinders modelDescription)))))
            PerspectContext r@{rolInContext} <- getPerspectContext mySystem
            void $ saveEntiteit_ mySystem (PerspectContext r {rolInContext = insert (unwrap modelsInUse) modelsToKeep rolInContext})
            for_ modelsToRemove removeEntiteit
            modelsToRecompile <- mySystem ##=
              ((getUnlinkedRoleInstances modelsInUse) >=>
              (conjunction (getRoleBinders modelDescription) (binding >=> (getRoleBinders modelDescription))) >=>
              context >=>
              externalRole
              )
            for_ modelsToRecompile recompileModel
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

    -- RoleInstance is of type model:ModelManagement$ManagedModel$External.
    recompileModel :: RoleInstance -> MonadPerspectives Unit
    recompileModel rid = do
      arcSources <- rid ##= getProperty (EnumeratedPropertyType "model:ModelManagement$ManagedModel$External$ArcSource")
      crlSources <- rid ##= getProperty (EnumeratedPropertyType "model:ModelManagement$ManagedModel$External$CrlSource")
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
