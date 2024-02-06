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

-- | This module defines External Core functions for model://perspectives.domains#Couchdb.

module Perspectives.Extern.Parsing where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, head, intercalate)
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Foreign.Object (Object, empty)
import Main.RecompileBasicModels (recompileModelsAtUrl)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectivesTransaction, MonadPerspectives)
import Perspectives.Couchdb (DeleteCouchdbDocument(..))
import Perspectives.Couchdb.Revision (Revision_, changeRevision)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (ModelUri, isModelUri, modelUri2ModelUrl)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (addAttachment) as Persistence
import Perspectives.Persistence.API (addDocument, deleteDocument, getAttachment, retrieveDocumentVersion, tryGetDocument_)
import Perspectives.PerspectivesState (getWarnings, resetWarnings)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runEmbeddedTransaction)
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile_)
import Unsafe.Coerce (unsafeCoerce)

-- | Read the .arc file, parse it and try to compile it. Does neither cache nor store.
-- | However, will load, cache and store dependencies of the model.
parseAndCompileArc :: Array ArcSource -> (ContextInstance ~~> Value)
parseAndCompileArc arcSource_ _ = case head arcSource_ of
  Nothing -> pure $ Value "No arc source given!"
  Just arcSource -> catchError
    do
      lift $ lift $ resetWarnings
      r <- lift $ lift $ runEmbeddedTransaction (ENR $ EnumeratedRoleType sysUser) (loadAndCompileArcFile_ arcSource)
      case r of
        Left errs -> ArrayT $ pure (Value <<< show <$> errs)
        -- Als er meldingen zijn, geef die dan terug.
        Right _ -> do
          warnings <- lift $ lift $ getWarnings
          pure $ Value $ intercalate "\n" (cons "OK" warnings)
    \e -> ArrayT $ pure [Value (show e)]

type ArcSource = String
type CrlSource = String
type Url = String

-- | Parse and compile the Arc file. Upload to the repository. Does neither cache, nor stores it in the local collection of DomeinFiles.
-- | If the file is not valid, nothing happens.
uploadToRepository ::
  Array String -> 
  Array ArcSource ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
uploadToRepository domeinFileName_ arcSource_ _ = case head domeinFileName_, head arcSource_ of
  Just domeinFileName, Just arcSource -> do
    r <- loadAndCompileArcFile_ arcSource
    case r of
      Left m -> logPerspectivesError $ Custom ("uploadToRepository: " <> show m)
      Right df@(DomeinFile {id, namespace}) -> do
        -- lift $ void $ storeDomeinFileInCache id df
        -- lift $ void $ CDB.uploadToRepository (DomeinFileId id)
        lift $ void $ uploadToRepository_ (unsafePartial modelUri2ModelUrl domeinFileName) df
  _, _ -> logPerspectivesError $ Custom ("uploadToRepository lacks arguments")

type URL = String

-- | As uploadToRepository, but provide the DomeinFile as argument.
uploadToRepository_ :: {repositoryUrl :: String, documentName :: String} -> DomeinFile -> MonadPerspectives Unit
uploadToRepository_ splitName (DomeinFile df) = do 
  -- Get the attachment info
  (mremoteDf :: Maybe DomeinFile) <- tryGetDocument_ splitName.repositoryUrl splitName.documentName
  attachments <- case mremoteDf of
    Nothing -> pure empty
    Just (DomeinFile {_attachments}) -> case _attachments of
      Nothing -> pure empty
      Just atts ->  traverseWithIndex
        (\attName {content_type} -> Tuple (MediaType content_type) <$> getAttachment splitName.repositoryUrl splitName.documentName attName)
        atts
  -- Get the revision (if any) from the remote database, so we can overwrite.
  (mVersion :: Maybe String) <- retrieveDocumentVersion splitName.repositoryUrl splitName.documentName
  -- The _id of df will be a versionless identifier. If we don't set it to the versioned name, the document
  -- will be stored under the versionless name.
  (newRev :: Revision_) <- addDocument splitName.repositoryUrl (changeRevision mVersion (DomeinFile df {_id = splitName.documentName})) splitName.documentName
  -- Now add the attachments.
  void $ execStateT (go splitName.repositoryUrl splitName.documentName attachments) newRev

  where
    -- As each attachment that we add will bump the document version, we have to catch it and use it on the
    -- next attachment.
    go :: URL -> String -> Object (Tuple MediaType (Maybe Foreign)) -> StateT Revision_ MonadPerspectives Unit
    go documentUrl documentName attachments = forWithIndex_ attachments \attName (Tuple mimetype mattachment) -> case mattachment of
      Nothing -> pure unit
      Just attachment -> do
        newRev <- get
        DeleteCouchdbDocument {rev} <- lift $ Persistence.addAttachment documentUrl documentName newRev attName attachment mimetype
        put rev

removeFromRepository :: 
  Array ModelUri ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
removeFromRepository modelUris _ = case head modelUris of
  Just modelUri -> if isModelUri modelUri
    then void $ lift $ removeFromRepository_ (unsafePartial modelUri2ModelUrl modelUri)
    else logPerspectivesError $ DomeinFileErrorBoundary "uploadToRepository" ("This modelURI is not well-formed: " <> modelUri)
  _ -> logPerspectivesError $ Custom ("removeFromRepository lacks the ModelURI argument.")

  where
  removeFromRepository_ :: {repositoryUrl :: String, documentName :: String} -> MonadPerspectives Boolean
  removeFromRepository_ splitName = deleteDocument splitName.repositoryUrl splitName.documentName Nothing

-- | Parse and compile all models found at the URL, e.g. https://perspectives.domains/models_perspectives_domains
compileRepositoryModels ::
  Array Url ->
  Array Url -> 
  Array RoleInstance -> MonadPerspectivesTransaction Unit
compileRepositoryModels modelsurl_ manifestsurl_ _ = case head modelsurl_, head manifestsurl_ of
  Just modelsurl, Just manifestsurl -> recompileModelsAtUrl modelsurl manifestsurl
  _, _ -> logPerspectivesError $ Custom ("compileRepositoryModels lacks arguments") 

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Parsing$ParseAndCompileArc" {func: unsafeCoerce parseAndCompileArc, nArgs: 1, isFunctional: True}
  , Tuple "model://perspectives.domains#Parsing$UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 2, isFunctional: True}
  , Tuple "model://perspectives.domains#Parsing$RemoveFromRepository" {func: unsafeCoerce removeFromRepository, nArgs: 1, isFunctional: True}
  , Tuple "model://perspectives.domains#Parsing$CompileRepositoryModels" {func: unsafeCoerce compileRepositoryModels, nArgs: 2, isFunctional: True}
]
