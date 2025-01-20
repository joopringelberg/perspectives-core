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

import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, head, intercalate)
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object, empty)
import Main.RecompileBasicModels (recompileModelsAtUrl)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.Couchdb (DeleteCouchdbDocument(..), DocWithAttachmentInfo(..))
import Perspectives.Couchdb.Revision (Revision_, changeRevision)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleExternalFunctionError, handleExternalStatementError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Files (getPFileTextValue)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (DomeinFileName, ModelUri, isModelUri, modelUri2ModelUrl, unversionedModelUri)
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.ModelTranslation (ModelTranslation, augmentModelTranslation, generateFirstTranslation, parseTranslation, writeTranslationYaml, generateTranslationTable) as MT
import Perspectives.ModelTranslation (ModelTranslation, emptyTranslationTable)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (addAttachment, addDocument, deleteDocument, fromBlob, getAttachment, getDocument, retrieveDocumentVersion, toFile, tryGetDocument_)
import Perspectives.PerspectivesState (getWarnings, resetWarnings)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runEmbeddedTransaction)
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile_)
import Simple.JSON (readJSON, readJSON_, writeJSON)
import Unsafe.Coerce (unsafeCoerce)

-- | Read the .arc file, parse it and try to compile it. Does neither cache nor store.
-- | However, will load, cache and store dependencies of the model.
-- | The DomeinFileName should be unversioned.
parseAndCompileArc :: Array DomeinFileName -> Array ArcSource -> (ContextInstance ~~> Value)
parseAndCompileArc domeinFileName_ arcSource_ _ = try
  (case head domeinFileName_, head arcSource_ of
    Nothing, _ -> pure $ Value "No model name given!"
    _, Nothing -> pure $ Value "No arc source given!"
    Just domeinFileName, Just arcSource -> catchError
      do
        lift $ lift $ resetWarnings
        r <- lift $ lift $ runEmbeddedTransaction true (ENR $ EnumeratedRoleType sysUser) (loadAndCompileArcFile_ (DomeinFileId domeinFileName) arcSource)
        case r of
          Left errs -> ArrayT $ pure (Value <<< show <$> errs) 
          -- Als er meldingen zijn, geef die dan terug.
          Right _ -> do
            warnings <- lift $ lift $ getWarnings
            pure $ Value $ intercalate "\n" (cons "OK" warnings)
      \e -> ArrayT $ pure [Value (show e)])
  >>= handleExternalFunctionError "model://perspectives.domains#Parsing$ParseAndCompileArc"

type ArcSource = String
type CrlSource = String
type Url = String

-- | Parse and compile the Arc file. Upload to the repository. Does neither cache, nor stores it in the local collection of DomeinFiles.
-- | If the file is not valid, nothing happens.
-- | The DomeinFileName should be versioned (e.g. model://perspectives.domains#System@1.0).
uploadToRepository ::
  Array DomeinFileName -> 
  Array ArcSource ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
uploadToRepository domeinFileName_ arcSource_ _ = try
  (case head domeinFileName_, head arcSource_ of
    Just domeinFileName, Just arcSource -> do
      r <- loadAndCompileArcFile_ (DomeinFileId $ unversionedModelUri domeinFileName) arcSource
      case r of
        Left m -> logPerspectivesError $ Custom ("uploadToRepository: " <> show m)
        -- Here we will have a tuple of the DomeinFile and an instance of StoredQueries.
        Right (Tuple df@(DomeinFile {id, namespace}) invertedQueries) -> do
          -- lift $ void $ storeDomeinFileInCache id df
          -- lift $ void $ CDB.uploadToRepository (DomeinFileId id)
          lift $ void $ uploadToRepository_ (unsafePartial modelUri2ModelUrl domeinFileName) df invertedQueries
    _, _ -> logPerspectivesError $ Custom ("uploadToRepository lacks arguments"))
  >>= handleExternalStatementError "model://perspectives.domains#Parsing$UploadToRepository"

type URL = String

-- | As uploadToRepository, but provide the DomeinFile as argument.
-- | Adds an empty TranslationTable if the DomeinFile did not yet exist in the Repository.
uploadToRepository_ :: {repositoryUrl :: String, documentName :: String} -> DomeinFile -> StoredQueries -> MonadPerspectives Unit
uploadToRepository_ splitName (DomeinFile df) invertedQueries = do 
  -- Get the attachment info
  (mremoteDf :: Maybe DocWithAttachmentInfo) <- tryGetDocument_ splitName.repositoryUrl splitName.documentName
  attachments <- case mremoteDf of
    Nothing -> pure empty
    Just (DocWithAttachmentInfo {_attachments}) -> case _attachments of
      Nothing -> pure empty
      Just atts ->  traverseWithIndex
        (\attName {content_type} -> Tuple (MediaType content_type) <$> getAttachment splitName.repositoryUrl splitName.documentName attName)
        atts
  -- Get the revision (if any) from the remote database, so we can overwrite.
  (mVersion :: Maybe String) <- retrieveDocumentVersion splitName.repositoryUrl splitName.documentName
  -- The _id of df will be a versionless identifier. If we don't set it to the versioned name, the document
  -- will be stored under the versionless name.
  (newRev :: Revision_) <- addDocument splitName.repositoryUrl (changeRevision mVersion (DomeinFile df {_id = splitName.documentName})) splitName.documentName
  case mremoteDf of 
    Nothing -> do 
      -- Add an empty translations file.
      theFile <- liftEffect $ unsafeCoerce toFile "translationtable.json" "application/json" (unsafeToForeign $ writeJSON emptyTranslationTable)
      DeleteCouchdbDocument {rev} <- addAttachment splitName.repositoryUrl splitName.documentName newRev "translationtable.json" theFile (MediaType "application/json")
      void $ execStateT (go splitName.repositoryUrl splitName.documentName attachments) rev
    -- Otherwise add the attachments.
    Just _ -> void $ execStateT (go splitName.repositoryUrl splitName.documentName attachments) newRev

  where
    -- As each attachment that we add will bump the document version, we have to catch it and use it on the
    -- next attachment.
    go :: URL -> String -> Object (Tuple MediaType (Maybe Foreign)) -> StateT Revision_ MonadPerspectives Unit
    go documentUrl documentName attachments = do 
      forWithIndex_ 
        attachments 
        (\attName (Tuple mimetype mattachment) -> do 
          case mattachment of
            Nothing -> pure unit
            Just attachment -> do
              newRev <- get
              DeleteCouchdbDocument {rev} <- lift $ addAttachment documentUrl documentName newRev attName attachment mimetype
              put rev)
      -- Lastly, add the StoredQueries
      (newRev :: Revision_) <- get
      theFile <- liftEffect $ toFile "storedQueries.json" "application/json" (unsafeToForeign $ writeJSON invertedQueries)
      lift $ void $ addAttachment documentUrl documentName newRev "storedQueries.json" theFile (MediaType "application/json")

removeFromRepository :: 
  Array ModelUri ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
removeFromRepository modelUris _ = try 
  (case head modelUris of
    Just modelUri -> if isModelUri modelUri
      then void $ lift $ removeFromRepository_ (unsafePartial modelUri2ModelUrl modelUri)
      else logPerspectivesError $ DomeinFileErrorBoundary "uploadToRepository" ("This modelURI is not well-formed: " <> modelUri)
    _ -> logPerspectivesError $ Custom ("removeFromRepository lacks the ModelURI argument."))
  >>= handleExternalStatementError "model://perspectives.domains#Parsing$RemoveFromRepository"

  where
  removeFromRepository_ :: {repositoryUrl :: String, documentName :: String} -> MonadPerspectives Boolean
  removeFromRepository_ splitName = deleteDocument splitName.repositoryUrl splitName.documentName Nothing

-- | Parse and compile all models found at the URL, e.g. https://perspectives.domains/models_perspectives_domains
compileRepositoryModels ::
  Array Url ->
  Array Url -> 
  Array RoleInstance -> MonadPerspectivesTransaction Unit
compileRepositoryModels modelsurl_ manifestsurl_ _ = try
  (case head modelsurl_, head manifestsurl_ of
    Just modelsurl, Just manifestsurl -> recompileModelsAtUrl modelsurl manifestsurl
    _, _ -> logPerspectivesError $ Custom ("compileRepositoryModels lacks arguments"))
  >>= handleExternalStatementError "model://perspectives.domains#Parsing$CompileRepositoryModels"

-------------------------------------------------------------------------------
---- MODEL TRANSLATION
-------------------------------------------------------------------------------
-- | From the DomeinFile indicated by the namespace, generate ModelTranslation and serialise it to a PString.
-- | The DomeinFileName should be versioned (e.g. model://perspectives.domains#System@1.0).
generateFirstTranslation :: Array DomeinFileName -> (RoleInstance ~~> Value)
generateFirstTranslation modelURI_ _ = case head modelURI_ of
  Nothing -> handleExternalFunctionError "model://perspectives.domains#Parsing$GenerateFirstTranslation"
        (Left (error "Model URI should be provided."))
  Just dfName -> do 
    case (unsafePartial modelUri2ModelUrl dfName) of
      {repositoryUrl, documentName} -> do 
        x <- try $ lift $ lift $ getDocument repositoryUrl documentName
        -- x <- try $ lift $ lift $ getDomeinFile (DomeinFileId dfName)
        case x of 
          Left e -> handleExternalFunctionError "model://perspectives.domains#Parsing$GenerateFirstTranslation"
            (Left e)
          Right df -> pure $ Value $ writeJSON $ MT.generateFirstTranslation df

-- | From a serialised ModelTranslation, generate YAML (a PString)
getTranslationYaml :: Array String -> (RoleInstance ~~> Value)
getTranslationYaml modelTranslation_ _ = case head modelTranslation_ of 
  Nothing -> handleExternalFunctionError "model://perspectives.domains#Parsing$GetTranslationYaml"
    (Left (error "A ModelTranslation should be provided."))
  Just modelTranslation -> case readJSON modelTranslation of 
    Left e -> handleExternalFunctionError "model://perspectives.domains#Parsing$GetTranslationYaml"
      (Left $ error (show e))
    Right (translation :: MT.ModelTranslation) -> pure $ Value $ MT.writeTranslationYaml translation

-- | From a YAML string, generate a (serialised) ModelTranslation.
parseYamlTranslation :: Array String -> (RoleInstance ~~> Value)
parseYamlTranslation pfile_ _ = ArrayT case head pfile_ of 
  Nothing -> pure []
  Just pfile -> do 
    mYaml <- lift $ getPFileTextValue pfile
    case mYaml of 
      Nothing -> pure $ []
      Just yaml -> do
        translation' <- liftEffect $ MT.parseTranslation yaml
        case translation' of 
          Left e -> throwError e
          Right translation -> pure $ [Value $ writeJSON translation]

-- | From a PString that holds a ModelTranslation, generate the table and upload to the repository.
-- | The DomeinFileName should be versioned (e.g. model://perspectives.domains#System@1.0).
generateTranslationTable :: Array String -> Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
generateTranslationTable translation_ domeinFileName_ _ = case head translation_, head domeinFileName_ of 
  Nothing, _ -> handleExternalStatementError "model://perspectives.domains#Parsing$GenerateTranslationTable"
    (Left (error "A String holding a ModelTranslation should be provided."))
  _, Nothing -> handleExternalStatementError "model://perspectives.domains#Parsing$GenerateTranslationTable"
    (Left (error "A Versioned model name should be provided. (e.g. model://perspectives.domains#System@1.0)"))
  Just modelTranslationString, Just domeinFileName -> case readJSON modelTranslationString of 
    -- Fail silently
    Left e -> log ("generateTranslationTable: " <> show e)
    -- Generate the table and upload as attachment to the repository DomeinFile.
    Right (modelTranslation :: ModelTranslation) -> case (unsafePartial modelUri2ModelUrl domeinFileName) of
      {repositoryUrl, documentName} -> do 
        table <- pure (MT.generateTranslationTable modelTranslation)
        theFile <- liftEffect $ toFile "translationtable.json" "application/json" (unsafeToForeign $ writeJSON table)
        mRev <- lift $ retrieveDocumentVersion repositoryUrl documentName
        -- Ignore the new revision. We do not have a local representation of the remote DomeinFile.
        void $ lift $ addAttachment 
          repositoryUrl 
          documentName 
          mRev
          "translationtable.json"
          theFile
          (MediaType "text/json")

-- | Fill a ModelTranslation freshly generated from a DomeinFile, with translations taken from a TranslationTable.
-- | NOTE: the TranslationTable must be available on the versioned model in the repository. It is not a property value.
-- | The ModelTranslation must be passed in as a string.
-- | The result is the (serialised) ModelTranslation.
-- | The original ModelTranslation is returned when there is no TranslationTable or when it cannot be parsed correctly.
augmentModelTranslation :: Array String -> Array String -> (RoleInstance ~~> Value)
augmentModelTranslation translation_ domeinFileName_ _ = case head translation_, head domeinFileName_ of 
  Nothing, _ -> handleExternalFunctionError "model://perspectives.domains#Parsing$AugmentModelTranslation"
    (Left (error "A String holding a ModelTranslation should be provided."))
  _, Nothing -> handleExternalFunctionError "model://perspectives.domains#Parsing$AugmentModelTranslation"
    (Left (error "A Versioned model name should be provided. (e.g. model://perspectives.domains#System@1.0)"))
  Just modelTranslationString, Just domeinFileName -> case readJSON_ modelTranslationString of 
    -- Fail silently
    Nothing -> handleExternalFunctionError "model://perspectives.domains#Parsing$AugmentModelTranslation"
      (Left (error "The model translation string could not be parsed."))
    -- Retrieve the Existing Translation from the repository and apply it to the ModelTranslation.
    Just (modelTranslation :: ModelTranslation) -> case (unsafePartial modelUri2ModelUrl domeinFileName) of
      {repositoryUrl, documentName} -> do 
        mTranslationTableBlob <- lift $ lift $ getAttachment repositoryUrl documentName "translationtable.json"
        case mTranslationTableBlob of 
          Nothing -> pure $ Value modelTranslationString
          Just translationTableBlob -> do 
            translationTableString <- liftAff $ fromBlob translationTableBlob
            case readJSON translationTableString of 
              Left e -> pure $ Value modelTranslationString
              Right translationTable -> pure $ Value $ writeJSON (MT.augmentModelTranslation translationTable modelTranslation)


-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Parsing$ParseAndCompileArc" {func: unsafeCoerce parseAndCompileArc, nArgs: 2, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Parsing$UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Parsing$RemoveFromRepository" {func: unsafeCoerce removeFromRepository, nArgs: 1, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Parsing$CompileRepositoryModels" {func: unsafeCoerce compileRepositoryModels, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Parsing$GenerateFirstTranslation" {func: unsafeCoerce generateFirstTranslation, nArgs: 1, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Parsing$GetTranslationYaml" {func: unsafeCoerce getTranslationYaml, nArgs: 1, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Parsing$ParseYamlTranslation" {func: unsafeCoerce parseYamlTranslation, nArgs: 1, isFunctional: True, isEffect: false}
  , Tuple "model://perspectives.domains#Parsing$GenerateTranslationTable" {func: unsafeCoerce generateTranslationTable, nArgs: 2, isFunctional: True, isEffect: true}
  , Tuple "model://perspectives.domains#Parsing$AugmentModelTranslation" {func: unsafeCoerce augmentModelTranslation, nArgs: 2, isFunctional: True, isEffect: false}
]
