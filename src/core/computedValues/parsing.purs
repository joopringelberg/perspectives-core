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
import Control.Monad.Trans.Class (lift) 
import Data.Array (cons, head, intercalate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Main.RecompileBasicModels (recompileModelsAtUrl)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectivesTransaction)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (removeFromRepository_)
import Perspectives.Extern.Couchdb (uploadToRepository_) as CDB
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (ModelUri, isModelUri, modelUri2ModelUrl)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
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
      Right df@(DomeinFile {_id, namespace}) -> do
        -- lift $ void $ storeDomeinFileInCache _id df
        -- lift $ void $ CDB.uploadToRepository (DomeinFileId _id)
        lift $ void $ CDB.uploadToRepository_ (unsafePartial modelUri2ModelUrl domeinFileName) df
  _, _ -> logPerspectivesError $ Custom ("uploadToRepository lacks arguments")

removeFromRepository :: 
  Array ModelUri ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
removeFromRepository modelUris _ = case head modelUris of
  Just modelUri -> if isModelUri modelUri
    then void $ lift $ removeFromRepository_ (unsafePartial modelUri2ModelUrl modelUri)
    else logPerspectivesError $ DomeinFileErrorBoundary "uploadToRepository" ("This modelURI is not well-formed: " <> modelUri)
  _ -> logPerspectivesError $ Custom ("removeFromRepository lacks the ModelURI argument.")


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
