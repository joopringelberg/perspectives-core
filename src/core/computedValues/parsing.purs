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
import Control.Monad.Writer (runWriterT)
import Data.Array (cons, head, intercalate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Main.RecompileBasicModels (recompileModelsAtUrl)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, type (~~>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (uploadToRepository) as CDB
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (modelUri2ModelRepository)
import Perspectives.LoadCRL (loadAndCacheCrlFile')
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.PerspectivesState (getWarnings, resetWarnings)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runEmbeddedTransaction)
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile_)
import Unsafe.Coerce (unsafeCoerce)

-- | Read the .arc file, parse it and try to compile it.
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

-- | Read the .crl file, parse it and try to compile it.
parseAndCompileCrl :: Array CrlSource -> (ContextInstance ~~> Value)
parseAndCompileCrl crlSource_ _ = case head crlSource_ of
  Nothing -> pure $ Value "No crl source given!"
  Just crlSource -> catchError
    do
      r <- lift $ lift $ loadAndCacheCrlFile' crlSource
      case r of
        Left errs -> ArrayT $ pure (Value <<< show <$> errs)
        Right _ -> pure $ Value "OK"
    \e -> ArrayT $ pure [(Value $ show e)]

type ArcSource = String
type CrlSource = String
type Url = String

-- | Parse and compile the Arc and Crl file. Upload to the repository.
-- | If the files are not valid, nothing happens.
-- | host_ and port_ are currently ignored.
uploadToRepository ::
  Array ArcSource ->
  Array CrlSource ->
  Array Url ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
uploadToRepository arcSource_ crlSource_ url_ _ = case head arcSource_, head crlSource_, head url_ of
  Just arcSource, Just crlSource, Just url -> do
    r <- loadAndCompileArcFile_ arcSource
    case r of
      Left m -> logPerspectivesError $ Custom ("uploadToRepository: " <> show m)
      Right (DomeinFile {_id, namespace}) -> lift $ void $ runWriterT $ runArrayT $ CDB.uploadToRepository (DomeinFileId _id) (unsafePartial $ modelUri2ModelRepository namespace)
  _, _, _ -> logPerspectivesError $ Custom ("uploadToRepository lacks arguments")

-- | Parse and compile the Arc file. Upload to the repository. Puts it in the Cache, but not in the local collection of DomeinFiles.
-- | If the file is not valid, nothing happens.
uploadToRepository2 ::
  Array ArcSource ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
uploadToRepository2 arcSource_ _ = case head arcSource_ of
  Just arcSource -> do
    r <- loadAndCompileArcFile_ arcSource
    case r of
      Left m -> logPerspectivesError $ Custom ("uploadToRepository: " <> show m)
      -- PROBABLY: STORE IN CACHE. model identification?
      Right df@(DomeinFile {_id, namespace}) -> do
        lift $ void $ storeDomeinFileInCache _id df
        lift $ void $ runWriterT $ runArrayT $ CDB.uploadToRepository (DomeinFileId _id) (unsafePartial $ modelUri2ModelRepository namespace)
  _ -> logPerspectivesError $ Custom ("uploadToRepository2 lacks arguments")

-- | Parse and compile all models found at the URL.
compileRepositoryModels ::
  Array Url ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
compileRepositoryModels url_ _ = case head url_ of
  Just url -> recompileModelsAtUrl url
  _ -> logPerspectivesError $ Custom ("compileRepositoryModels lacks arguments")

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Parsing$ParseAndCompileArc" {func: unsafeCoerce parseAndCompileArc, nArgs: 1}
  , Tuple "model://perspectives.domains#Parsing$ParseAndCompileCrl" {func: unsafeCoerce parseAndCompileCrl, nArgs: 1}
  , Tuple "model://perspectives.domains#Parsing$UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 3}
  , Tuple "model://perspectives.domains#Parsing$UploadToRepository2" {func: unsafeCoerce uploadToRepository2, nArgs: 1}
  , Tuple "model://perspectives.domains#Parsing$CompileRepositoryModels" {func: unsafeCoerce compileRepositoryModels, nArgs: 1}
]
