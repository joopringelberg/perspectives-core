  -- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

-- | This module defines External Core functions for model:Couchdb.

module Perspectives.Extern.Parsing where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (runWriterT)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (MonadPerspectivesTransaction, type (~~>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileId(..))
import Perspectives.Extern.Couchdb (uploadToRepository) as CDB
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.LoadCRL (loadAndCacheCrlFile')
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile_, loadArcAndCrl')
import Unsafe.Coerce (unsafeCoerce)

-- | Read the .arc file, parse it and try to compile it.
parseAndCompileArc :: Array ArcPath -> (ContextInstance ~~> Value)
parseAndCompileArc arcPath_ _ = case head arcPath_ of
  Nothing -> pure $ Value "No path to arc file given!"
  Just arcPath -> do
    r <- lift $ lift $ loadAndCompileArcFile_ arcPath
    case r of
      Left errs -> ArrayT $ pure (Value <<< show <$> errs)
      Right _ -> pure $ Value "OK"

-- | Read the .crl file, parse it and try to compile it.
parseAndCompileCrl :: Array CrlPath -> (ContextInstance ~~> Value)
parseAndCompileCrl crlPath_ _ = case head crlPath_ of
  Nothing -> pure $ Value "No path to crl file given!"
  Just crlPath -> do
    r <- lift $ lift $ loadAndCacheCrlFile' crlPath
    case r of
      Left errs -> ArrayT $ pure (Value <<< show <$> errs)
      Right _ -> pure $ Value "OK"

type ArcPath = String
type CrlPath = String
type Url = String

-- | Parse and compile the Arc and Crl file. Upload to the repository.
-- | If the files are not valid, nothing happens.
-- | host_ and port_ are currently ignored.
uploadToRepository ::
  Array ArcPath ->
  Array CrlPath ->
  Array Url ->
  Array RoleInstance -> MonadPerspectivesTransaction Unit
uploadToRepository arcPath_ crlPath_ url_ _ = case head arcPath_, head crlPath_, head url_ of
  Just arcPath, Just crlPath, Just url -> do
    r <- lift $ lift $ loadArcAndCrl' arcPath crlPath
    case r of
      Left m -> pure unit
      Right df@(DomeinFile drf@{_id}) -> do
        lift $ lift $ void $ storeDomeinFileInCache _id df
        -- construct the url from host and port.
        lift $ lift $ void $ runWriterT $ runArrayT $ CDB.uploadToRepository (DomeinFileId _id) url
  _, _, _ -> pure unit

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model:Parsing$ParseAndCompileArc" {func: unsafeCoerce parseAndCompileArc, nArgs: 1}
  , Tuple "model:Parsing$ParseAndCompileCrl" {func: unsafeCoerce parseAndCompileCrl, nArgs: 1}
  , Tuple "model:Parsing$UploadToRepository" {func: unsafeCoerce uploadToRepository, nArgs: 3}
]
