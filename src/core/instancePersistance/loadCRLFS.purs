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

module Perspectives.LoadCRL.FS where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Node.Process (cwd)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.Indexed (replaceIndexedNames)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (updateRevision)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.SaveUserData (saveContextInstance)

-- | Loads a file from a directory relative to the active process.
-- | Replaces all indexed names.
-- | All context and role instances are loaded into the cache.
-- | Takes care of the responsibility CURRENTUSER.
-- | NOTICE that if an entity was in Couchdb before and was not already in cache,
-- | it will now have revision Nothing in cache. Saving it will lead to conflict.
loadAndCacheCrlFile :: String ->
  String ->
  MonadPerspectives (Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol)))
loadAndCacheCrlFile file directoryName = do
  procesDir <- liftEffect cwd
  loadAndCacheCrlFile' (Path.concat [procesDir, directoryName, file])

type FilePath = String

loadAndCacheCrlFile' :: FilePath -> MonadPerspectives (Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol)))
loadAndCacheCrlFile' filePath = do
  text <- lift $ readTextFile UTF8 filePath
  parseResult <- replaceIndexedNames text >>= parseAndCache
  case parseResult of
    Left e -> pure $ Left [Custom (show e)]
    Right contextsAndRoles@(Tuple contextInstances roleInstances) -> do
      pure $ Right contextsAndRoles

loadAndCacheCrlFile_ :: String ->
  String ->
  MonadPerspectives (Array PerspectivesError)
loadAndCacheCrlFile_ file directoryName = do
  r <- loadAndCacheCrlFile file directoryName
  case r of
    Left e -> pure e
    Right _ -> pure []

-- | Loads a file from the given directory relative to the directory of the active process.
-- | Replaces all indexed names.
-- | Runs all bot actions.
-- | All instances are loaded into the cache, and stored in Couchdb.
-- | This function takes care of
-- | PERSISTENCE
-- | QUERY UPDATES
-- | RULE TRIGGERING
-- | CURRENTUSER
loadAndSaveCrlFile :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadAndSaveCrlFile file directoryName = do
  procesDir <- liftEffect cwd
  text <- lift $ readTextFile UTF8 (Path.concat [procesDir, directoryName, file])
  parseResult <- replaceIndexedNames text >>= parseAndCache
  case parseResult of
    Left e -> pure $ [Custom (show e)]
    Right contextsAndRoles@(Tuple contextInstances roleInstances) -> do
      -- Update the revision in cache by visiting Couchdb.
      forWithIndex_ contextInstances \i _ -> updateRevision (ContextInstance i)
      forWithIndex_ roleInstances \i _ -> updateRevision (RoleInstance i)
      void $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser) (forWithIndex_ contextInstances
        \i _ -> do
          saveContextInstance (ContextInstance i))
      pure []
