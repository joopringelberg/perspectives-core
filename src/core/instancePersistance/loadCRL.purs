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

module Perspectives.LoadCRL where

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
import Perspectives.Actions (setupAndRunBotActions)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances (saveEntiteit)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))

-- | Loads a file from a directory relative to the active process.
-- | All context and role instances are loaded into the cache.
-- | Runs all bot actions.
-- | Does not save to the database.
loadCrlFile :: String ->
  String ->
  MonadPerspectives (Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol)))
loadCrlFile file directoryName = do
  procesDir <- liftEffect cwd
  text <- lift $ readTextFile UTF8 (Path.concat [procesDir, directoryName, file])
  parseResult <- parseAndCache text
  case parseResult of
    Left e -> pure $ Left [Custom (show e)]
    Right contextsAndRoles@(Tuple contextInstances _) -> do
      forWithIndex_ contextInstances
        \i (_ :: PerspectContext) -> setupAndRunBotActions (ContextInstance i)
      pure $ Right contextsAndRoles

-- | Loads a file from the directory "src/model" relative to the directory of the active process.
-- | Runs all bot actions.
-- | All instances are loaded into the cache, and stored in Couchdb.
loadAndSaveCrlFile :: String -> String -> MonadPerspectives (Array PerspectivesError)
loadAndSaveCrlFile file directoryName = do
  r <- loadCrlFile file directoryName
  case r of
    Left e -> pure e
    Right (Tuple contextInstances roleInstances) -> do
      forWithIndex_ contextInstances
        \i (_ :: PerspectContext) -> saveEntiteit (ContextInstance i)
      forWithIndex_ roleInstances
        \i (_ :: PerspectRol) -> saveEntiteit (RoleInstance i)
      pure []
