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

import Effect.Console (log)
import Effect.Class (liftEffect)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Node.Process (cwd)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Checking.PerspectivesTypeChecker.Messages (UserMessage)
import Perspectives.SaveUserData (saveDomeinFileAsUserData)

modelDirectory :: String
modelDirectory = "./src/model"

type FileName = String

-- | Loads a file from the directory "src/model" relative to the directory of the
-- | active process.
-- | All definitions are loaded into the cache. The file is parsed and stored in Couchdb.
loadCRLFile :: FileName -> MonadPerspectives (Array UserMessage)
loadCRLFile file = do
  liftEffect $ log ("=========================Parse the file " <> file <> "===================")
  procesDir <- liftEffect cwd
  text <- lift $ readTextFile UTF8 (Path.concat [procesDir, modelDirectory, file])
  parseResult <- parseAndCache text
  case parseResult of
    (Right buitenRollen) -> do
      liftEffect $ log  "Attempting to save userdata..."
      saveDomeinFileAsUserData buitenRollen
      liftEffect $ log "Done. These are the items:\n"
      liftEffect $ log $ show buitenRollen
      liftEffect $ log "\n\n"
      pure []
    (Left e) -> do
      liftEffect $ log (show e)
      pure []
