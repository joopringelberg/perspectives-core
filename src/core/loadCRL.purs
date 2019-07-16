module Perspectives.LoadCRL where

import Prelude

import Effect.Console (log)
import Effect.Class (liftEffect)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Node.Process (cwd)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives, UserMessage)
import Perspectives.SaveUserData (saveDomeinFileAsUserData)
import Perspectives.InstanceRepresentation (PerspectContext)

modelDirectory :: String
modelDirectory = "./src/model"

type FileName = String

-- | Loads a file from the directory "src/model" relative to the directory of the
-- | active process.
-- | All definitions are loaded into the cache. The file is parsed and stored in Couchdb.
loadCRLFile :: Boolean -> FileName -> MonadPerspectives (Array UserMessage)
loadCRLFile checkSemantics file = do
  liftEffect $ log ("=========================Parse the file " <> file <> "===================")
  procesDir <- liftEffect cwd
  text <- lift $ readTextFile UTF8 (Path.concat [procesDir, modelDirectory, file])
  parseResult <- parseAndCache text
  case parseResult of
    (Right buitenRollen) -> do
      liftEffect $ log  "Attempting to save userdata..."
      saveDomeinFileAsUserData domeinFile
      liftEffect $ log "Done. These are the items:\n"
      liftEffect $ log $ show buitenRollen
      liftEffect $ log "\n\n"
      pure []
    (Left e) -> do
      liftEffect $ log (show e)
      pure []
