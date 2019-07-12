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
import Perspectives.ContextRoleParser (ParseRoot(..), parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives, UserMessage)
import Perspectives.DomeinCache (removeDomeinFileFromCouchdb, storeDomeinFileInCache, storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..))

import Perspectives.Identifiers (Namespace)
import Perspectives.PerspectivesState (domeinCacheRemove)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.SaveUserData (saveDomeinFileAsUserData)
import Perspectives.InstanceRepresentation (PerspectContext)
import Perspectives.TypeDefChecker (checkModel)

modelDirectory :: String
modelDirectory = "./src/model"

type FileName = String

withSemanticChecks :: Boolean
withSemanticChecks = true

withoutSemanticChecks :: Boolean
withoutSemanticChecks = false

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
    (Right (Tuple parseRoot domeinFile@(DomeinFile dfr))) ->
      case parseRoot of
        (RootContext textName)-> do
          (mCtxt :: Maybe PerspectContext) <- catchError ((getPerspectEntiteit textName) >>= pure <<< Just)
            (\_ -> do
              liftEffect $ log ("Model file parsed, but root of '" <> textName <> "' results in runtime error!\n")
              pure Nothing)
          case mCtxt of
            Nothing -> do
              liftEffect $ log ("Model file parsed, but cannot find the root context for '" <> textName <> "'!\n")
              pure []
            (Just ctxt) -> do
              df@(DomeinFile {_id}) <- pure $ DomeinFile dfr {_id = textName}
              (messages :: Array UserMessage) <-
                if checkSemantics
                  then do
                    liftEffect $ log ("Parsed. Run the typeDefChecker on " <> show textName <> ".\n")
                    -- put it in the DomeinFileCache.
                    void $ storeDomeinFileInCache _id df
                    checkModel _id <* domeinCacheRemove _id
                  else do
                    liftEffect $ log ("Parsed. Skipping typeDefChecker for " <> show textName <> ".\n")
                    pure []
              case null messages of
                true -> do
                  liftEffect $ log $ "Model '" <> textName <> (if checkSemantics then "' is OK and will be stored.\n" else "' will be stored.\n")
                  storeDomeinFileInCouchdb df
                false -> do
                  liftEffect $ log $ "Model '" <> textName <> "' contains semantical errors and is not saved!\n"
                  liftEffect $ for_ messages
                    \m -> do
                      log $ show m
                      log "------"
                  liftEffect $ log "\n\n"
              pure messages

        (UserData buitenRollen) -> do
          liftEffect $ log  "Attempting to save userdata..."
          saveDomeinFileAsUserData domeinFile
          liftEffect $ log "Done. These are the items:\n"
          liftEffect $ log $ show buitenRollen
          liftEffect $ log "\n\n"
          pure []
    (Left e) -> do
      liftEffect $ log (show e)
      pure []

-- | Remove the model from the model cache
unloadModel :: Namespace -> MonadPerspectives Unit
unloadModel = void <<< domeinCacheRemove

-- | Remove the file from Couchdb and the model from the cache.
unLoadCRLFile :: Namespace -> MonadPerspectives Unit
unLoadCRLFile = removeDomeinFileFromCouchdb
