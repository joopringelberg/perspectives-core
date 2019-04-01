module Perspectives.LoadCRL where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Node.Process (PROCESS, cwd)
import Perspectives.CollectDomeinFile (domeinFileFromContext)
import Perspectives.ContextRoleParser (ParseRoot(..), parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives, UserMessage)
import Perspectives.DomeinCache (removeDomeinFileFromCouchdb, storeDomeinFileInCache, storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (Namespace)
import Perspectives.PerspectivesState (domeinCacheRemove)
import Perspectives.PerspectivesTypes (BuitenRol(..))
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.SaveUserData (saveUserData)
import Perspectives.Syntax (PerspectContext)
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
loadCRLFile :: forall e. Boolean -> FileName -> MonadPerspectives (AjaxAvarCache (console :: CONSOLE, fs :: FS, exception :: EXCEPTION, process :: PROCESS | e)) (Array UserMessage)
loadCRLFile checkSemantics file = do
  lift $ log ("=========================Parse the file " <> file <> "===================")
  procesDir <- liftEff cwd
  text <- lift $ liftEff $ readTextFile UTF8 (Path.concat [procesDir, modelDirectory, file])
  parseResult <- parseAndCache text
  case parseResult of
    (Right parseRoot) ->
      case parseRoot of
        (RootContext textName)-> do
          (mCtxt :: Maybe PerspectContext) <- catchError ((getPerspectEntiteit textName) >>= pure <<< Just)
            (\_ -> do
              lift $ log ("Model file parsed, but root of '" <> textName <> "' results in runtime error!\n")
              pure Nothing)
          case mCtxt of
            Nothing -> do
              lift $ log ("Model file parsed, but cannot find the root context for '" <> textName <> "'!\n")
              pure []
            (Just ctxt) -> do
              df@(DomeinFile {_id}) <- domeinFileFromContext ctxt
              (messages :: Array UserMessage) <-
                if checkSemantics
                  then do
                    lift $ log ("Parsed. Run the typeDefChecker on " <> show textName <> ".\n")
                    -- put it in the DomeinFileCache.
                    void $ storeDomeinFileInCache _id df
                    checkModel _id <* domeinCacheRemove _id
                  else do
                    lift $ log ("Parsed. Skipping typeDefChecker for " <> show textName <> ".\n")
                    pure []
              case null messages of
                true -> do
                  lift $ log $ "Model '" <> textName <> (if checkSemantics then "' is OK and will be stored.\n" else "' will be stored.\n")
                  storeDomeinFileInCouchdb df
                false -> do
                  lift $ log $ "Model '" <> textName <> "' contains semantical errors and is not saved!\n"
                  lift $ for_ messages
                    \m -> do
                      log $ show m
                      log "------"
                  lift $ log "\n\n"
              pure messages

        (UserData buitenRollen) -> do
          lift $ log  "Attempting to save userdata..."
          saveUserData (map BuitenRol buitenRollen)
          lift $ log "Done. These are the items:\n"
          lift $ log $ show buitenRollen
          lift $ log "\n\n"
          pure []
    (Left e) -> do
      lift $ log (show e)
      pure []

-- | Remove the model from the model cache
unloadModel :: forall e. Namespace -> MonadPerspectives (AjaxAvarCache e) Unit
unloadModel = void <<< domeinCacheRemove

-- | Remove the file from Couchdb and the model from the cache.
unLoadCRLFile :: forall e. Namespace -> MonadPerspectives (AjaxAvarCache e) Unit
unLoadCRLFile = removeDomeinFileFromCouchdb
