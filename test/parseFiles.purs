module Test.ParseFiles where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Perspectives.CollectDomeinFile (domeinFileFromContext)
import Perspectives.ContextRoleParser (ParseRoot(..), parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.SaveUserData (saveUserData)
import Perspectives.Syntax (PerspectContext)
import Perspectives.TypeDefChecker (checkModel)

modelDirectory :: String
modelDirectory = "/Users/joopringelberg/Code/perspectives-core/src/model"

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE, fs :: FS, exception :: EXCEPTION | e)) Unit
test = do
  lift $ log "=========================Parse a file==================="
  text <- lift $ liftEff $ readTextFile UTF8 (Path.concat [modelDirectory, "TestBotActie.crl"])
  parseResult <- parseAndCache text
  case parseResult of
    (Right parseRoot) ->
      case parseRoot of
        (RootContext textName)-> do
          -- save it
          lift $ log  "Attempting to save..."
          (mCtxt :: Maybe PerspectContext) <- catchError ((getPerspectEntiteit textName) >>= pure <<< Just)
            (\_ -> pure Nothing)
          case mCtxt of
            Nothing -> do
              lift $ log ("Cannot find context " <> textName)
            (Just ctxt) -> do
              df@(DomeinFile {_id}) <- domeinFileFromContext ctxt
              storeDomeinFileInCouchdb df
              lift $ log ("Saved, will attempt to run the typeDefChecker... " <> show textName)
              messages <- checkModel _id
              lift $ for_ messages
                \m -> do
                  log $ show m
                  log "------"

        (UserData buitenRollen) -> do
          lift $ log  "Attempting to save..."
          saveUserData buitenRollen
          lift $ log $ show buitenRollen
    (Left e) -> lift $ log (show e)
