module Test.LoadArc where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console (logShow)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.Couchdb.Revision (changeRevision)
import Perspectives.TypePersistence.LoadArc (loadCompileAndSaveArcFile, loadCompileAndSaveArcFile', loadAndCompileArcFile)
import Test.Perspectives.Utils (runP, setupUser)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite  "Perspectives.loadArc" do
  test "Load a model file and store it in Couchdb: reload and compare with original" do
    -- 1. Load and save a model.
    messages <- runP $ loadCompileAndSaveArcFile' "contextAndRole" testDirectory
    if null messages
      then pure unit
      else do
        logShow messages
        assert "The file could not be saved" false
    -- 2. Reload it from the database into the cache.
    retrievedModel <- runP $ retrieveDomeinFile "model:ContextAndRole"
    -- 3. Reload the file without caching or saving.
    r <- runP $ loadAndCompileArcFile "contextAndRole" testDirectory
    -- 4. Compare the model in cache with the model from the file.
    -- logShow retrievedModel
    case r of
      Left e -> assert "The same file loaded the second time fails" false
      Right reParsedModel -> do
        -- logShow (changeRevision Nothing reParsedModel)
        assert "The model reloaded from couchdb should equal the model loaded from file."
          (eq (changeRevision Nothing retrievedModel) (changeRevision Nothing reParsedModel))

  test "Load a model file and cache it" do
    -- 1. Load and save a model.
    messages <- runP (loadAndCompileArcFile "perspectivesSysteem" modelDirectory)
    case messages of
      Left m -> do
        logShow messages
        assert "The file could not be parsed or compiled" false
      _ -> pure unit

  test "Load a model file and store it in Couchdb" do
    -- 1. Load and save a model.
    messages <- runP do
      setupUser
      catchError (loadCompileAndSaveArcFile "TestBotActie" modelDirectory)
        \e -> logShow e *> pure []
    if null messages
      then pure unit
      else do
        logShow messages
        assert "The file could not be parsed, compiled or saved" false
