module Test.LoadArc where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.Couchdb.Revision (changeRevision)
import Perspectives.DomeinCache (removeDomeinFileFromCouchdb, retrieveDomeinFile)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.TypePersistence.LoadArc.FS (loadAndCompileArcFile, loadCompileAndCacheArcFile', loadCompileAndSaveArcFile, loadCompileAndSaveArcFile')
import Test.Perspectives.Utils (clearUserDatabase, runP, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.loadArc" do
  test "Load a model file and store it in Couchdb: reload and compare with original" do
    -- 1. Load and save a model.
    messages <- runP $ withSystem $ loadCompileAndSaveArcFile' "contextAndRole" testDirectory
    if null messages
      then pure unit
      else do
        logShow messages
        assert "The file could not be saved" false
    -- 2. Reload it from the database into the cache.
    retrievedModel <- runP $ retrieveDomeinFile $ DomeinFileId "model:ContextAndRole"
    -- 3. Reload the file without caching or saving.
    r <- runP $ withSystem $ loadAndCompileArcFile "contextAndRole" testDirectory
    -- 4. Compare the model in cache with the model from the file.
    -- logShow retrievedModel
    case r of
      Left e -> assert ("The same file loaded the second time fails: " <> show e) false
      Right (Tuple reParsedModel _) -> do
        -- logShow (changeRevision Nothing reParsedModel)
        assert "The model reloaded from couchdb should equal the model loaded from file."
          (eq (changeRevision Nothing retrievedModel) (changeRevision Nothing reParsedModel))
    runP $ removeDomeinFileFromCouchdb "model:ContextAndRole"

  test "Load model:System and cache it" do
    messages <- runP do
      addAllExternalFunctions
      -- 1. Load the required model:Couchdb.
      _ <- loadCompileAndCacheArcFile' "couchdb" modelDirectory
      -- 2. Load the required model:Serialise.
      _ <- loadCompileAndCacheArcFile' "serialise" modelDirectory
      -- 3. Try to load PerspectivesSystem.
      log "Starting on model:PerspectivesSystem"
      loadAndCompileArcFile "perspectivesSysteem" modelDirectory
    case messages of
      Left m -> do
        logShow messages
        assert "The file could not be parsed or compiled" false
      _ -> pure unit

  test "Load simpleChat and cache it" $ runP $ withSystem do
    messages <- loadAndCompileArcFile "simpleChat" modelDirectory
    case messages of
      Left m -> do
        logShow messages
        liftAff $ assert "The file could not be parsed or compiled" false
      _ -> pure unit

  test "Load TestBotActie and cache it" $ runP $ withSystem do
    messages <- loadAndCompileArcFile "testBotActie" modelDirectory
    case messages of
      Left m -> do
        logShow messages
        liftAff $ assert "The file could not be parsed or compiled" false
      _ -> pure unit

  test "Load model:Couchdb from file and store it in Couchdb" do
    messages <- runP do
      catchError (loadCompileAndSaveArcFile' "couchdb" modelDirectory)
        \e -> logShow e *> pure []
    if null messages
      then pure unit
      else do
        logShow messages
        assert "The file could not be parsed, compiled or saved" false
    runP $ removeDomeinFileFromCouchdb "model:Couchdb"

  test "Load model:Serialise from file and store it in Couchdb" do
    messages <- runP do
      catchError (loadCompileAndSaveArcFile' "serialise" modelDirectory)
        \e -> logShow e *> pure []
    if null messages
      then pure unit
      else do
        logShow messages
        assert "The file could not be parsed, compiled or saved" false
    runP $ removeDomeinFileFromCouchdb "model:Serialise"

  test "Load a model file and instances and store it in Couchdb" do
    messages <- runP do
      catchError (do
        void $ loadCompileAndCacheArcFile' "couchdb" modelDirectory
        void $ loadCompileAndCacheArcFile' "serialise" modelDirectory
        loadCompileAndSaveArcFile "perspectivesSysteem" modelDirectory
        )
        \e -> logShow e *> pure []
    if null messages
      then pure unit
      else do
        logShow messages
        assert "The file could not be parsed, compiled or saved" false
    runP do
      removeDomeinFileFromCouchdb "model:System"
      clearUserDatabase
