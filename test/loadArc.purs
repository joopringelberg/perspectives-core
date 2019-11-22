module Test.LoadArc where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console (logShow)
import Foreign.Object (lookup)
import Perspectives.DomeinCache (removeDomeinFileFromCache, retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadAndSaveArcFile, loadArcFile)
import Simple.JSON (writeJSON)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.loadArc" do
  test "Load a model file and store it in Couchdb: reload and compare with original" do
    -- 1. Load and save a model.
    messages <- runP $ loadAndSaveArcFile "test1.arc" testDirectory
    if null messages
      then pure unit
      else do
        logShow messages
        assert "The file could not be saved" false
    -- 2. Clear it from the cache. NOTE: because we re-run perspectives, this is unnecessary.
    runP $ removeDomeinFileFromCache "model:Test"
    -- 3. Reload it from the database into the cache.
    retrievedModel <- runP $ retrieveDomeinFile "model:Test"
    -- 4. Reload the file without caching or saving.
    r <- runP $ loadArcFile "test1.arc" testDirectory
    -- 5. Compare the model in cache with the model from the file.
    case r of
      Left e -> assert "The same file loaded the second time fails" false
      Right reParsedModel -> assert "The model reloaded from couchdb should equal the model loaded from file."
        (eq retrievedModel reParsedModel)
