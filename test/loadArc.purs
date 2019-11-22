module Test.LoadArc where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console (logShow)
import Foreign.Object (lookup)
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
  test "Load a model file and store it in Couchdb" do
    messages <- runP $ loadAndSaveArcFile "test1.arc" testDirectory
    if null messages
      then pure unit
      else do
        logShow messages
        assert "The file could not be saved" false
