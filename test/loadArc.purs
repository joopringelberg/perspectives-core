module Test.LoadArc where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Perspectives.TypePersistence.LoadArc (loadArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.loadArc" do
  test "Load a model file and store it in Couchdb" do
    messages <- runP $ loadArcFile "test1.arc" testDirectory
    if null messages
      then assert "bla" true
      else assert ("An error upon loading the arc file: " <> show messages) false
