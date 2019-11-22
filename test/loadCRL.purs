module Test.LoadCRL where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Either (Either)
import Data.Tuple (Tuple)
import Effect.Class.Console (logShow)
import Foreign.Object (Object)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.LoadCRL (loadAndSaveCrlFile, loadCrlFile)
import Perspectives.Parsing.Messages (PerspectivesError)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.loadCRL" do
  test "Load a file with a context instance in cache" do
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- runP $ loadCrlFile "test1.crl" testDirectory
    logShow r
    pure unit

  testOnly "Load a file with a context instance in couchdb" do
    r <- runP $ loadAndSaveCrlFile "test1.crl" testDirectory
    if null r
      then pure unit
      else do
        logShow r
        assert "Expected to load a file into couchdb" false
