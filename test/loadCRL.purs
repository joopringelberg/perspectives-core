module Test.LoadCRL where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.LoadCRL (loadCRLFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.loadCRL" do
  test "Load a file with a context instance" do
    void $ runP $ loadCRLFile "systeemInstanties.crl"
    assert "bla" true
