module Test.SetupCouchdb where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.SetupCouchdb (setupCouchdbForFirstUser)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.SetupCouchdb" do
  test "setupCouchdbForFirstUser" do
    setupCouchdbForFirstUser "cor" "geheim"
    assert "Just ran 'setupCouchdbForFirstUser'" true
