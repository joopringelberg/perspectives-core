module Test.SetupCouchdb where

import Prelude

import Control.Monad.Free (Free)
import Effect.Aff.Class (liftAff)
import Perspectives.SetupCouchdb (setupCouchdbForAnotherUser, setupCouchdbForFirstUser)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "/Users/joopringelberg/Code/perspectives-core/test"

theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.SetupCouchdb" do
  test "setupCouchdbForFirstUser" do
    setupCouchdbForFirstUser "cor" "geheim"
    assert "Just ran 'setupCouchdbForFirstUser'" true

  testOnly "setupCouchdbForAnotherUser" (runP do
    setupCouchdbForAnotherUser "joop" "geheim"
    liftAff $ assert "Just ran 'setupCouchdbForAnotherUser'" true
)
