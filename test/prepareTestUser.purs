module Test.PrepareTestUser where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.RunPerspectives (runPerspectives)
import Test.Perspectives.Utils (setupCouchdbForTestUser)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test)

theSuite :: Free TestF Unit
theSuite = suiteSkip "Test.PrepareTestUser" do

  test "setupCouchdbForTestUser" (runPerspectives "cor" "geheim" "cor" setupCouchdbForTestUser)
