module Test.Perspectives.LoadAModel (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.LoadCRL (loadCRLFile, withSemanticChecks, withoutSemanticChecks)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test)

theSuite :: Free TestF Unit
theSuite = suiteSkip "Loading the model:" do
  test "loadCRLFile" do
    void $ runP $ loadCRLFile withSemanticChecks "systeemInstanties.crl"
    -- void $ runP $ loadCRLFile withSemanticChecks "query.crl"
  -- test "Unloading the model" do
    -- unLoadTestModel "model:Perspectives"
