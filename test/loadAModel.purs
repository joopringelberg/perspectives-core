module Test.Perspectives.LoadAModel (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.LoadCRL (loadCRLFile, withSemanticChecks, withoutSemanticChecks)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, runP)
import Test.Unit (TestF, suite, suiteSkip, test)

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suite "Loading the model:" do
  test "" do
    void $ runP $ loadCRLFile withSemanticChecks "testBotActie.crl"
    -- void $ runP $ loadCRLFile withSemanticChecks "query.crl"
  -- test "Unloading the model" do
    -- unLoadTestModel "model:Perspectives"
