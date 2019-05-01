module Test.Perspectives.QueryCompiler (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.ModelBasedTripleGetters (rollenDef)
import Perspectives.PerspectivesTypes (PBool(..), PropertyDef(..))
import Perspectives.QueryCombinators (not, notEmpty, conj, equal) as QC
import Perspectives.QueryCompiler (constructQueryFunction)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterConstructors (getInternalProperty)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, removeTestContext, u, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

tba :: String -> String
tba s = "model:TestBotActie$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "QueryCompiler" do
  test "Setting up" do
    loadTestModel "testBotActie.crl"

  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "queryCompiler.crl"
  ---------------------------------------------------------------------------------
  test "equal" do
    loadTestModel "testBotActie.crl"
    loadTestModel "queryCompiler.crl"

    assertEqual "$propsEqual should return the application of equal."
      do
        propsEqual <- constructQueryFunction (tba "Test$binnenRolBeschrijving$propsEqual")
        (u "qcrtest1") ##= propsEqual
      ["true"]

    assertEqual "$propsEqual should return the application of equal."
      do
        propsEqual <- constructQueryFunction (tba "Test$binnenRolBeschrijving$propsEqual")
        (u "qcrtest2") ##= propsEqual
      ["false"]

    -- unLoadTestModel "model:TestBotActie"


  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Tearing down" do
    removeTestContext "usr:qcrtest1"
    removeTestContext "usr:qcrtest2"
