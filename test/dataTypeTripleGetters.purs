module Test.Perspectives.DataTypeTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.DataTypeTripleGetters (buitenRol, getUnqualifiedProperty)
import Perspectives.PerspectivesTypes (Value(..))
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterComposition ((>->))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "DataTypeTripleGetters" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "TestOGC.crl"
  ---------------------------------------------------------------------------------

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Changing testfile" do
    unLoadTestModel "model:TestOGC"
    loadTestModel "testTypeDefChecker.crl"

  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "testTypeDefChecker.crl"
  ---------------------------------------------------------------------------------


  test "getUnqualifiedProperty" do
    loadTestModel "testTypeDefChecker.crl"
    assertEqual "myContext5 should have value false for external property contextDef2ExtProp1"
      ((t2 "myContext5") ##= buitenRol >-> getUnqualifiedProperty "contextDef2ExtProp1")
      [Value "false"]
    assertEqual "myContextDef5Rol1Prop1 should have a local value of $isVerplicht of true"
      ((t2 "myContextDef5$rol1$myContextDef5Rol1Prop1") ##= buitenRol >-> getUnqualifiedProperty "isVerplicht")
      [Value "true"]
    assertEqual "myContextDef5Rol1Prop1 should have a local value of $isFunctioneel of false"
      ((t2 "myContextDef5$rol1$myContextDef5Rol1Prop1") ##= buitenRol >-> getUnqualifiedProperty "isFunctioneel")
      [Value "false"]
    unLoadTestModel "model:TestTDC"

  -- testOnly "" do
  --   loadTestModel "testTypeDefChecker.crl"
  --
  --   unLoadTestModel "model:TestTDC"

  test "Tearing down" do
    unLoadTestModel "model:TestTDC"
