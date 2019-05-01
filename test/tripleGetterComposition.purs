module Test.Perspectives.TripleGetterComposition (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.ModelBasedTripleGetters (hasType)
import Perspectives.PerspectivesTypes (PBool(PBool))
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterComposition (traverse)
import Perspectives.TripleGetterConstructors (closure_, directAspects, some)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "TripleGetterComposition" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "TestOGC.crl"
  ---------------------------------------------------------------------------------
  test "traverse" do
    assertEqual "t:myContextDef has four direct and indirect Aspects: "
      (t "myContextDef" ##= closure_ directAspects)
      ["model:TestOGC$myContextDef","model:TestOGC$myAspect","model:TestOGC$myUrAspect","model:Perspectives$Context"]
    assertEqual "t:myContextDef has none of its Aspects as type, except psp:Context."
      (t "myContextDef" ##= traverse hasType "hasType" (closure_ directAspects))
      [PBool "false", PBool "true"]
    assertEqual "t:myContextDef has some of its (indirect) Aspects as its type."
      (t "myContextDef" ##= some (traverse hasType "hasType" (closure_ directAspects)))
      [PBool "true"]

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
