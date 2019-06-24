module Test.Perspectives.TripleGetterComposition (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Effect.Aff (Milliseconds(..), delay)
import Perspectives.Actions (removeProperty', setProperty')
import Perspectives.ModelBasedTripleGetters (hasContextType, isContextTypeOf)
import Perspectives.PerspectivesTypes (PBool(PBool))
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.StringTripleGetterConstructors (getInternalProperty)
import Perspectives.TripleGetterComposition (preferLeft, traverse)
import Perspectives.TripleGetterConstructors (closure_, directAspects, some)
import Test.Perspectives.Utils (addTestContext, assertEqual, assertEqualWithPropagation, loadTestModel, removeTestContext, u, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

tba :: String -> String
tba s = "model:TestBotActie$" <> s

theSuite :: Free TestF Unit
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
      (t "myContextDef" ##= traverse isContextTypeOf "hasContextType" (closure_ directAspects))
      [PBool "false", PBool "true"]
    assertEqual "t:myContextDef has some of its (indirect) Aspects as its type."
      (t "myContextDef" ##= some (traverse hasContextType "hasContextType" (closure_ directAspects)))
      [PBool "true"]

  test "preferLeft" do
    loadTestModel "testBotActie.crl"
    t1 <- pure """{ "id": "u:mc1"
      , "ctype": "model:TestBotActie$Test"
      , "rollen": {}
      , "interneProperties": {"model:TestBotActie$Test$binnenRolBeschrijving$v2": ["aap"]}
      , "externeProperties": {}
      }"""
    (addTestContext t1)
    assertEqual "$v2 moet gelijk zijn aan 'aap'"
      (u "mc1" ##= (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v2" ))
      ["aap"]
    assertEqual "(v1 `preferLeft` v2) moet gelijk zijn aan 'aap'"
      (u "mc1" ##= preferLeft (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v1") (\_ -> (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v2")) "v2")
      ["aap"]
    assertEqualWithPropagation "Maak $v1 gelijk aan 'noot'. v1 `preferLeft` v2 is nu gelijk aan 'noot'."
      do
        void $ setProperty' (tba "Test$binnenRolBeschrijving$v1") "noot" (u "mc1_binnenRol")
        lift $ delay (Milliseconds 100.0)
        (u "mc1" ##= preferLeft (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v1") (\_ -> (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v2")) "v2")
      ["noot"]
      500.0
    assertEqualWithPropagation "Verwijder de waarde van $v1; v1 `preferLeft` $v2 is nu weer gelijk aan 'aap'."
      do
        void $ removeProperty' (tba "Test$binnenRolBeschrijving$v1") "noot" (u "mc1_binnenRol")
        lift $ delay (Milliseconds 100.0)
        (u "mc1" ##= preferLeft (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v1") (\_ -> (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v2")) "v2")
      ["aap"]
      500.0
    assertEqualWithPropagation "Maak $v2 nu gelijk aan 'mies'. v1 `preferLeft` v2 is nu gelijk aan 'mies'."
      do
        void $ setProperty' (tba "Test$binnenRolBeschrijving$v2") "mies" (u "mc1_binnenRol")
        lift $ delay (Milliseconds 100.0)
        (u "mc1" ##= preferLeft (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v1") (\_ -> (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v2")) "v2")
      ["mies"]
      500.0
    assertEqualWithPropagation "Maak $v1 nu gelijk aan 'teun'. v1 `preferLeft` v2 is nu gelijk aan 'teun'."
      do
        void $ setProperty' (tba "Test$binnenRolBeschrijving$v1") "teun" (u "mc1_binnenRol")
        lift $ delay (Milliseconds 100.0)
        (u "mc1" ##= preferLeft (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v1") (\_ -> (getInternalProperty "model:TestBotActie$Test$binnenRolBeschrijving$v2")) "v2")
      ["teun"]
      500.0

    (removeTestContext (u "mc1"))
    -- unLoadTestModel "model:TestBotActie"

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
