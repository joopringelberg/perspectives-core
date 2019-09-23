module Test.Perspectives.QueryCombinators (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.DataTypeTripleGetters (context, genericContext, label)
import Perspectives.ModelBasedTripleGetters (rollenDef)
import Perspectives.PerspectivesTypes (BuitenRol(..), PBool(..), PropertyDef(..))
import Perspectives.QueryAST (ElementaryQueryStep(..)) as QA
import Perspectives.QueryCombinators (not, notEmpty, conj, equal) as QC
import Perspectives.QueryCompiler (constructQueryFunction, constructUnqualifiedGetter, getPropertyFunction)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterConstructors (getInternalProperty)
import Test.Perspectives.Utils (assertEqual, loadTestModel, removeTestContext, u, unLoadTestModel, p)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

tba :: String -> String
tba s = "model:TestBotActie$" <> s

theSuite :: Free TestF Unit
theSuite = suiteSkip "QueryCombinators" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "TestOGC.crl"
  ---------------------------------------------------------------------------------

  test "notEmpty" do
    assertEqual "t:myContext does have roles defined."
      ((t "myContext") ##= (QC.notEmpty rollenDef))
      [PBool "true"]
  test "not" do
    assertEqual "myContextDef does have roles defined."
      ((t "myContextDef") ##= QC.not (QC.notEmpty rollenDef))
      [PBool "false"]
    assertEqual "t:myContext does not have roles defined."
      ((t "myContext") ##= QC.not (QC.notEmpty rollenDef))
      [PBool "false"]
  test "conj" do
    assertEqual "Conjunction of a test and its negation must be false"
      ((t "myContextDef") ##= QC.conj (QC.not (QC.notEmpty rollenDef)) (QC.notEmpty rollenDef))
      [PBool "false"]
    assertEqual "Conjunction of the same test twice must be true"
      ((t "myContextDef") ##= QC.conj (QC.notEmpty rollenDef) (QC.notEmpty rollenDef))
      [PBool "true"]

  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "queryCombinators.crl"
  ---------------------------------------------------------------------------------
  test "equal" do
    loadTestModel "testBotActie.crl"
    loadTestModel "queryCombinators.crl"

    assertEqual "equality applied to two equal string properties should return true"
      ((u "qctest1") ##= (QC.equal
        (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v1"))
        (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v2"))))
      [PBool "true"]

    assertEqual "equality applied to two different string properties should return false"
      ((u "qctest2") ##= (QC.equal
        (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v1"))
        (getInternalProperty (PropertyDef $ tba "Test$binnenRolBeschrijving$v2"))))
      [PBool "false"]

    -- unLoadTestModel "model:TestBotActie"

  test "contextLabel" do
    assertEqual "The contextLabel of the buitenRol of u:MijnSysteem should be 'MijnSysteem'"
      do
        ((u "MijnSysteem_buitenRol") ##= genericContext >-> label)
      ["MijnSysteem"]
    assertEqual "The contextLabel of the buitenRol of u:MijnSysteem should be 'MijnSysteem'"
      do
        getter <- constructQueryFunction $ p "BuitenRolPrototype$contextLabel"
        ((u "MijnSysteem_buitenRol") ##= getter)
      ["MijnSysteem"]

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
    removeTestContext "usr:qctest1"
    removeTestContext "usr:qctest2"
