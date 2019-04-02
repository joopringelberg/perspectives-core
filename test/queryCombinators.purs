module Test.Perspectives.QueryCombinators (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (TypedTripleGetter, type (**>))
import Perspectives.DataTypeTripleGetters (rolType)
import Perspectives.ModelBasedTripleGetters (buitenRolBeschrijvingDef, contextBot, hasType, isContextTypeOf, isOrHasAspect, isRolTypeOf, mogelijkeBinding, nonQueryRollen, ownPropertiesDef, propertiesDef, rollenDef, sumToSequence)
import Perspectives.PerspectivesTypes (ContextDef(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..))
import Perspectives.QueryCombinators (contains, not, notEmpty, conj) as QC
import Perspectives.RunMonadPerspectivesQuery ((##=), (##>>))
import Perspectives.TripleGetterComposition (before, followedBy, (>->))
import Perspectives.TripleGetterConstructors (agreesWithType, closureOfAspect, closure_, directAspects, searchRolInContext, searchUnqualifiedRol)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, p, runP, unLoadTestModel, q)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "QueryCombinators" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "TestOGC.crl"
  ---------------------------------------------------------------------------------

  test "notEmpty" do
    assertEqual "myContextDef does have roles defined."
      ((t "myContextDef") ##= (QC.notEmpty rollenDef))
      [PBool "true"]
    assertEqual "t:myContext does not have roles defined."
      ((t "myContext") ##= (QC.notEmpty rollenDef))
      [PBool "false"]
  test "not" do
    assertEqual "myContextDef does have roles defined."
      ((t "myContextDef") ##= QC.not (QC.notEmpty rollenDef))
      [PBool "false"]
    assertEqual "t:myContext does not have roles defined."
      ((t "myContext") ##= QC.not (QC.notEmpty rollenDef))
      [PBool "true"]
  test "conj" do
    assertEqual "Conjunction of a test and its negation must be false"
      ((t "myContextDef") ##= QC.conj (QC.not (QC.notEmpty rollenDef)) (QC.notEmpty rollenDef))
      [PBool "false"]
    assertEqual "Conjunction of the same test twice must be true"
      ((t "myContextDef") ##= QC.conj (QC.notEmpty rollenDef) (QC.notEmpty rollenDef))
      [PBool "true"]

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
