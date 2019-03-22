module Test.Perspectives.TypeDefChecker (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.ModelBasedTripleGetters (buitenRolBeschrijvingDef, mogelijkeBinding, ownPropertiesDef, propertiesDef, rollenDef)
import Perspectives.PerspectivesTypes (PropertyDef(..), RolDef(..))
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, p, typeDefCheckerNotifies, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suite "TypeDefChecker" do
  test "All types" do
    typeDefCheckerNotifies "TestOGC.crl" ["MissingRange", "MissingAspect"]
