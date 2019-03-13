module Test.Perspectives.ModelBasedTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.ModelBasedTripleGetters (ownPropertiesDef, propertiesDef, rollenDef)
import Perspectives.PerspectivesTypes (BuitenRol(..), ContextRol(..), PropertyDef(..), RolDef(..), RolInContext(..), binding)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, addTestContext, assertEqual, loadTestModel, p, u, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suite "ModelBasedTripleGetters" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  test "rollenDef" do
    assertEqual "The Context 'psp:Property' defines three roles."
      ((p "Property") ##= rollenDef)
      -- []
      [ RolDef (p "Property$range")
      , RolDef (p "Property$aspectProperty")
      , RolDef (p "Property$bindingProperty")]
  test "ownPropertiesDef" do
    assertEqual "De roldefinitie t:myAspect$myAspectRol1 definieert de property $myAspectRol1Property."
      (RolDef (t "myAspect$myAspectRol1") ##= ownPropertiesDef)
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"]
  test "propertiesDef" do
    assertEqual "De roldefinitie t:myContextDef$rol1 ontleent property $myAspectRol1Property aan zijn aspectRol."
      (RolDef (t "myContextDef$rol1") ##= propertiesDef)
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"]
  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
