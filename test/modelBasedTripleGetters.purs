module Test.Perspectives.ModelBasedTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.ModelBasedTripleGetters (ownPropertiesDef, propertiesDef, rollenDef)
import Perspectives.PerspectivesTypes (PropertyDef(..), RolDef(..))
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "ModelBasedTripleGetters" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  test "rollenDef" do
    assertEqual "The Context 'psp:Property' defines three roles."
      ((p "Property") ##= rollenDef)
      -- []
      [ RolDef (p "Property$range")
      , RolDef (p "Property$aspectProperty")
      , RolDef (p "Property$bindingProperty")]
    assertEqual "myContextDef defines three roles"
      ((t "myContextDef") ##= rollenDef)
      (map RolDef ["model:TestOGC$myContextDef$rol1","model:TestOGC$myAspect$myAspectRol1","model:TestOGC$myAspect$myAspectRol2","model:TestOGC$myUrAspect$myUrAspectRol1","model:Perspectives$Context$binnenRolBeschrijving","model:Perspectives$Context$buitenRolBeschrijving","model:Perspectives$Context$rolInContext","model:Perspectives$Context$interneView","model:Perspectives$Context$externeView","model:Perspectives$Context$prototype","model:Perspectives$Context$aspect","model:Perspectives$Context$gebruikerRol","model:Perspectives$Context$contextBot"])
  test "ownPropertiesDef" do
    assertEqual "De roldefinitie t:myAspect$myAspectRol1 definieert de property $myAspectRol1Property."
      (RolDef (t "myAspect$myAspectRol1") ##= ownPropertiesDef)
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"]
  test "propertiesDef" do
    assertEqual "De roldefinitie t:myContextDef$rol1 ontleent property $myAspectRol1Property aan zijn aspectRol."
      (RolDef (t "myContextDef$rol1") ##= propertiesDef)
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property",
      PropertyDef $ t "myUrAspect$myUrAspectRol1$myUrAspectRol1Property"]
  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
