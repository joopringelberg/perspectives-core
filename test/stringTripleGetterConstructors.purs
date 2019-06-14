module Test.Perspectives.StringTripleGetterConstructors (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Perspectives.ModelBasedStringTripleGetters (searchView)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.StringTripleGetterConstructors (getUnqualifiedContextRol, getUnqualifiedRolDefinition, propertyReferenties, searchLocallyAndInPrototypeHierarchy, searchUnqualifiedRol, searchUnqualifiedRolDefinition)
import Test.Perspectives.Utils (assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

theSuite :: Free TestF Unit
theSuite = suiteSkip "StringTripleGetterConstructors" do
  -- test "Setting up" do
  --   loadTestModel "perspectives.crl"
  test "propertyReferenties" do
    assertEqual "psp:PerspectivesSysteem$gebruiker$VolledigeNaam has two roles $propertyReferentie"
      (((p "PerspectivesSysteem$gebruiker$VolledigeNaam") ##= propertyReferenties) >>= (pure <<< length))
      2

  test "searchUnqualifiedRol" do
    assertEqual "psp:PerspectivesSysteem$gebruiker$VolledigeNaam has two roles $propertyReferentie"
      (((p "PerspectivesSysteem$gebruiker$VolledigeNaam") ##= (searchUnqualifiedRol "propertyReferentie")) >>= (pure <<< length))
      2

  test "getUnqualifiedContextRol" do
    assertEqual "psp:PerspectivesSysteem$gebruiker$VolledigeNaam has two roles $propertyReferentie"
      (((p "PerspectivesSysteem$gebruiker$VolledigeNaam") ##= (getUnqualifiedContextRol "propertyReferentie")) >>= (pure <<< length))
      2

  test "getUnqualifiedContextRol" do
    assertEqual "psp:PerspectivesSysteem$gebruiker$VolledigeNaam has two roles $propertyReferentie"
      (((p "PerspectivesSysteem$gebruiker$VolledigeNaam") ##= (searchLocallyAndInPrototypeHierarchy (getUnqualifiedContextRol "propertyReferentie"))) >>= (pure <<< length))
      2

  test "getUnqualifiedRolDefinition" do
    assertEqual "model:Perspectives$PerspectivesSysteem has a rol 'modelsInUse'"
      ((p "PerspectivesSysteem") ##= getUnqualifiedRolDefinition "modelsInUse")
      [p "PerspectivesSysteem$modelsInUse"]

  test "searchUnqualifiedRolDefinition" do
    assertEqual "model:Perspectives$PerspectivesSysteem$gebruiker has a view VolledigeNaam"
      ((p "PerspectivesSysteem$gebruiker") ##= searchView "VolledigeNaam")
      [p "PerspectivesSysteem$gebruiker$VolledigeNaam"]


  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --   unLoadTestModel "model:TestOGC"

  -- test "Tearing down" do
  --   unLoadTestModel "model:TestOGC"
