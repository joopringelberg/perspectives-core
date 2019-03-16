module Test.Perspectives.ModelBasedObjectGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.DataTypeObjectGetters (buitenRol, context, iedereRolInContext)
import Perspectives.DataTypeTripleGetters (propertyTypen) as DTG
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijvingDef, propertyIsVerplicht, rolIsVerplicht)
import Perspectives.ObjectGetterConstructors (closure_, directAspectProperties)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (BuitenRol(..), ContextDef(..), ContextRol(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..), Value(..), binding, genericBinding, getProperty, getUnqualifiedProperty)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suite "ObjectGetterConstructors" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  test "rolIsVerplicht" do
    assertEqual "t:myAspect$myAspectRol1 is verplicht."
      (rolIsVerplicht (RolDef $ t "myAspect$myAspectRol1"))
      [PBool "true"]
    assertEqual "t:myContextDef$rol1 is verplicht by virtue of its aspectRol."
      (rolIsVerplicht (RolDef $ t "myContextDef$rol1"))
      [PBool "true"]
  testOnly "propertyIsVerplicht" do
    loadTestModel "TestOGC.crl"
    assertEqual "t:myAspect$myAspectRol1$myAspectRol1Property is verplicht."
      (propertyIsVerplicht (PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"))
      [PBool "true"]
    assertEqual "Closure of directAspectproperties of t:myContextDef$rol1$rol1Property has two members"
      (directAspectProperties (PropertyDef $ t "myContextDef$rol1$rol1Property"))
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"]
    assertEqual "t:myContextDef$rol1$rol1Property is verplicht by virtue of its aspectProperty."
      (propertyIsVerplicht (PropertyDef $ t "myContextDef$rol1$rol1Property"))
      [PBool "true"]
    unLoadTestModel "model:TestOGC"
    
  -- testOnly "Add a test!" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
  pure unit
