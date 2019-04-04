module Test.Perspectives.ModelBasedObjectGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (type (~~>))
import Perspectives.DataTypeObjectGetters (buitenRol)
import Perspectives.ModelBasedObjectGetters (propertyIsFunctioneel, propertyIsVerplicht, rolDef, rolIsVerplicht)
import Perspectives.ObjectGetterConstructors (directAspectProperties, getRoleBinders)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (BuitenRol, ContextRol(..), PBool(..), PropertyDef(..), RolDef(..))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "ModelBasedObjectGetters" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  test "rolIsVerplicht" do
    assertEqual "t:myAspect$myAspectRol1 is verplicht."
      (rolIsVerplicht (RolDef $ t "myAspect$myAspectRol1"))
      [PBool "true"]
    assertEqual "t:myContextDef$rol1 is verplicht by virtue of its aspectRol."
      (rolIsVerplicht (RolDef $ t "myContextDef$rol1"))
      [PBool "true"]
  test "propertyIsVerplicht" do
    assertEqual "t:myAspect$myAspectRol1$myAspectRol1Property is verplicht."
      (propertyIsVerplicht (PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"))
      [PBool "true"]
    assertEqual "Closure of directAspectproperties of t:myContextDef$rol1$rol1Property has two members"
      (directAspectProperties (PropertyDef $ t "myContextDef$rol1$rol1Property"))
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"]
    assertEqual "t:myContextDef$rol1$rol1Property is verplicht by virtue of its aspectProperty."
      (propertyIsVerplicht (PropertyDef $ t "myContextDef$rol1$rol1Property"))
      [PBool "true"]
    assertEqual "t:myContextDef4$rol1$rol1Property is verplicht."
      (propertyIsVerplicht (PropertyDef $ t "myContextDef4$rol1$rol1Property"))
      [PBool "false"]
  test "propertyIsFunctioneel" do
    assertEqual "t:myAspect$myAspectRol1$myAspectRol1Property is relational."
      (propertyIsFunctioneel (PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"))
      [PBool "false"]
    assertEqual "t:myContextDef$rol1$rol1Property is functional because it overrides its aspectProperty."
      (propertyIsFunctioneel (PropertyDef $ t "myContextDef$rol1$rol1Property"))
      [PBool "true"]
  test "propertyIsFunctioneel" do
    assertEqual "t:myAspect$myAspectRol1$myAspectRol1Property is relational."
      (propertyIsFunctioneel (PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"))
      [PBool "false"]
    assertEqual "t:myContextDef$rol1$rol1Property is functional because it overrides its aspectProperty."
      (propertyIsFunctioneel (PropertyDef $ t "myContextDef$rol1$rol1Property"))
      [PBool "true"]

  -- testOnly "Add a test!" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "testTypeDefChecker.crl"
  ---------------------------------------------------------------------------------
  test "Changing testfile" do
    unLoadTestModel "model:TestOGC"
    loadTestModel "testTypeDefChecker.crl"

  test "rolDef" do
    assertEqual ""
      ((unwrap >>> buitenRol /-/ (getRoleBinders (RolDef "model:Perspectives$Rol$rolProperty") :: forall eff. (BuitenRol ~~> ContextRol) eff)) (PropertyDef $ t2 "myContextDef6$rol1$myContextDef6Prop3"))
      [ContextRol "model:TestTDC$myContextDef6$rol1$rolProperty_3"]
    assertEqual "$myContextDef6Prop3 should have Rol $rol1 as defining Rol."
      (rolDef (PropertyDef $ t2 "myContextDef6$rol1$myContextDef6Prop3"))
      [(RolDef $ t2 "myContextDef6$rol1")]

  -- testOnly "Add a test!" do
  --   loadTestModel "testTypeDefChecker.crl"
  --
  --   unLoadTestModel "model:TestTDC"

  test "Tearing down" do
    unLoadTestModel "model:TestTDC"
  pure unit
