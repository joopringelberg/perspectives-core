module Test.Perspectives.DataTypeObjectGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.DataTypeObjectGetters (buitenRol, buitenRol', context, contextType, iedereRolInContext, internePropertyTypen, isBuitenRol, label, propertyTypen, rolBindingDef, rolType, toSingle, typeVanIedereRolInContext)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (BuitenRol(..), RolDef(..), RolInContext(..), Value(..), binding, getUnqualifiedProperty)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, addTestContext, assertEqual, loadTestModel, p, removeTestContext, u, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

t1 :: String
t1 = """{ "id": "u:myContext"
  , "prototype": "psp:ContextPrototype"
  , "ctype": "psp:Context"
  , "rollen": { "psp:Context$rolInContext":  [ { "properties": {}, "binding": "psp:Context_buitenRol" }]}
  , "interneProperties": {}
  , "externeProperties": {}
  }"""

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "DataTypeObjectGetters" do
  test "Setting up" (addTestContext t1)
  test "contextType" do
    assertEqual "Context 'u:myContext' should have contextDef psp:Context as contextType."
      (contextType (u "myContext"))
      [p "Context"]
  test "toSingle" do
    assertEqual "toSingle unsafely takes a single value out of getter results"
      (unsafePartial $ toSingle contextType (u "myContext"))
      (p "Context")
  test "buitenRol" do
    assertEqual "u:myContext should have buitenRol u:myContext_buitenRol"
      (buitenRol (u "myContext"))
      [BuitenRol $ u "myContext_buitenRol"]
  test "buitenRol'" do
    assertEqual "No buitenrol for a non-existing context"
      (buitenRol' (p "DoesntExist"))
      (Nothing :: Maybe BuitenRol)
  test "iedereRolInContext" do
    assertEqual "u:myContext has a single role in context"
      (iedereRolInContext (u "myContext"))
      [(u "myContext$rolInContext_0")]
  test "typeVanIedereRolInContext" do
    assertEqual "u:myContext has a single type of role in context"
      (typeVanIedereRolInContext (u "myContext"))
      [(p "Context$rolInContext")]
  test "propertyTypen" do
    assertEqual "The BuitenRol of the Rol $mogelijkeBinding of psp:Rol has two properties"
      (propertyTypen (p "Rol$mogelijkeBinding_buitenRol"))
      [(p "Rol$buitenRolBeschrijving$isFunctioneel"),
      (p "Rol$buitenRolBeschrijving$isVerplicht")]
  testSkip "internePropertyTypen" do
      assertEqual "No instances of 'intern' in model:Perspectives"
        (internePropertyTypen (p ""))
        [(p ""),
        (p "")]
  test "label" do
    assertEqual "u:myContext has label 'myContext'"
      (label (u "myContext"))
      ["myContext"]
  test "rolType" do
    assertEqual "The role in myContext should have type 'psp:Context$RolInContext'"
      (((iedereRolInContext >=> pure <<< map RolInContext) /-/ rolType) (u "myContext"))
      [RolDef (p "Context$rolInContext")]

  -- test "binding" do
  --   assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving_buitenRol' as binding"
  --     (binding (RolInContext $ p "Context$rolInContext_1"))
  --     [RolDef (p "Context$binnenRolBeschrijving_buitenRol")]
  test "binding" do
    assertEqual "rolInContext of u:myContext should be bound to psp:Context_buitenRol"
      (((iedereRolInContext >=> pure <<< map RolInContext) /-/ binding) (u "myContext"))
      [RolInContext (p "Context_buitenRol")]
  test "context" do
    assertEqual "Context$rolInContext_1 should have 'psp:Context' as context"
      (context (RolInContext $ p "Context$rolInContext_1"))
      [p "Context"]
    assertEqual "The role of u:myContext should have 'u:myContext' as context"
      (((iedereRolInContext >=> pure <<< map RolInContext) /-/ context) (u "myContext"))
      [u "myContext"]
  test "rolBindingDef" do
    assertEqual "The role of u:myContext should have 'psp:Context' as rolBindingDef"
      ((iedereRolInContext >=> pure <<< map RolInContext) /-/ rolBindingDef $ (u "myContext"))
      [p "Context"]

  test "isBuitenRol" do
    assertEqual "The buitenRol of u:myContext is a BuitenRol."
      (isBuitenRol (BuitenRol $ u "myContext_buitenRol"))
      true

  testSkip "getUnqualifiedProperty" do
    loadTestModel "testTypeDefChecker.crl"
    assertEqual "myContext5 should have value false for external property contextDef2ExtProp1"
      ((buitenRol /-/ (getUnqualifiedProperty "contextDef2ExtProp1")) (t2 "myContext5"))
      [Value "false"]
    assertEqual "myContextDef5Rol1Prop1 should have a local value of $isVerplicht of true"
      ((buitenRol /-/ getUnqualifiedProperty "isVerplicht" )(t2 "myContextDef5$rol1$myContextDef5Rol1Prop1"))
      [Value "true"]
    assertEqual "myContextDef5Rol1Prop1 should have a local value of $isFunctioneel of false"
      ((buitenRol /-/ getUnqualifiedProperty "isFunctioneel" )(t2 "myContextDef5$rol1$myContextDef5Rol1Prop1"))
      [Value "false"]
    unLoadTestModel "model:TestTDC"

  test "Tearing down" do
    removeTestContext (u "myContext")
