module Test.Perspectives.ObjectGetterConstructors (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.DataTypeObjectGetters (buitenRol, context, iedereRolInContext, rolBindingDef)
import Perspectives.ObjectGetterConstructors (all, closureOfBinding, concat, directAspectRoles, directAspects, getRolInContext, getUnqualifiedContextRol, getUnqualifiedRolInContext, searchProperty, some)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (BuitenRol(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..), Value(..), binding, genericBinding, getProperty, getUnqualifiedProperty)
import Test.Perspectives.Utils (TestEffects, p, q, assertEqual)
import Test.Unit (TestF, suite, suiteSkip, test, testSkip)

theSuite :: forall e. Free (TestF (TestEffects e)) Unit
theSuite = suiteSkip "ObjectGetterConstructors" do
  test "directAspects" do
    assertEqual "psp:Function should have direct aspect psp:Context"
      (directAspects (p "Function"))
      [p "Context"]
    assertEqual "q:ComputedRolGetter should have direct aspects psp:Function and psp:Rol"
      (directAspects (q "ComputedRolGetter"))
      [p "Function", p "Rol"]
  testSkip "directAspectRoles" do
    assertEqual "Find an example of aspectRoles!"
      (directAspectRoles (RolDef $ p "XXX"))
      [RolDef $ p "YYY"]
  test "concat" do
    assertEqual "The concatenation of 'context' and 'rolBindingDef', applied to psp:Context$rolInContext_1 should result in [psp:Context, psp:Context$binnenRolBeschrijving]!"
      (concat context rolBindingDef (RolInContext $ p "Context$rolInContext_1"))
      [(p "Context"), (p "Context$binnenRolBeschrijving")]
  test "getProperty" do
    assertEqual "Property isFunctioneel of the Role 'range' of 'Property' should be true."
      (getProperty (PropertyDef (p "Rol$buitenRolBeschrijving$isFunctioneel")) (BuitenRol $ p "Property$range_buitenRol"))
      [Value "true"]
  test "getUnqualifiedProperty" do
    assertEqual "Property isFunctioneel of the Role 'range' of 'Property' should be true."
      (getUnqualifiedProperty "isFunctioneel" (BuitenRol $ p "Property$range_buitenRol"))
      [Value "true"]
  test "some" do
    assertEqual "Some roles defined to psp:Rol are functioneel"
      (some (iedereRolInContext /-/ genericBinding /-/ ((getUnqualifiedProperty "isFunctioneel") <<< RolInContext) >=> pure <<< map (PBool <<< unwrap)) (p "Rol"))
      [PBool "true"]
  test "all" do
    assertEqual "Some roles defined to psp:Rol are functioneel"
      -- ((iedereRolInContext /-/ binding /-/ s5) (ContextDef $ p "Rol"))
      (all (iedereRolInContext /-/ genericBinding /-/ ((getUnqualifiedProperty "isFunctioneel") <<< RolInContext) >=> pure <<< map (PBool <<< unwrap)) (p "Rol"))
      [PBool "false"]
  test "getRol" do
    assertEqual "psp:View has a single RolInContext: $propertyReferentie."
      (getRolInContext (RolDef (p "Context$rolInContext")) (p "View"))
      [RolInContext "model:Perspectives$View$rolInContext_1"]
  test "getUnqualifiedRol" do
    assertEqual "psp:View has a single RolInContext: $propertyReferentie."
      (getUnqualifiedRolInContext "rolInContext" (p "View"))
      [RolInContext "model:Perspectives$View$rolInContext_1"]
  test "closureOfBinding" do
    assertEqual "$mogelijkeBinding of $range of psp:Property is bound to psp:SimpleValue (its BuitenRol) and that is in turn bound to (the BuitenRol of) psp:ContextPrototype"
      ((getUnqualifiedContextRol "mogelijkeBinding" /-/ binding /-/ closureOfBinding) (p "Property$range"))
      [BuitenRol $ p "ContextPrototype_buitenRol"]
  test "searchQualifiedProperty" do
    assertEqual "The BuitenRol of psp:SimpleValue is bound to the BuitenRol of psp:ContextPrototype, (but this has no properties!)which in turn bears the property isFunctioneel ('true')."
      ((buitenRol /-/ (searchProperty (PropertyDef $ p "isFunctioneel"))) (p "SimpleValue"))
      ([]:: Array Value)
  pure unit
