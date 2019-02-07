module Test.Perspectives.ObjectGetterConstructors (theSuite) where

import Prelude

import Control.Monad.Eff.Now (NOW)
import Control.Monad.Free (Free)
import Perspectives.CoreTypes (type (~~>))
import Perspectives.DataTypeObjectGetters (binding, buitenRol, context, iedereRolInContext, rolBindingDef)
import Perspectives.ObjectGetterConstructors (all, closureOfBinding, concat, directAspectRoles, directAspects, getProperty, getRol, getUnqualifiedProperty, getUnqualifiedRol, searchQualifiedProperty, some)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypesInPurescript (BuitenRol(..), ContextDef(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..))
import Test.Perspectives.Utils (TestEffects, p, q, assertEqual)
import Test.Unit (TestF, suite, test, testSkip)

theSuite :: forall e. Free (TestF (TestEffects e)) Unit
theSuite = suite "DataTypeObjectGetters" do
  test "directAspects" do
    assertEqual "psp:Function should have direct aspect psp:Context"
      (directAspects (ContextDef $ p "Function"))
      [ContextDef $ p "Context"]
    assertEqual "q:ComputedRolGetter should have direct aspects psp:Function and psp:Rol"
      (directAspects (ContextDef $ q "ComputedRolGetter"))
      [ContextDef $ p "Function", ContextDef $ p "Rol"]
  testSkip "directAspectRoles" do
    assertEqual "Find an example of aspectRoles!"
      (directAspectRoles (RolDef $ p "XXX"))
      [RolDef $ p "YYY"]
  test "concat" do
    assertEqual "The concatenation of 'context' and 'rolBindingDef', applied to psp:Context$rolInContext_1 should result in [psp:Context, psp:Context$binnenRolBeschrijving]!"
      (concat context rolBindingDef (RolInContext $ p "Context$rolInContext_1"))
      [ContextDef (p "Context"), ContextDef (p "Context$binnenRolBeschrijving")]
  test "getProperty" do
    assertEqual "Property isFunctioneel of the Role 'range' of 'Property' should be true."
      (getProperty (PropertyDef (p "Rol$buitenRolBeschrijving$isFunctioneel")) (BuitenRol $ p "Property$range_buitenRol"))
      [PBool "true"]
  test "getUnqualifiedProperty" do
    assertEqual "Property isFunctioneel of the Role 'range' of 'Property' should be true."
      (getUnqualifiedProperty "isFunctioneel" (BuitenRol $ p "Property$range_buitenRol"))
      [PBool "true"]
  test "some" do
    assertEqual "Some roles defined to psp:Rol are functioneel"
      -- ((iedereRolInContext /-/ binding /-/ s5) (ContextDef $ p "Rol"))
      (some (iedereRolInContext /-/ binding /-/ ((getUnqualifiedProperty "isFunctioneel") :: (RolInContext ~~> PBool) (now :: NOW |e))) (ContextDef $ p "Rol"))
      [PBool "true"]
  test "all" do
    assertEqual "Some roles defined to psp:Rol are functioneel"
      -- ((iedereRolInContext /-/ binding /-/ s5) (ContextDef $ p "Rol"))
      (all (iedereRolInContext /-/ binding /-/ ((getUnqualifiedProperty "isFunctioneel") :: (RolInContext ~~> PBool) (now :: NOW |e))) (ContextDef $ p "Rol"))
      [PBool "false"]
  test "getRol" do
    assertEqual "psp:View has a single RolInContext: $propertyReferentie."
      (getRol (RolDef (p "Context$rolInContext")) (ContextDef $ p "View"))
      [RolInContext "model:Perspectives$View$rolInContext_1"]
  test "getUnqualifiedRol" do
    assertEqual "psp:View has a single RolInContext: $propertyReferentie."
      (getUnqualifiedRol "rolInContext" (ContextDef $ p "View"))
      [RolInContext "model:Perspectives$View$rolInContext_1"]
  test "closureOfBinding" do
    assertEqual "$mogelijkeBinding of $range of psp:Property is bound to psp:SimpleValue (its BuitenRol) and that is in turn bound to (the BuitenRol of) psp:ContextPrototype"
      ((getUnqualifiedRol "mogelijkeBinding" /-/ binding /-/ closureOfBinding) (RolDef (p "Property$range")) )
      [BuitenRol $ p "ContextPrototype_buitenRol"]
  test "searchQualifiedProperty" do
    assertEqual "The BuitenRol of psp:SimpleValue is bound to the BuitenRol of psp:ContextPrototype, which in turn bears the property isFunctioneel ('true')."
      (buitenRol /-/ (searchQualifiedProperty (PropertyDef $ p "isFunctioneel")) (ContextDef (p "SimpleValue")) )
      [PBool "true"]
