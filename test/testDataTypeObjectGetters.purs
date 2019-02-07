module Test.Perspectives.DataTypeObjectGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Perspectives.DataTypeObjectGetters (binding, binding', buitenRol, buitenRol', context, contextType, iedereRolInContext, internePropertyTypen, label, propertyTypen, rolBindingDef, rolType, toSingle, typeVanIedereRolInContext)
import Perspectives.PerspectivesTypesInPurescript (BuitenRol(..), ContextDef(..), PString(..), PropertyDef(..), RolDef(..), RolInContext(..))
import Test.Perspectives.Utils (TestEffects, p, assertEqual)
import Test.Unit (TestF, suite, suiteSkip, test, testSkip)

theSuite :: forall e. Free (TestF (TestEffects e)) Unit
theSuite = suiteSkip "DataTypeObjectGetters" do
  test "contextType" do
    assertEqual "psp:SimpleValue should have contextType psp:Context"
      (contextType (ContextDef $ p "SimpleValue"))
      [ContextDef $ p "Context"]
  test "toSingle" do
    assertEqual "toSingle unsafely takes a single value out of getter results"
      (toSingle contextType (ContextDef $ p "SimpleValue"))
      (ContextDef $ p "Context")
  test "buitenRol" do
    assertEqual "psp:SimpleValue should have buitenRol psp:SimpleValue_buitenRol"
      (buitenRol (ContextDef $ p "SimpleValue"))
      [BuitenRol $ p "SimpleValue_buitenRol"]
  test "buitenRol'" do
    assertEqual "psp:SimpleValue should have buitenRol psp:SimpleValue_buitenRol"
      (buitenRol' (ContextDef $ p "SimpleValue"))
      (Just (BuitenRol $ p "SimpleValue_buitenRol"))
    assertEqual "No buitenrol for a non-existing context"
      (buitenRol' (ContextDef $ p "DoesntExist"))
      Nothing
  test "iedereRolInContext" do
    assertEqual "psp:ContextPrototype has two roles in context"
      (iedereRolInContext (ContextDef $ p "ContextPrototype"))
      [RolInContext (p "ContextPrototype$buitenRolBeschrijving_1"),
      RolInContext (p "ContextPrototype$binnenRolBeschrijving_1")]
  test "typeVanIedereRolInContext" do
    assertEqual "psp:ContextPrototype has two types of role in context"
      (typeVanIedereRolInContext (ContextDef $ p "ContextPrototype"))
      [RolDef (p "Context$buitenRolBeschrijving"),
      RolDef (p "Context$binnenRolBeschrijving")]
  test "propertyTypen" do
    assertEqual "The BuitenRol of the Rol $mogelijkeBinding of psp:Rol has two properties"
      (propertyTypen (BuitenRol $ p "Rol$mogelijkeBinding_buitenRol"))
      [PropertyDef (p "Rol$buitenRolBeschrijving$isFunctioneel"),
      PropertyDef (p "Rol$buitenRolBeschrijving$isVerplicht")]
  testSkip "internePropertyTypen" do
      assertEqual "No instances of 'intern' in model:Perspectives"
        (internePropertyTypen (ContextDef $ p ""))
        [PropertyDef (p ""),
        PropertyDef (p "")]
  test "label" do
    assertEqual "psp:SimpleValue has label 'SimpleValue'"
      (label (ContextDef $ p "SimpleValue"))
      [PString "SimpleValue"]
  test "rolType" do
    assertEqual "Context$rolInContext_1 should have type 'psp:Context$RolInContext'"
      (rolType (RolInContext $ p "Context$rolInContext_1"))
      [RolDef (p "Context$rolInContext")]
  test "binding" do
    assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving_buitenRol' as binding"
      (binding (RolInContext $ p "Context$rolInContext_1"))
      [BuitenRol (p "Context$binnenRolBeschrijving_buitenRol")]
  test "binding'" do
    assertEqual "Context$rolInContext_1 should have the single value 'psp:Context$binnenRolBeschrijving_buitenRol' as binding"
      (binding' (RolInContext $ p "Context$rolInContext_1"))
      (BuitenRol (p "Context$binnenRolBeschrijving_buitenRol"))
  test "context" do
    assertEqual "Context$rolInContext_1 should have 'psp:Context' as context"
      (context (RolInContext $ p "Context$rolInContext_1"))
      [ContextDef (p "Context")]
  test "rolBindingDef" do
    assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving' as context"
      (rolBindingDef (RolInContext $ p "Context$rolInContext_1"))
      [ContextDef (p "Context$binnenRolBeschrijving")]
