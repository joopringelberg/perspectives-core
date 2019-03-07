module Test.Perspectives.DataTypeObjectGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.DataTypeObjectGetters (buitenRol, buitenRol', context, contextType, iedereRolInContext, internePropertyTypen, label, propertyTypen, rolBindingDef, rolType, toSingle, typeVanIedereRolInContext)
import Perspectives.PerspectivesTypes (BuitenRol(..), ContextRol(..), RolDef(..), RolInContext(..), binding)
import Test.Perspectives.Utils (TestEffects, addTestContext, assertEqual, p, u)
import Test.Unit (TestF, suite, suiteSkip, test, testSkip)

t1 :: String
t1 = """{ "id": "u:myContext"
  , "ctype": "psp:Context"
  , "rollen": { "psp:Context$rolInContext":  [ { "properties": {}, "binding": "psp:Context_buitenRol" }]}
  , "interneProperties": {}
  , "externeProperties": {}
  }"""

theSuite :: forall e. Free (TestF (TestEffects e)) Unit
theSuite = suite "DataTypeObjectGetters" do
  test "Setting up" (addTestContext t1)
  test "contextType" do
    assertEqual "The View 'Systeem$gebruiker$volledigeNaam' should have contextDef psp:View as contextType."
      (contextType (p "Systeem$gebruiker$VolledigeNaam"))
      [p "View"]
  test "toSingle" do
    assertEqual "toSingle unsafely takes a single value out of getter results"
      (unsafePartial $ toSingle contextType (p "SimpleValue"))
      (p "Context")
  test "buitenRol" do
    assertEqual "psp:SimpleValue should have buitenRol psp:SimpleValue_buitenRol"
      (buitenRol (p "SimpleValue"))
      [BuitenRol $ p "SimpleValue_buitenRol"]
  test "buitenRol'" do
    assertEqual "psp:SimpleValue should have buitenRol psp:SimpleValue_buitenRol"
      (buitenRol' (p "SimpleValue"))
      (Just (BuitenRol $ p "SimpleValue_buitenRol"))
    assertEqual "No buitenrol for a non-existing context"
      (buitenRol' (p "DoesntExist"))
      (Nothing :: Maybe BuitenRol)
  test "iedereRolInContext" do
    assertEqual "psp:ContextPrototype has two roles in context"
      (iedereRolInContext (p "ContextPrototype"))
      [(p "ContextPrototype$buitenRolBeschrijving_1"),
      (p "ContextPrototype$binnenRolBeschrijving_1")]
  test "typeVanIedereRolInContext" do
    assertEqual "psp:ContextPrototype has two types of role in context"
      (typeVanIedereRolInContext (p "ContextPrototype"))
      [(p "Context$buitenRolBeschrijving"),
      (p "Context$binnenRolBeschrijving")]
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
    assertEqual "psp:SimpleValue has label 'SimpleValue'"
      (label (p "SimpleValue"))
      ["SimpleValue"]
    assertEqual "u:myContext has label 'myContext'"
      (label (u "myContext"))
      ["myContext"]
  test "rolType" do
    assertEqual "Context$rolInContext_1 should have type 'psp:Context$RolInContext'"
      (rolType (RolInContext $ p "Context$rolInContext_1"))
      [RolDef (p "Context$rolInContext")]
  -- test "binding" do
  --   assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving_buitenRol' as binding"
  --     (binding (RolInContext $ p "Context$rolInContext_1"))
  --     [RolDef (p "Context$binnenRolBeschrijving_buitenRol")]
  test "binding" do
    assertEqual "Clustergenoot should have a Gebruiker as binding"
      (binding (RolInContext $ u "MijnSysteem$MijnCluster$clusterGenoot_1"))
      [RolInContext (u "MijnSysteem$gebruiker_1")]
    assertEqual "Range of isVerplicht of Property should be Boolean"
      (binding (RolInContext $ p "Property$buitenRolBeschrijving$isVerplicht$range_1"))
      [RolInContext (p "Boolean_buitenRol")]
    assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving_buitenRol' as binding"
      (binding (RolInContext $ p "Context$rolInContext_1"))
      [RolInContext (p "Context$binnenRolBeschrijving_buitenRol")]
  test "context" do
    assertEqual "Context$rolInContext_1 should have 'psp:Context' as context"
      (context (RolInContext $ p "Context$rolInContext_1"))
      [p "Context"]
  test "rolBindingDef" do
    assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving' as rolBindingDef"
      (rolBindingDef (RolInContext $ p "Context$rolInContext_1"))
      [p "Context$binnenRolBeschrijving"]
  test "context" do
    assertEqual "The instance of the rol Aspect called 'psp:Zaak$aspect_1' should have 'psp:Zaak' as context."
      (context (ContextRol $ p "Zaak$aspect_1"))
      [p "Zaak"]
    assertEqual "The instance of the rol RolProperty called 'psp:Systeem$gebruiker$rolProperty_1' should have 'psp:Systeem$gebruiker' as context."
      (context (ContextRol $ p "Systeem$gebruiker$rolProperty_1"))
      [p "Systeem$gebruiker"]
