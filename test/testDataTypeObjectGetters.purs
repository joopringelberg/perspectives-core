module Test.Perspectives.DataTypeObjectGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Perspectives.DataTypeObjectGetters (binding', buitenRol, buitenRol', iedereRolInContext, internePropertyTypen, label, propertyTypen, rolBindingDef, rolType, toSingle, typeVanIedereRolInContext)
import Perspectives.PerspectivesTypesInPurescript (Aspect(..), BuitenRol(..), ClusterGenoot(..), ContextDef(..), Gebruiker(..), PBoolBR(..), PString(..), PStringBR(..), PropertyDef(..), Range(..), RolDef(..), RolInContext(..), RolProperty(..), SimpleValueDefBR(..), View(..), binding, context, contextType)
import Test.Perspectives.Utils (TestEffects, p, assertEqual, u)
import Test.Unit (TestF, suite, suiteSkip, test, testSkip)

theSuite :: forall e. Free (TestF (TestEffects e)) Unit
theSuite = suite "DataTypeObjectGetters" do
  test "contextType" do
    assertEqual "The View 'Systeem$gebruiker$volledigeNaam' should have contextDef psp:View as contextType."
      (contextType (View $ p "Systeem$gebruiker$VolledigeNaam"))
      [ContextDef $ p "View"]
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
      (Nothing :: Maybe BuitenRol)
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
  -- test "binding" do
  --   assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving_buitenRol' as binding"
  --     (binding (RolInContext $ p "Context$rolInContext_1"))
  --     [RolDef (p "Context$binnenRolBeschrijving_buitenRol")]
  test "binding" do
    assertEqual "Clustergenoot should have a Gebruiker as binding"
      (binding (ClusterGenoot $ u "MijnSysteem$MijnCluster$clusterGenoot_1"))
      [Gebruiker (u "MijnSysteem$gebruiker_1")]
    assertEqual "Range of isVerplicht of Property should be Boolean"
      (binding (Range $ p "Property$buitenRolBeschrijving$isVerplicht$range_1"))
      [PBoolBR (p "Boolean_buitenRol")]
    assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving_buitenRol' as binding"
      (binding (RolInContext $ p "Context$rolInContext_1"))
      [RolDef (p "Context$binnenRolBeschrijving_buitenRol")]
  test "binding'" do
    assertEqual "Context$rolInContext_1 should have the single value 'psp:Context$binnenRolBeschrijving_buitenRol' as binding"
      (binding' (RolInContext $ p "Context$rolInContext_1"))
      (RolDef (p "Context$binnenRolBeschrijving_buitenRol"))
-- test "context" do
  --   assertEqual "Context$rolInContext_1 should have 'psp:Context' as context"
  --     (context (RolInContext $ p "Context$rolInContext_1"))
  --     [ContextDef (p "Context")]
  test "rolBindingDef" do
    assertEqual "Context$rolInContext_1 should have 'psp:Context$binnenRolBeschrijving' as rolBindingDef"
      (rolBindingDef (RolInContext $ p "Context$rolInContext_1"))
      [ContextDef (p "Context$binnenRolBeschrijving")]
  test "context" do
    assertEqual "The instance of the rol Aspect called 'psp:Zaak$aspect_1' should have 'psp:Zaak' as context."
      (context (Aspect $ p "Zaak$aspect_1"))
      [ContextDef (p "Zaak")]
    assertEqual "The instance of the rol RolProperty called 'psp:Systeem$gebruiker$rolProperty_1' should have 'psp:Systeem$gebruiker' as context."
      (context (RolProperty $ p "Systeem$gebruiker$rolProperty_1"))
      [RolDef (p "Systeem$gebruiker")]
