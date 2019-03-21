module Test.Perspectives.TripleGetterConstructors (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Newtype (unwrap)
import Perspectives.DataTypeTripleGetters (binding, buitenRol, context, genericBinding, getUnqualifiedProperty, iedereRolInContext)
import Perspectives.PerspectivesTypes (BuitenRol(..), ContextDef(..), ContextRol(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..), Value(..))
import Perspectives.QueryCombinators (contains)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.StringTripleGetterConstructors (all, directAspects, getContextRol, searchInAspectsAndPrototypes, searchLocallyAndInPrototypeHierarchy, some)
import Perspectives.TripleGetterComposition (before, followedBy, (>->))
import Perspectives.TripleGetterConstructors (closure_, concat, directAspectRoles, getInternalProperty, getRolInContext, getRoleBinders, getUnqualifiedPropertyDefinition, getUnqualifiedRolDefinition, getUnqualifiedRolInContext, getUnqualifiedRoleBinders, searchContextRol, searchExternalProperty, searchExternalUnqualifiedProperty, searchInAspectRolesAndPrototypes, searchInPrototypeHierarchy, searchInternalUnqualifiedProperty, searchProperty, searchUnqualifiedProperty, searchUnqualifiedPropertyDefinition, searchUnqualifiedRol, searchUnqualifiedRolDefinition)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suite "TripleGetterConstructors" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  test "getContextRol" do
    assertEqual "The Context 'psp:Property' has three rolInContext roles."
      (((p "Property") ##= (getContextRol ( "model:Perspectives$Context$rolInContext"))) >>= (pure <<< length))
      3
  test "searchInAspectsAndPrototypes" do
    assertEqual "searchInAspectsAndPrototypes applied to getContextRol should yield at least as many results"
      (((p "Property") ##= searchInAspectsAndPrototypes (getContextRol ( "model:Perspectives$Context$rolInContext"))) >>= (pure <<< length))
      3
  test "searchLocallyAndInPrototypeHierarchy" do
    assertEqual "searchLocallyAndInPrototypeHierarchy applied to getContextRol should yield at least as many results"
      (((p "Property") ##= searchLocallyAndInPrototypeHierarchy (getContextRol ( "model:Perspectives$Context$rolInContext"))) >>= (pure <<< length))
      3
  test "closure_" do
    assertEqual "The inclusive closure of directAspects of t:myContextDef has four members!"
      ((t "myContextDef") ##= closure_ directAspects )
      [t "myContextDef", t "myAspect", t "myUrAspect", p "Context"]
  test "contains" do
    assertEqual "The directAspects of t:myContextDef include t:myAspect"
      ((t "myContextDef") ##= contains (t "myAspect") directAspects)
      [PBool "true"]
  test "searchInPrototypeHierarchy" do
    assertEqual "The prototype hierarchy of t:myContext includes t:myUrAspect, which instantiates the role t:myContextDef$rol1"
      ((t "myContext") ##= searchInPrototypeHierarchy (context >-> (getUnqualifiedRolInContext "rol1")))
      [RolInContext $ t "myContextPrototype$rol1_1"]
  test "directAspects" do
    assertEqual "t:myContextDef should have direct aspect t:myAspect"
      ((t "myContextDef") ##= directAspects )
      [t "myAspect"]
  test "directAspects" do
    assertEqual "t:myContextDef should have direct aspect t:myAspect"
      ((t "myContextDef") ##= directAspects)
      [t "myAspect"]
  test "directAspectRoles" do
    assertEqual "t:myContextDef$rol1 should have t:myAspect$myAspectRol1 as aspect role."
      ((RolDef $ t "myContextDef$rol1") ##= directAspectRoles)
      [RolDef $ t "myAspect$myAspectRol1"]
  test "concat" do
    assertEqual "The concatenation of 'aspect' and 'rolInContext', applied to t:myContextDef should result in [t:myAspect, t:myContextDef$rol1]!"
      ((t "myContextDef") ##= concat directAspects (iedereRolInContext `followedBy` RolInContext >-> binding >-> context))
      [(t "myAspect"), (t "myContextDef$rol1")]
  test "some" do
    assertEqual "Some roles defined to t:myAspect are functioneel"
      ((t "myAspect") ##= some (iedereRolInContext >-> genericBinding >-> (RolInContext `before` (getUnqualifiedProperty "isFunctioneel")) `followedBy` (PBool <<< unwrap)))
      [PBool "true"]
  test "all" do
    assertEqual "Not all roles defined to t:myAspect are functioneel"
      ((t "myAspect") ##= all (iedereRolInContext >-> genericBinding >-> (RolInContext `before` (getUnqualifiedProperty "isFunctioneel")) `followedBy` (PBool <<< unwrap)))
      [PBool "false"]
  test "getRolInContext" do
    assertEqual "t:myContextDef has a single RolInContext: $rol1."
      ((t "myContextDef") ##= getRolInContext (RolDef (p "Context$rolInContext")))
      [RolInContext $ t "myContextDef$rolInContext_1"]
  test "getUnqualifiedRolInContext" do
    assertEqual "t:myContextDef has a single RolInContext: $rol1."
      ((t "myContextDef") ##= getUnqualifiedRolInContext "rolInContext")
      [RolInContext $ t "myContextDef$rolInContext_1"]
    assertEqual "t:myContextPrototype has a binding for $myAspectRol2, which is defined in an Aspect."
      ((t "myContextPrototype") ##= getUnqualifiedRolInContext "myAspectRol2")
      [RolInContext $ t "myContextPrototype$myAspect$myAspectRol2_1"]
  test "searchContextRol" do
    assertEqual "t:myUrAspect has an instance of psp:Context$binnenRolBeschrijving"
      ((t "myUrAspect") ##= searchContextRol (RolDef $ p "Context$binnenRolBeschrijving"))
      [ContextRol $ p "ContextPrototype$binnenRolBeschrijving_1"]
  test "searchUnqualifiedRol" do
    assertEqual "t:myContext has $rol1 through its prototype"
      ((t "myContext") ##= (searchUnqualifiedRol "rol1") )
      [ContextRol $ t "myContextPrototype$rol1_1"]
    assertEqual "t:myContextPrototype has $binnenRolBeschrijving through its prototype"
      ((t "myContextPrototype") ##= (searchUnqualifiedRol "binnenRolBeschrijving") )
      [ContextRol $ p "ContextPrototype$binnenRolBeschrijving_1"]
  test "getUnqualifiedRolDefinition (Look for the definition of a Rol by its local name, in the ContextDef (not searching prototypes or Aspects))" do
    assertEqual "myContextDef defines the role $rol1"
      (ContextDef (t "myContextDef") ##= getUnqualifiedRolDefinition "rol1")
      [RolDef $ t "myContextDef$rol1"]
  test "searchUnqualifiedRolDefinition (Look for the definition of a Rol by its local name, in the ContextDef and its Aspects and in all their prototypes)" do
    assertEqual "myContextDef2 acquires the role t:myAspect$myAspectRol1 from its aspect t:myAspect"
      (ContextDef (t "myContextDef2") ##= searchUnqualifiedRolDefinition "myAspectRol1")
      [RolDef $ t "myAspect$myAspectRol1"]

  test "searchProperty (The value of the property pd, wherever in the telescope it is represented)" do
    assertEqual "rol1 of t:myContextPrototype assigns value 'true' to property myAspectRol1Property."
      ((t "myContextPrototype") ##= (getUnqualifiedRolInContext "rol1" >-> (searchProperty (PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"))))
      ([Value "false"]:: Array Value)
    assertEqual "We should be able to retrieve the property myAspectRol1Property from t:myContext, because it has t:myContextPrototype as prototype"
      ((t "myContext") ##= (searchUnqualifiedRol "rol1" >-> (searchProperty (PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"))))
      ([Value "false"]:: Array Value)
    assertEqual "The buitenRol of $myUrAspectRol1 should have a value for psp:Rol$buitenRolBeschrijving$isFunctioneel"
      ((t "myUrAspect$myUrAspectRol1") ##= (buitenRol >-> searchProperty (PropertyDef $ p "Rol$buitenRolBeschrijving$isFunctioneel")))
      [(Value "true")]
  test "searchInAspectRolesAndPrototypes" do
    assertEqual "The buitenRol of $myUrAspectRol1 should have a value for psp:Rol$buitenRolBeschrijving$isFunctioneel"
      ((t "myUrAspect$myUrAspectRol1") ##= (searchInAspectRolesAndPrototypes (buitenRol >-> searchProperty (PropertyDef $ p "Rol$buitenRolBeschrijving$isFunctioneel"))))
      [(Value "true")]
  test "searchUnqualifiedProperty (The value of the unqualifiedproperty pd, wherever in the telescope it is represented)" do
    assertEqual "We should be able to retrieve the property myAspectRol1Property from t:myContext, because it has t:myContextPrototype as prototype"
      ((t "myContext") ##= (searchUnqualifiedRol "rol1" >-> (searchUnqualifiedProperty "myAspectRol1Property")))
      ([Value "false"]:: Array Value)
  test "searchExternalProperty (Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope, shadowing any values)"
    do
      assertEqual "t:myContextWithExternalPropertyPrototype has a value for $extProp1"
        ((t "myContextWithExternalPropertyPrototype") ##= (searchExternalProperty $ PropertyDef $ t "myContextDefWithExternalProperty$buitenRolBeschrijving$extProp1"))
        ([Value "hello world!"]:: Array Value)
      assertEqual "t:myContextWithExternalProperty has a value for $extProp1 through its prototype"
        ((t "myContextWithExternalProperty") ##= (searchExternalProperty $ PropertyDef $ t "myContextDefWithExternalProperty$buitenRolBeschrijving$extProp1"))
        ([Value "hello world!"]:: Array Value)
      assertEqual "t:myContextWithExternalProperty has a value for $extProp1 through its prototype"
        ((t "myContextWithShadowedExternalProperty") ##= (searchExternalProperty $ PropertyDef $ t "myContextDefWithExternalProperty$buitenRolBeschrijving$extProp1"))
        ([Value "Rain on the roof"]:: Array Value)
  test "searchExternalUnqualifiedProperty (Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope, shadowing any values on the prototypes)" do
    assertEqual "t:myContextWithExternalPropertyPrototype has a value for $extProp1"
      ((t "myContextWithExternalPropertyPrototype") ##= (searchExternalUnqualifiedProperty "extProp1"))
      ([Value "hello world!"]:: Array Value)
    assertEqual "t:myContextWithExternalProperty has a value for $extProp1 through its prototype"
      ((t "myContextWithExternalProperty") ##= (searchExternalUnqualifiedProperty "extProp1"))
      ([Value "hello world!"]:: Array Value)
    assertEqual "t:myContextWithShadowedExternalProperty shadows the value for $extProp1 of its prototype"
      ((t "myContextWithShadowedExternalProperty") ##= (searchExternalUnqualifiedProperty "extProp1"))
      ([Value "Rain on the roof"]:: Array Value)
  test "getInternalProperty (Look for the property with the given local name on the binnenRol of the ContextType c)" do
    assertEqual "t:myContextWithInternalPropertyPrototype gives a value to $intProp1"
      ((t "myContextWithInternalPropertyPrototype") ##= (getInternalProperty $ PropertyDef $ t "myContextDefWithInternalProperty$binnenRolBeschrijving$intProp1"))
      [Value "hello world!"]
  test "searchInternalUnqualifiedProperty (Look for the property with the given local name on the binnenRol of the ContextType c and on its telescope)" do
    assertEqual "t:myContextWithInternalProperty has no value for $intProp1 through its prototype"
      ((t "myContextWithInternalProperty") ##= (searchInternalUnqualifiedProperty "intProp1"))
      ([]:: Array Value)
    assertEqual "t:myContextWithInternalProperty has a value for $someProp through its buitenRol"
      ((t "myContextWithInternalProperty") ##= (searchInternalUnqualifiedProperty "someProp"))
      ([Value "hello again."]:: Array Value)
    assertEqual "t:myContextWithShadowedInternalProperty has a value for its internal $someProp that shadows the value of its external $someProp"
      ((t "myContextWithShadowedInternalProperty") ##= (searchInternalUnqualifiedProperty "someProp"))
      ([Value "Sun on the roof"]:: Array Value)
    assertEqual "t:myContextWithShadowedInternalProperty has a value for its external $someProp."
      ((t "myContextWithShadowedInternalProperty") ##= (searchExternalUnqualifiedProperty "someProp"))
      ([Value "Rain on the roof"]:: Array Value)
  test "getRoleBinders (From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have it as their binding))" do
    assertEqual "The buitenRol of t:myContextWithExternalPropertyPrototype is bound to two Contexts that have declared it as their prototype"
      ((t "myContextWithExternalPropertyPrototype") ##= (buitenRol >-> getRoleBinders (RolDef $ t "myContextDefWithExternalProperty$buitenRolBeschrijving")) )
      ([BuitenRol $ t "myContextWithShadowedExternalProperty_buitenRol", BuitenRol $ t "myContextWithExternalProperty_buitenRol"])
  test "getUnqualifiedRoleBinders (From the instance of a Rol of any kind, find the instances of the Rol with the given local name that bind it (that have it as their binding). The type of ln can be buitenRolBeschrijving)"
    do
      assertEqual "The buitenRol of t:myContextWithExternalPropertyPrototype is bound to two Contexts that have declared it as their prototype"
        ((t "myContextWithExternalPropertyPrototype") ##= (buitenRol >-> getUnqualifiedRoleBinders "buitenRolBeschrijving"))
        ([BuitenRol $ t "myContextWithShadowedExternalProperty_buitenRol", BuitenRol $ t "myContextWithExternalProperty_buitenRol"])
  test "getUnqualifiedPropertyDefinition" do
    assertEqual "myAspectRol1 has a local definition for the property myAspectRol1Property."
      ((RolDef $ t "myAspect$myAspectRol1") ##= getUnqualifiedPropertyDefinition "myAspectRol1Property")
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"]
  test "searchUnqualifiedPropertyDefinition" do
    assertEqual "myAspectRol1 obtains a definition for the property myUrAspectRol1Property from its AspectRol."
      ((RolDef $ t "myAspect$myAspectRol1") ##= searchUnqualifiedPropertyDefinition "myUrAspectRol1Property")
      [PropertyDef $ t "myUrAspect$myUrAspectRol1$myUrAspectRol1Property"]

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --   unLoadTestModel "model:TestOGC"

  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
