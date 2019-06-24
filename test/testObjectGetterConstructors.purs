module Test.Perspectives.ObjectGetterConstructors (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.DataTypeObjectGetters (buitenRol, context, contextType, iedereRolInContext)
import Perspectives.DataTypeTripleGetters (propertyTypen) as DTG
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijvingDef)
import Perspectives.ObjectGetterConstructors (all, closureOfAspect, closureOfBinding, closure_, concat, containedIn, directAspectRoles, directAspects, getInternalProperty, getRolInContext, getRoleBinders, getUnqualifiedPropertyDefinition, getUnqualifiedRolDefinition, getUnqualifiedRolInContext, getUnqualifiedRoleBinders, hasLocalRolDefinition, hasRolDefinition, mogelijkeBinding, searchContextRol, searchExternalProperty, searchExternalUnqualifiedProperty, searchInPrototypeHierarchy, searchInternalUnqualifiedProperty, searchProperty, searchUnqualifiedProperty, searchUnqualifiedPropertyDefinition, searchUnqualifiedRol, searchUnqualifiedRolDefinition, some)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (BuitenRol(..), ContextDef(..), ContextRol(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..), Value(..), binding, genericBinding, getProperty, getUnqualifiedProperty)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Test.Perspectives.Utils (assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

theSuite :: Free TestF Unit
theSuite = suiteSkip "ObjectGetterConstructors" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  test "closure_" do
    assertEqual "The inclusive closure of directAspects of t:myContextDef has four members!"
      (closure_ directAspects (t "myContextDef"))
      [t "myContextDef", t "myAspect", t "myUrAspect", p "Context"]
  test "searchInPrototypeHierarchy" do
    assertEqual "The prototype hierarchy of t:myContext includes t:myUrAspect, which instantiates the role t:myContextDef$rol1"
      (searchInPrototypeHierarchy (context /-/ (getUnqualifiedRolInContext "rol1")) (t "myContext"))
      [RolInContext $ t "myContextPrototype$rol1_0001"]
  test "containedIn" do
    assertEqual "The directAspects of t:myContextDef include t:myAspect"
      (containedIn (t "myAspect") directAspects (t "myContextDef"))
      [PBool "true"]
  test "directAspects" do
    assertEqual "t:myContextDef should have direct aspect t:myAspect"
      (directAspects (t "myContextDef"))
      [t "myAspect"]
  test "directAspectRoles" do
    assertEqual "t:myContextDef$rol1 should have t:myAspect$myAspectRol1 as aspect role."
      (directAspectRoles (RolDef $ t "myContextDef$rol1"))
      [RolDef $ t "myAspect$myAspectRol1"]
  test "closureOfAspect" do
    assertEqual "closureOfAspect of t:myContextDef should have three Aspects"
      (closureOfAspect (t "myContextDef"))
      [t "myAspect", t "myUrAspect", p "Context"]
  test "concat" do
    assertEqual "The concatenation of 'aspect' and 'rolInContext', applied to t:myContextDef should result in [t:myAspect, t:myContextDef$rol1]!"
      (concat directAspects (iedereRolInContext >=> (pure <<< map RolInContext) /-/ binding /-/ context) (t "myContextDef"))
      [(t "myAspect"), (t "myContextDef$rol1")]
  test "getProperty" do
    assertEqual "The external Property isFunctioneel of the Role 'rol1' of 't:myContextDef' should be true."
      (getProperty (PropertyDef (p "Rol$buitenRolBeschrijving$isFunctioneel")) (BuitenRol $ t "myContextDef$rol1_buitenRol"))
      [Value "true"]
  test "getUnqualifiedProperty" do
    assertEqual "The external Property isFunctioneel of the Role 'rol1' of 't:myContextDef' should be true."
      (getUnqualifiedProperty "isFunctioneel" (BuitenRol $ t "myContextDef$rol1_buitenRol"))
      [Value "true"]
    assertEqual "The rolproperty $myAspectRol1Property of t:myContextPrototype should be false"
      ((searchUnqualifiedRol "rol1" /-/ getUnqualifiedProperty "myAspectRol1Property") (t "myContextPrototype"))
      [Value "false"]
  test "searchUnqualifiedRol" do
    assertEqual "t:myContext has $rol1 through its prototype"
      ((searchUnqualifiedRol "rol1") (t "myContext"))
      [ContextRol $ t "myContextPrototype$rol1_0001"]
    assertEqual "t:myContextPrototype has $binnenRolBeschrijving through its prototype"
      ((searchUnqualifiedRol "binnenRolBeschrijving") (t "myContextPrototype"))
      [ContextRol $ p "ContextPrototype$binnenRolBeschrijving_1"]
  test "propertyTypen (The types of every property for which this rol has a value)" do
    assertEqual "The buitenrol of t:myAspect$myAspectRol1 has one external property"
      (t "myAspect$myAspectRol1_buitenRol" ##= DTG.propertyTypen)
      ["model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht"]
  test "some" do
    assertEqual "Some roles defined to t:myUrAspect are functioneel"
      (some (iedereRolInContext /-/ genericBinding /-/ ((getUnqualifiedProperty "isFunctioneel") <<< RolInContext) >=> pure <<< map (PBool <<< unwrap)) (t "myUrAspect"))
      [PBool "true"]
  test "all" do
    assertEqual "Not all roles defined to t:myAspect are functioneel"
      (all (iedereRolInContext /-/ genericBinding /-/ ((getUnqualifiedProperty "isFunctioneel") <<< RolInContext) >=> pure <<< map (PBool <<< unwrap)) (t "myAspect"))
      [PBool "false"]
  test "getRolInContext" do
    assertEqual "t:myContextDef has a single RolInContext: $rol1."
      (getRolInContext (RolDef (p "Context$rolInContext")) (t "myContextDef"))
      [RolInContext $ t "myContextDef$rolInContext_0001"]
  test "getUnqualifiedRolInContext" do
    assertEqual "t:myContextDef has a single RolInContext: $rol1."
      (getUnqualifiedRolInContext "rolInContext" (t "myContextDef"))
      [RolInContext $ t "myContextDef$rolInContext_0001"]
    assertEqual "t:myContextPrototype has a binding for $myAspectRol2, which is defined in an Aspect."
      (getUnqualifiedRolInContext "myAspectRol2" (t "myContextPrototype"))
      [RolInContext $ t "myContextPrototype$myAspect$myAspectRol2_0001"]
  test "searchContextRol" do
    assertEqual "t:myUrAspect has an instance of psp:Context$binnenRolBeschrijving"
      (searchContextRol (RolDef $ p "Context$binnenRolBeschrijving") (t "myUrAspect"))
      [ContextRol $ p "ContextPrototype$binnenRolBeschrijving_1"]
  test "closureOfBinding" do
    -- TODO: als het nieuwe model is opgeslagen, deze test mee laten draaien.
    -- assertEqual "$range of t:myAspect$myAspectRol1$myAspectRol1Property is bound to psp:Boolean (its BuitenRol) and that is in turn bound to (the BuitenRol of) psp:SimpleValuePrototype"
    --   ((getUnqualifiedContextRol "range" /-/ binding /-/ closureOfBinding) (t "myAspect$myAspectRol1$myAspectRol1Property"))
    --   [BuitenRol $ p "SimpleValuePrototype_buitenRol"]
    assertEqual "t:myContextDef3 has t:myContextDef as prototype and that has ContextPrototype as prototype. Hence closureOfBinding of buitenRol t:myContextDef3 has two buitenRollen."
      ((buitenRol /-/ closureOfBinding /-/ context) (t "myContextDef3"))
      [t "myContextDef", p "ContextPrototype"]
  test "searchProperty (The value of the property pd, wherever in the telescope it is represented)" do
    assertEqual "rol1 of t:myContextPrototype assigns value 'true' to property myAspectRol1Property."
      ((getUnqualifiedRolInContext "rol1" /-/ (searchProperty (PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"))) (t "myContextPrototype"))
      ([Value "false"]:: Array Value)
    assertEqual "We should be able to retrieve the property myAspectRol1Property from t:myContext, because it has t:myContextPrototype as prototype"
      ((searchUnqualifiedRol "rol1" /-/ (searchProperty (PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"))) (t "myContext"))
      ([Value "false"]:: Array Value)
  test "searchUnqualifiedProperty (The value of the unqualifiedproperty pd, wherever in the telescope it is represented)" do
    assertEqual "We should be able to retrieve the property myAspectRol1Property from t:myContext, because it has t:myContextPrototype as prototype"
      ((searchUnqualifiedRol "rol1" /-/ (searchUnqualifiedProperty "myAspectRol1Property")) (t "myContext"))
      ([Value "false"]:: Array Value)
  test "getUnqualifiedRolDefinition (Look for the definition of a Rol by its local name, in the ContextDef (not searching prototypes or Aspects))" do
    assertEqual "myContextDef defines the role $rol1"
      (getUnqualifiedRolDefinition "rol1" $ ContextDef (t "myContextDef"))
      [RolDef $ t "myContextDef$rol1"]
  test "searchUnqualifiedRolDefinition (Look for the definition of a Rol by its local name, in the ContextDef and its Aspects and in all their prototypes)" do
    assertEqual "myContextDef2 acquires the role t:myAspect$myAspectRol1 from its aspect t:myAspect"
      (searchUnqualifiedRolDefinition "myAspectRol1" $ ContextDef (t "myContextDef2"))
      [RolDef $ t "myAspect$myAspectRol1"]
    assertEqual "for model:TestBotActie$Test a role with local name 'binnenRolBeschrijving' is defined through its Aspect psp:Context."
      ((contextType >=> (pure <<< map ContextDef) /-/ (searchUnqualifiedRolDefinition "binnenRolBeschrijving")) "model:TestBotActie$Test")
      [RolDef $ "model:Perspectives$Context$binnenRolBeschrijving"]
  test "buitenRolBeschrijvingDef" do
    assertEqual "From a context that is a definition, get the definition of its BuitenRol."
      (buitenRolBeschrijvingDef (t "myContextDef"))
      [RolDef $ p "ContextPrototype$buitenRolBeschrijving"]
    assertEqual "Found through three layers."
      (buitenRolBeschrijvingDef (t "myContextDef3"))
      [RolDef $ p "ContextPrototype$buitenRolBeschrijving"]
  test "getUnqualifiedPropertyDefinition" do
    assertEqual "The Rol t:myUrAspect$myUrAspectRol1 has the definition of property $myUrAspectRol1Property"
      ((getUnqualifiedPropertyDefinition "myUrAspectRol1Property") (RolDef $ t "myUrAspect$myUrAspectRol1"))
      [PropertyDef $ t "myUrAspect$myUrAspectRol1$myUrAspectRol1Property"]
  test "searchUnqualifiedPropertyDefinition" do
    assertEqual "The Rol t:myContextDef$rol1 has, through its AspectRollen, the definition of property $myUrAspectRol1Property"
      ((searchUnqualifiedPropertyDefinition "myUrAspectRol1Property") (RolDef $ t "myContextDef$rol1"))
      [PropertyDef $ t "myUrAspect$myUrAspectRol1$myUrAspectRol1Property"]
  test "mogelijkeBinding" do
    assertEqual "The mogelijkeBinding of $myUrAspectRol1 is psp:Rol"
      (mogelijkeBinding (RolDef $ t "myUrAspect$myUrAspectRol1"))
      [p "Rol"]
  test "hasLocalRolDefinition" do
    assertEqual "t:myAspect has a local definition for $myAspectRol2"
      (hasLocalRolDefinition (RolDef $ t "myAspect$myAspectRol2") (ContextDef $ t "myAspect"))
      [PBool "true"]
  test "hasRolDefinition" do
    assertEqual "t:myContextDef has a definition for $myAspectRol2 through its Aspect t:myAspect"
      (hasRolDefinition (RolDef $ t "myAspect$myAspectRol2") (ContextDef $ t "myContextDef"))
      [PBool "true"]
  test "searchExternalProperty (Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope, shadowing any values)" do
    assertEqual "t:myContextWithExternalPropertyPrototype has a value for $extProp1"
      ((searchExternalProperty $ PropertyDef $ t "myContextDefWithExternalProperty$buitenRolBeschrijving$extProp1") (t "myContextWithExternalPropertyPrototype"))
      ([Value "hello world!"]:: Array Value)
    assertEqual "t:myContextWithExternalProperty has a value for $extProp1 through its prototype"
      ((searchExternalProperty $ PropertyDef $ t "myContextDefWithExternalProperty$buitenRolBeschrijving$extProp1") (t "myContextWithExternalProperty"))
      ([Value "hello world!"]:: Array Value)
    assertEqual "t:myContextWithExternalProperty has a value for $extProp1 through its prototype"
      ((searchExternalProperty $ PropertyDef $ t "myContextDefWithExternalProperty$buitenRolBeschrijving$extProp1") (t "myContextWithShadowedExternalProperty"))
      ([Value "Rain on the roof"]:: Array Value)
  test "searchExternalUnqualifiedProperty (Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope, shadowing any values on the prototypes)" do
    assertEqual "t:myContextWithExternalPropertyPrototype has a value for $extProp1"
      ((searchExternalUnqualifiedProperty "extProp1") (t "myContextWithExternalPropertyPrototype"))
      ([Value "hello world!"]:: Array Value)
    assertEqual "t:myContextWithExternalProperty has a value for $extProp1 through its prototype"
      ((searchExternalUnqualifiedProperty "extProp1") (t "myContextWithExternalProperty"))
      ([Value "hello world!"]:: Array Value)
    assertEqual "t:myContextWithShadowedExternalProperty shadows the value for $extProp1 of its prototype"
      ((searchExternalUnqualifiedProperty "extProp1") (t "myContextWithShadowedExternalProperty"))
      ([Value "Rain on the roof"]:: Array Value)
  test "getInternalProperty (Look for the property with the given local name on the binnenRol of the ContextType c)" do
    assertEqual "t:myContextWithInternalPropertyPrototype gives a value to $intProp1"
      ((getInternalProperty $ PropertyDef $ t "myContextDefWithInternalProperty$binnenRolBeschrijving$intProp1") (t "myContextWithInternalPropertyPrototype"))
      [Value "hello world!"]
  test "searchInternalUnqualifiedProperty (Look for the property with the given local name on the binnenRol of the ContextType c and on its telescope)" do
    assertEqual "t:myContextWithInternalProperty has no value for $intProp1 through its prototype"
      ((searchInternalUnqualifiedProperty "intProp1") (t "myContextWithInternalProperty"))
      ([]:: Array Value)
    assertEqual "t:myContextWithInternalProperty has a value for $someProp through its buitenRol"
      ((searchInternalUnqualifiedProperty "someProp") (t "myContextWithInternalProperty"))
      ([Value "hello again."]:: Array Value)
    assertEqual "t:myContextWithShadowedInternalProperty has a value for its internal $someProp that shadows the value of its external $someProp"
      ((searchInternalUnqualifiedProperty "someProp") (t "myContextWithShadowedInternalProperty"))
      ([Value "Sun on the roof"]:: Array Value)
    assertEqual "t:myContextWithShadowedInternalProperty has a value for its external $someProp."
      ((searchExternalUnqualifiedProperty "someProp") (t "myContextWithShadowedInternalProperty"))
      ([Value "Rain on the roof"]:: Array Value)
  test "getRoleBinders (From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have it as their binding))" do
    assertEqual "The buitenRol of t:myContextWithExternalPropertyPrototype is bound to two Contexts that have declared it as their prototype"
      ((buitenRol /-/ getRoleBinders (RolDef $ t "myContextDefWithExternalProperty$buitenRolBeschrijving")) (t "myContextWithExternalPropertyPrototype"))
      ([BuitenRol $ t "myContextWithExternalProperty_buitenRol",
        BuitenRol $ t "myContextWithShadowedExternalProperty_buitenRol"])
  test "getUnqualifiedRoleBinders (From the instance of a Rol of any kind, find the instances of the Rol with the given local name that bind it (that have it as their binding). The type of ln can be buitenRolBeschrijving)"
    do
      assertEqual "The buitenRol of t:myContextWithExternalPropertyPrototype is bound to two Contexts that have declared it as their prototype"
        ((buitenRol /-/ getUnqualifiedRoleBinders "buitenRolBeschrijving") (t "myContextWithExternalPropertyPrototype"))
        ([BuitenRol $ t "myContextWithExternalProperty_buitenRol"
        , BuitenRol $ t "myContextWithShadowedExternalProperty_buitenRol"])
  test "getUnqualifiedPropertyDefinition" do
    assertEqual "myAspectRol1 has a local definition for the property myAspectRol1Property."
      (getUnqualifiedPropertyDefinition "myAspectRol1Property" (RolDef $ t "myAspect$myAspectRol1"))
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"]
  test "searchUnqualifiedPropertyDefinition" do
    assertEqual "myAspectRol1 obtains a definition for the property myUrAspectRol1Property from its AspectRol."
      (searchUnqualifiedPropertyDefinition "myUrAspectRol1Property" (RolDef $ t "myAspect$myAspectRol1"))
      [PropertyDef $ t "myUrAspect$myUrAspectRol1$myUrAspectRol1Property"]

  -- testOnly "Add a test!" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Tearing down" do
    unLoadTestModel "model:TestOGC"
  pure unit
