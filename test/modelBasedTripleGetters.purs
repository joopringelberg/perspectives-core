module Test.Perspectives.ModelBasedTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (TypedTripleGetter, type (**>))
import Perspectives.ModelBasedTripleGetters (agreesWithType, buitenRolBeschrijvingDef, contextBot, hasType, isOrHasAspect, mogelijkeBinding, nonQueryRollen, ownPropertiesDef, propertiesDef, rollenDef, sumToSequence)
import Perspectives.PerspectivesTypes (ContextDef(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..))
import Perspectives.QueryCombinators (contains)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterComposition (before, followedBy)
import Perspectives.TripleGetterConstructors (closure_, directAspects)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suite "ModelBasedTripleGetters" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "TestOGC.crl"
  ---------------------------------------------------------------------------------
  test "rollenDef" do
    assertEqual "The Context 'psp:Property' defines three roles."
      ((p "Property") ##= rollenDef)
      -- []
      [ RolDef (p "Property$range")
      , RolDef (p "Property$aspectProperty")
      , RolDef (p "Property$bindingProperty")]
    assertEqual "myContextDef defines three roles"
      ((t "myContextDef") ##= rollenDef)
      (map RolDef ["model:TestOGC$myContextDef$rol1","model:TestOGC$myAspect$myAspectRol1","model:TestOGC$myAspect$myAspectRol2","model:TestOGC$myUrAspect$myUrAspectRol1","model:Perspectives$Context$binnenRolBeschrijving","model:Perspectives$Context$buitenRolBeschrijving","model:Perspectives$Context$rolInContext","model:Perspectives$Context$interneView","model:Perspectives$Context$externeView","model:Perspectives$Context$prototype","model:Perspectives$Context$aspect","model:Perspectives$Context$gebruikerRol","model:Perspectives$Context$contextBot"])
  test "ownPropertiesDef" do
    assertEqual "De roldefinitie t:myAspect$myAspectRol1 definieert de property $myAspectRol1Property."
      (RolDef (t "myAspect$myAspectRol1") ##= ownPropertiesDef)
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property"]
  test "propertiesDef" do
    assertEqual "De roldefinitie t:myContextDef$rol1 ontleent property $myAspectRol1Property aan zijn aspectRol."
      (RolDef (t "myContextDef$rol1") ##= propertiesDef)
      [ PropertyDef $ t "myContextDef$rol1$rol1Property",
      PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property",
      PropertyDef $ t "myUrAspect$myUrAspectRol1$myUrAspectRol1Property"]
  test "buitenRolBeschrijvingDef" do
    assertEqual "From a context that is a definition, get the definition of its BuitenRol."
      ((t "myContextDef") ##= buitenRolBeschrijvingDef)
      [RolDef $ p "ContextPrototype$buitenRolBeschrijving"]
    assertEqual "Found through three layers."
      ((t "myContextDef3") ##= buitenRolBeschrijvingDef)
      [RolDef $ p "ContextPrototype$buitenRolBeschrijving"]
  test "mogelijkeBinding" do
    assertEqual "$myAspectRol1 has mogelijkeBinding psp:Rol through its $aspectRol"
      ((RolDef $ t "myAspect$myAspectRol1") ##= mogelijkeBinding)
      [p "Rol"]
  test "contextBot" do
    assertEqual "t:myContext6 has a contextBot"
      (ContextDef $ t "myContext6" ##= contextBot)
      [RolInContext $ t "myContext6$contextBot_1"]
  test "agreesWithType" do
    assertEqual "t:myContextDef agrees with type t:myContextDef"
      (t "myContextDef" ##= agreesWithType (t "myContextDef"))
      [PBool "true"]
    assertEqual "t:myContextDef does not agree with type psp:Property"
      (t "myContextDef" ##= agreesWithType (p "Property"))
      [PBool "false"]
    assertEqual "t:myContextDef agrees with type psp:ElkType"
      (t "myContextDef" ##= agreesWithType (p "ElkType"))
      [PBool "true"]
    assertEqual "t:myContextDef does not agree with type psp:Niets"
      (t "myContextDef" ##= agreesWithType (p "Niets"))
      [PBool "false"]
  test "isOrHasAspect" do
    assertEqual "t:myContextDef is or has aspect psp:Context"
      (t "myContextDef" ##= isOrHasAspect (p "Context"))
      [PBool "true"]
    assertEqual "t:myContextDef is or has aspect t:myAspect"
      (t "myContextDef" ##= isOrHasAspect (t "myAspect"))
      [PBool "true"]
    assertEqual "t:myContextDef is or has aspect t:myUrAspect"
      (t "myContextDef" ##= isOrHasAspect (t "myUrAspect"))
      [PBool "true"]
    assertEqual "t:myContextDef is not nor has aspect psp:Property"
      (t "myContextDef" ##= isOrHasAspect (p "Property"))
      [PBool "false"]
  test "hasType" do
    assertEqual "t:myContextPrototype has type psp:Context"
      (t "myContextPrototype" ##= hasType (p "Context"))
      [PBool "true"]
    assertEqual "t:myContextPrototype has type t:myAspect"
      (t "myContextPrototype" ##= hasType (t "myAspect"))
      [PBool "true"]
    assertEqual "t:myContextPrototype has type t:myUrAspect"
      (t "myContextPrototype" ##= hasType (t "myUrAspect"))
      [PBool "true"]
    assertEqual "t:myContextPrototype does not have type psp:Property"
      (t "myContextPrototype" ##= hasType (p "Property"))
      [PBool "false"]
  test "sumToSequence" do
    assertEqual "sumToSequence of t:myContextDef is t:myContextDef"
      (t "myContextDef" ##= sumToSequence)
      [t "myContextDef"]
    assertEqual "sumToSequence of t:myContextDef5$rol1$mySum is [psp:Rol, psp:Property]"
      (t "myContextDef5$rol1$mySum" ##= sumToSequence)
      [p "Rol", p "Property"]

  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Changing testfile" do
    unLoadTestModel "model:TestOGC"
    loadTestModel "testTypeDefChecker.crl"

  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "testTypeDefChecker.crl"
  ---------------------------------------------------------------------------------
  test "rollenDef" do
    assertEqual "myContextDef2 defines a single rol and inherits many from Context"
      (t2 "myContextDef2" ##= rollenDef)
      (RolDef <$> ["model:TestTDC$myContextDef2$rol1","model:Perspectives$Context$binnenRolBeschrijving","model:Perspectives$Context$buitenRolBeschrijving","model:Perspectives$Context$rolInContext","model:Perspectives$Context$interneView","model:Perspectives$Context$externeView","model:Perspectives$Context$prototype","model:Perspectives$Context$aspect","model:Perspectives$Context$gebruikerRol","model:Perspectives$Context$contextBot"])
  test "isNotAQuery" do
    assertEqual "t:myContextDef2$rol1 is not a query-rol"
      (RolDef $ t2 "myContextDef2$rol1" ##= isNotAQuery)
      [PBool "false"]
  test "nonQueryRollen" do
    assertEqual "myContextDef2 defines a single non-query rol and inherits many from Context"
      (t2 "myContextDef2" ##= nonQueryRollen)
      (RolDef <$> ["model:TestTDC$myContextDef2$rol1","model:Perspectives$Context$binnenRolBeschrijving","model:Perspectives$Context$buitenRolBeschrijving","model:Perspectives$Context$rolInContext","model:Perspectives$Context$interneView","model:Perspectives$Context$externeView","model:Perspectives$Context$prototype","model:Perspectives$Context$aspect","model:Perspectives$Context$gebruikerRol","model:Perspectives$Context$contextBot"])

  -- testOnly "" do
  --   loadTestModel "testTypeDefChecker.crl"
  --
  --   unLoadTestModel "model:TestTDC"

  test "Tearing down" do
    unLoadTestModel "model:TestTDC"

-- part of the definition of nonQueryRollen, we need it here to test it seperately.
-- returns true iff the inclusive closure of aspectRol of the RolDef contains psp:Rol.
isNotAQuery :: forall e. (RolDef **> PBool) e
isNotAQuery = contains (RolDef "model:Perspectives$Rol") (unwrap `before` (closure_ directAspects) `followedBy` RolDef)
