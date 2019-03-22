module Test.Perspectives.ModelBasedTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (TypedTripleGetter, type (**>))
import Perspectives.ModelBasedTripleGetters (buitenRolBeschrijvingDef, mogelijkeBinding, nonQueryRollen, ownPropertiesDef, propertiesDef, rollenDef)
import Perspectives.PerspectivesTypes (PBool(..), PropertyDef(..), RolDef(..))
import Perspectives.QueryCombinators (contains)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterComposition (before, followedBy)
import Perspectives.TripleGetterConstructors (closure_, directAspects)
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "ModelBasedTripleGetters" do
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
      [PropertyDef $ t "myAspect$myAspectRol1$myAspectRol1Property",
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
  test "nonQueryRollen" do
    assertEqual "myContextDef2 defines a single non-query rol"
      (t2 "myContextDef2" ##= rollenDef)
      [RolDef $ t2 "myContextDef2$rol1"]
    assertEqual "t:myContextDef2$rol1 is not a query-rol"
      (RolDef $ t2 "myContextDef2$rol1" ##= isNotAQuery)
      [PBool "false"]
    assertEqual "myContextDef2 defines a single non-query rol"
      (t2 "myContextDef2" ##= nonQueryRollen)
      [RolDef $ p "Rol"]

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
