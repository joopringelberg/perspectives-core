module Test.Perspectives.ModelBasedTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (type (**>))
import Perspectives.DataTypeTripleGetters (binding, identity, rolType)
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijving)
import Perspectives.ModelBasedStringTripleGetters (hasOnEachRolTelescopeTheContextTypeOf)
import Perspectives.ModelBasedTripleGetters (buitenRolBeschrijvingDef, collectUnqualifiedPropertyDefinitions, contextBot, expressionType, getFunctionResultType, hasType, isContextTypeOf, isOrHasAspect, isRolTypeOf, mandatoryProperties, mogelijkeBinding, nonQueryRollen, ownPropertiesDef, propertiesDef, rollenDef, sumToSequence, botActiesInContext)
import Perspectives.PerspectivesTypes (ContextDef(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..))
import Perspectives.QueryCombinators (contains, ignoreCache)
import Perspectives.RunMonadPerspectivesQuery ((##=), (##>>))
import Perspectives.TripleGetterComposition (before, followedBy, (>->), (<<-<))
import Perspectives.TripleGetterConstructors (agreesWithType, closureOfAspect, closure_, count, directAspects, getRolInContext, searchUnqualifiedPropertyDefinition, searchUnqualifiedRol)
import Test.Perspectives.Utils (assertEqual, loadTestModel, p, runP, unLoadTestModel, q)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

tba :: String -> String
tba s = "model:TestBotActie$" <> s

theSuite :: Free TestF Unit
theSuite = suiteSkip "ModelBasedTripleGetters" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "TestOGC.crl"
  ---------------------------------------------------------------------------------
  test "rollenDef" do
    assertEqual "The Context 'psp:Property' defines a number of roles."
      ((p "Property") ##= rollenDef)
      -- []
      [ RolDef (p "Property$range")
      , RolDef (p "Property$aspectProperty")
      , RolDef (p "Property$bindingProperty")
      , RolDef "model:Perspectives$Property$binnenRolBeschrijving"
      , RolDef "model:Perspectives$Property$buitenRolBeschrijving"
      , RolDef "model:Perspectives$ContextPrototype$buitenRolBeschrijving"
      , RolDef "model:Perspectives$ContextPrototype$binnenRolBeschrijving"]
    assertEqual "myContextDef defines 16 roles"
      ((t "myContextDef") ##= count rollenDef)
      ([16])
    assertEqual "q:ComputedRolGetter has no defined Roles through Aspects."
      (q "ComputedRolGetter" ##= closureOfAspect >-> rollenDef)
      (RolDef <$>
      [ p "Function$result"
      , p "ContextPrototype$buitenRolBeschrijving"
      , p "ContextPrototype$binnenRolBeschrijving"])
    assertEqual "psp:TrustedCluster does have ???."
      (p "TrustedCluster" ##= closureOfAspect >-> rollenDef)
      []
    assertEqual "psp:TrustedCluster does have 3 roles."
      (p "TrustedCluster" ##= closure_ directAspects >-> rollenDef)
      [ RolDef $ p "TrustedCluster$clusterGenoot"
      , RolDef $ p "TrustedCluster$binnenRolBeschrijving"
      , RolDef $ p "TrustedCluster$buitenRolBeschrijving"
      , RolDef "model:Perspectives$ContextPrototype$buitenRolBeschrijving"
      , RolDef "model:Perspectives$ContextPrototype$binnenRolBeschrijving"]
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
    assertEqual "t:myContextDef6$buitenRolBeschrijving has no $mogelijkeBinding through its prototype (The RolDef has no prototype)"
      ((RolDef $ t "myContextDef6$buitenRolBeschrijving") ##= mogelijkeBinding)
      []
    assertEqual "t:myContextDef6$rol1 does have a value for mogelijkeBinding"
      ((RolDef $ t "myContextDef6$rol1") ##= mogelijkeBinding)
      [t "myContextDef5$rol1"]
  test "contextBot" do
    assertEqual "t:myContext6 has a contextBot"
      ((ContextDef $ t "myContext6") ##= contextBot)
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
    assertEqual "psp:SimpleValue should have type psp:Context"
      (p "SimpleValue" ##= hasType (p "Context"))
      [PBool "true"]

  test "sumToSequence" do
    assertEqual "sumToSequence of t:myContextDef is t:myContextDef"
      (t "myContextDef" ##= sumToSequence)
      [t "myContextDef"]
    assertEqual "sumToSequence of t:myContextDef5$rol1$mySum is [psp:Rol, psp:Property]"
      (t "myContextDef5$rol1$mySum" ##= sumToSequence)
      [p "Rol", p "Property"]
  test "isRolTypeOf" do
    r <- runP (t "myContextPrototype" ##>> searchUnqualifiedRol "rol1")
    assertEqual "$rol1 of t:myContextPrototype has type t:myContextDef$rol1"
      ((t "myContextDef$rol1") ##= (isRolTypeOf r))
      [PBool "true"]
    assertEqual "$rol1 of t:myContextPrototype has type t:myAspect$myAspectRol1"
      ((t "myAspect$myAspectRol1") ##= (isRolTypeOf r))
      [PBool "true"]
  test "isContextTypeOf" do
    assertEqual "ActieAspect `isContextTypeOf` RaadpleegtClusterGenoot is false"
      (p "ActieAspect" ##= (isContextTypeOf (p "TrustedCluster$RaadpleegtClusterGenoot")))
      [PBool "false"]
    assertEqual "RaadpleegtClusterGenoot `isOrHasAspect` ActieAspect is true"
      ((p "TrustedCluster$RaadpleegtClusterGenoot") ##= (isOrHasAspect (p "ActieAspect")))
      [PBool "true"]
  test "collectUnqualifiedPropertyDefinitions" do
    assertEqual "t:myContextDef9$rol1 does not by itself a property defined."
      ((RolDef $ t "myContextDef9$rol1") ##= searchUnqualifiedPropertyDefinition "rol1Property")
      []
    assertEqual "t:myContextDef5$rol1 should have a property 'rol1Property' by virtue of its Aspects"
      ((RolDef $ t "myContextDef5$rol1") ##= ignoreCache (collectUnqualifiedPropertyDefinitions "rol1Property"))
      [PropertyDef $ t "myContextDef4$rol1$rol1Property"]
    assertEqual "t:myContextDef6$rol1 should have a value for mogelijkeBinding"
      ((RolDef $ t "myContextDef6$rol1") ##= (mogelijkeBinding >-> sumToSequence `followedBy` RolDef))
      [RolDef $ t "myContextDef5$rol1"]
    assertEqual "lazyIntersectionOfTripleObjects should pass on the values of its left argument to its right argument."
      ((RolDef $ t "myContextDef6$rol1") ##= ((mogelijkeBinding >-> sumToSequence `followedBy` RolDef) <<-< (\_ -> identity) $ "identity"))
      [RolDef $ t "myContextDef5$rol1"]
    assertEqual "lazyIntersectionOfTripleObjects should pass on the values of its left argument to its right argument."
      ((RolDef $ t "myContextDef6$rol1") ##= ((mogelijkeBinding >-> sumToSequence `followedBy` RolDef) <<-< (\_ -> identity) $ "identity"))
      [RolDef $ t "myContextDef5$rol1"]
    assertEqual "t:myContextDef6$rol1 should have a property 'rol1Property' by virtue of its mogelijkeBinding graph and Aspects of the bottom of the graph"
      ((RolDef $ t "myContextDef6$rol1") ##= collectUnqualifiedPropertyDefinitions "rol1Property")
      [PropertyDef $ t "myContextDef4$rol1$rol1Property"]
    assertEqual "t:myContextDef9$rol1 should have a property 'rol1Property' by virtue of its mogelijkeBinding graph and Aspects of the bottom of the graph"
      ((RolDef $ t "myContextDef9$rol1") ##= collectUnqualifiedPropertyDefinitions "rol1Property")
      [PropertyDef $ t "myContextDef4$rol1$rol1Property"]

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
      (t2 "myContextDef2" ##= count rollenDef)
      ([15])
  test "isNotAQuery" do
    assertEqual "t:myContextDef2$rol1 is not a query-rol"
      ((RolDef $ t2 "myContextDef2$rol1") ##= isNotAQuery)
      [PBool "false"]
  test "nonQueryRollen" do
    assertEqual "myContextDef2 defines a single non-query rol and inherits many from Context"
      (t2 "myContextDef2" ##= count nonQueryRollen)
      ([15])
  test "mandatoryProperties" do
    assertEqual "psp:Rol has a single mandatory external property."
      ((p "Rol") ##= buitenRolBeschrijvingDef >-> mandatoryProperties)
      [PropertyDef $ p "Rol$buitenRolBeschrijving$isFunctioneel"]
    assertEqual "t:myContextDef2 has a mandatory external property $contextDef2ExtProp1."
      ((t2 "myContextDef2") ##= buitenRolBeschrijvingDef >-> mandatoryProperties)
      [PropertyDef $ t2 "myContextDef2$buitenRolBeschrijving$contextDef2ExtProp1"]

  -- testOnly "" do
  --   loadTestModel "testTypeDefChecker.crl"
  --
  --   unLoadTestModel "model:TestTDC"

  test "Unloading testTypeDefChecker" do
    unLoadTestModel "model:TestTDC"

  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "perspectives.crl"
  ---------------------------------------------------------------------------------
  test "getFunctionResultType" do
    assertEqual "The function result type of psp:PerspectivesSysteem$modellen is psp:Context"
      ((p "PerspectivesSysteem$modellen") ##= getFunctionResultType)
      [p "Context"]

  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "testBotActie.crl"
  ---------------------------------------------------------------------------------
  test "Loading testBotActie.crl" do
    loadTestModel "testBotActie.crl"
  test "expressionType" do
    assertEqual "tba:Test$botCopiesV1ToV2$self is a Function"
      (("model:TestBotActie$Test$botCopiesV1ToV2$self") ##= (hasType "model:Perspectives$Function"))
      [PBool "true"]
    assertEqual "tba:Test$botCopiesV1ToV2$self expressionType psp:Actie"
      (("model:TestBotActie$Test$botCopiesV1ToV2$self") ##= expressionType)
      [p "Zaak"]
    assertEqual "tba:Test$botCopiesV1ToV2$self has no value for getFunctionResultType"
      (("model:TestBotActie$Test$botCopiesV1ToV2$self") ##= getFunctionResultType)
      ["model:TestBotActie$Test"]

  test "checkBindingOfRolInContext" do
    assertEqual "de mogelijkeBinding van de gebruikerRol van tba:Test is psp:PerspectivesSysteem$gebruiker"
      ((tba "Test") ##= getRolInContext (RolDef $ p "Context$gebruikerRol") >-> rolType >-> mogelijkeBinding >-> sumToSequence)
      [p "PerspectivesSysteem$gebruiker"]
    assertEqual "PerspectivesSysteem$gebruiker `isContextTypeOf` usr:MijnSysteem$gebruiker(1)"
      do
        allowedBinding <- pure (p "PerspectivesSysteem$gebruiker")
        boundValue <- ((tba "Test") ##>> getRolInContext (RolDef $ p "Context$gebruikerRol") >-> binding)
        (allowedBinding ##= isRolTypeOf boundValue)
      [PBool "true"]
    assertEqual "PerspectivesSysteem$gebruiker `hasOnEachRolTelescopeTheContextTypeOf` usr:MijnSysteem$gebruiker(1)"
      do
        allowedBinding <- pure (p "PerspectivesSysteem$gebruiker")
        boundValue <- ((tba "Test") ##>> getRolInContext (RolDef $ p "Context$gebruikerRol") >-> binding)
        (allowedBinding ##= hasOnEachRolTelescopeTheContextTypeOf (unwrap boundValue))
      [PBool "true"]

  testOnly "botActiesInContext" do
    loadTestModel "testBotActie.crl"
    assertEqual "tba:Test has a single action for the contextBot."
      (ContextDef (tba "Test") ##= botActiesInContext) 
      [ContextDef $ tba "Test$botCopiesV1ToV2"]
    unLoadTestModel "model:TestBotActie"

  test "Tearing down" do
    unLoadTestModel "model:TestBotActie"

-- part of the definition of nonQueryRollen, we need it here to test it seperately.
-- returns true iff the inclusive closure of aspectRol of the RolDef contains psp:Rol.
isNotAQuery :: forall e. (RolDef **> PBool)
isNotAQuery = contains (RolDef "model:Perspectives$Rol") (unwrap `before` (closure_ directAspects) `followedBy` RolDef)
