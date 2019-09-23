module Test.Perspectives.ModelBasedTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (type (**>))
import Perspectives.DataTypeTripleGetters (binding, identity, rolType)
import Perspectives.ModelBasedStringTripleGetters (hasContextTypeOnEachRolTelescopeOf)
import Perspectives.ModelBasedTripleGetters (botActiesInContext, buitenRolBeschrijvingDef, canBeBoundToRolType, collectUnqualifiedPropertyDefinitions, contextBot, contextInstanceCanBeBoundToRolType, effectiveRolType, equalsOrIsAspectOf, expressionType, getFunctionResultType, hasContextType, hasRolType, isAspectOf, isContextTypeOf, isOrHasAspect, isRolTypeOf, isTypeOfRolOnTelescopeOf, mandatoryProperties, mogelijkeBinding, nonQueryRollen, ownPropertiesDef, propertiesDef, propertyReferenties, rollenDef, sumToSequence)
import Perspectives.PerspectivesTypes (ContextDef(..), ContextRol(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..))
import Perspectives.QueryCombinators (containedIn, ignoreCache)
import Perspectives.RunMonadPerspectivesQuery ((##=), (##>>))
import Perspectives.TripleGetterComposition (before, followedBy, (>->), (<<-<))
import Perspectives.TripleGetterConstructors (agreesWithType, closureOfAspect, closure_, count, directAspects, getRolInContext, searchUnqualifiedPropertyDefinition, searchUnqualifiedRol, some)
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
    assertEqual "myContextDef defines 14 roles"
      ((t "myContextDef") ##= count rollenDef)
      ([14])
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
      -- (PropertyDef <$>
      --   [ "model:TestOGC$myContextDef$rol1$rol1Property"
      --   , "model:TestOGC$myAspect$myAspectRol1$myAspectRol1Property"
      --   , "model:TestOGC$myUrAspect$myUrAspectRol1$myUrAspectRol1Property"
      --   , "model:Perspectives$Rol$buitenRolBeschrijving$isFunctioneel"
      --   , "model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht"])
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
      [RolInContext $ t "myContext6$contextBot_0001"]
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
  test "equalsOrIsAspectOf" do
    assertEqual "t:myContextDef is or has aspect psp:Context"
      (t "myContextDef" ##= equalsOrIsAspectOf (p "Context"))
      -- equalsOrIsAspectOf (t "myContextDef") (p "Context")
      [PBool "true"]
    assertEqual "t:myContextDef is or has aspect t:myAspect"
      (t "myContextDef" ##= equalsOrIsAspectOf (t "myAspect"))
      [PBool "true"]
    assertEqual "t:myContextDef is or has aspect t:myUrAspect"
      (t "myContextDef" ##= equalsOrIsAspectOf (t "myUrAspect"))
      [PBool "true"]
    assertEqual "t:myContextDef is not nor has aspect psp:Property"
      (t "myContextDef" ##= equalsOrIsAspectOf (p "Property"))
      [PBool "false"]

  test "hasContextType" do
    assertEqual "t:myContextPrototype has type psp:Context"
      (p "Context" ##= hasContextType (t "myContextPrototype"))
      [PBool "true"]
    assertEqual "t:myContextPrototype has type t:myAspect"
      (t "myAspect" ##= hasContextType (t "myContextPrototype"))
      [PBool "true"]
    assertEqual "t:myContextPrototype has type t:myUrAspect"
      (t "myUrAspect" ##= hasContextType (t "myContextPrototype"))
      [PBool "true"]
    assertEqual "t:myContextPrototype does not have type psp:Property"
      (p "Property" ##= hasContextType (t "myContextPrototype"))
      [PBool "false"]
    assertEqual "psp:SimpleValue should have type psp:Context"
      (p "Context" ##= hasContextType (p "SimpleValue"))
      [PBool "true"]
    assertEqual "psp:PerspectivesSysteem should have type psp:Context"
      (p "Context" ##= hasContextType (p "PerspectivesSysteem"))
      [PBool "true"]

  test "isContextTypeOf" do
    assertEqual "ActieAspect `isContextTypeOf` RaadpleegtClusterGenoot is false"
      (p "TrustedCluster$RaadpleegtClusterGenoot" ##= (isContextTypeOf (p "ActieAspect")))
      [PBool "false"]
    assertEqual "RaadpleegtClusterGenoot `equalsOrIsAspectOf` ActieAspect is true"
      ((p "TrustedCluster$RaadpleegtClusterGenoot") ##= (equalsOrIsAspectOf (p "ActieAspect")))
      [PBool "true"]
    assertEqual "psp:Context is a type of psp:PerspectivesSysteem"
      (p "PerspectivesSysteem" ##= isContextTypeOf (p "Context"))
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
      (unwrap r ##= (isRolTypeOf $ (t "myContextDef$rol1")))
      [PBool "true"]
    assertEqual "$rol1 of t:myContextPrototype has type t:myAspect$myAspectRol1"
      (unwrap r ##= (isRolTypeOf (t "myAspect$myAspectRol1")))
      [PBool "true"]
  test "hasRolType" do
    r <- runP (t "myContextPrototype" ##>> searchUnqualifiedRol "rol1")
    assertEqual "$rol1 of t:myContextPrototype has type t:myContextDef$rol1"
      ((t "myContextDef$rol1") ##= (hasRolType $ unwrap r))
      [PBool "true"]
    assertEqual "$rol1 of t:myContextPrototype has type t:myAspect$myAspectRol1"
      ((t "myAspect$myAspectRol1") ##= (hasRolType $ unwrap r))
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
    assertEqual "myContextDef2 has a total of 13 roles."
      (t2 "myContextDef2" ##= count rollenDef)
      ([13])
  test "isNotAQuery" do
    assertEqual "t:myContextDef2$rol1 is not a query-rol"
      ((RolDef $ t2 "myContextDef2$rol1") ##= isNotAQuery)
      [PBool "false"]
  test "nonQueryRollen" do
    assertEqual "myContextDef2 has a total of 13 non-query roles."
      (t2 "myContextDef2" ##= count nonQueryRollen)
      ([13])
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
      [p "Model"]

  test "propertyReferenties" do
    assertEqual "psp:PerspectivesSysteem$gebruiker$VolledigeNaam has two roles $propertyReferentie"
      (((RolDef $ p "PerspectivesSysteem$gebruiker$VolledigeNaam") ##= propertyReferenties) >>= (pure <<< length))
      2

  test "effectiveRolType" do
    assertEqual "psp:PerspectivesSysteem$modellen should have as effective roltype the type of psp:Model"
      ((p "PerspectivesSysteem$modellen") ##= effectiveRolType)
      ["model:Perspectives$Model"]
    assertEqual "psp:PerspectivesSysteem$modelsInUse should have as effective roltype the type of psp:PerspectivesSysteem$modelsInUse"
      ((p "PerspectivesSysteem$modelsInUse") ##= effectiveRolType)
      ["model:Perspectives$PerspectivesSysteem$modelsInUse"]

  test "isAspectOf" do
    assertEqual "psp:Context is an aspect of psp:Context"
      ((p "Context") ##= isAspectOf (p "Context"))
      [PBool "false"]
    assertEqual "psp:Context is an aspect of psp:Systeem"
      ((p "Systeem") ##= isAspectOf (p "Context"))
      [PBool "true"]
    assertEqual "psp:Property is not an aspect of psp:Systeem"
      ((p "Systeem") ##= isAspectOf (p "Property"))
      [PBool "false"]

  test "equalsOrIsAspectOf" do
    assertEqual "psp:Context is an aspect of psp:Context"
      (p "Context" ##= equalsOrIsAspectOf (p "Context"))
      [PBool "true"]
    assertEqual "psp:Context is an aspect of psp:Systeem"
      ((p "Systeem") ##= equalsOrIsAspectOf (p "Context"))
      [PBool "true"]
    assertEqual "psp:Property is not an aspect of psp:Systeem"
      ((p "Systeem") ##= equalsOrIsAspectOf (p "Property"))
      [PBool "false"]

  test "isContextTypeOf" do
    assertEqual "psp:Context is a (context) type of psp:Systeem"
      ((p "Systeem") ##= isContextTypeOf (p "Context"))
      [PBool "true"]
    assertEqual "psp:PerspectivesSysteem is not a (context) type of psp:Systeem"
      ((p "Systeem") ##= isContextTypeOf (p "PerspectivesSysteem"))
      [PBool "false"]
    assertEqual "psp:Systeem is a (context) type of psp:PerspectivesSysteem"
      ((p "PerspectivesSysteem") ##= isContextTypeOf (p "Systeem"))
      [PBool "true"]
    assertEqual "psp:Systeem is not a (context) type of psp:Property"
      ((p "Property") ##= isContextTypeOf (p "Systeem"))
      [PBool "false"]
    -- deze tests falen omdat het effectieve roltype van 'modellen' psp:Model is.
    -- Dat is geen instantie van q:ComputedRolGetter of psp:Function.
    -- assertEqual "q:ComputedRolGetter is a (context) type of psp:PerspectivesSysteem$modellen."
    --   ((p "PerspectivesSysteem$modellen") ##= (isContextTypeOf (q "ComputedRolGetter")))
    --   [PBool "true"]
    -- assertEqual "psp:Function is a (context) type of psp:PerspectivesSysteem$modellen."
    --   ((p "PerspectivesSysteem$modellen") ##= (isContextTypeOf "model:Perspectives$Function"))
    --   [PBool "true"]


  test "hasContextType" do
    assertEqual "psp:Context is a (context) type of psp:Systeem"
      (p "Context" ##= hasContextType (p "Systeem"))
      [PBool "true"]
    assertEqual "psp:Systeem is not a (context) type of psp:Context"
      (p "PerspectivesSysteem" ##= hasContextType (p "Systeem"))
      [PBool "false"]
    assertEqual "psp:Context is a (context) type of psp:PerspectivesSysteem"
      ((p "Systeem") ##= hasContextType (p "PerspectivesSysteem"))
      [PBool "true"]
    assertEqual "psp:Property is not a (context) type of psp:Systeem"
      ((p "Systeem") ##= hasContextType (p "Property"))
      [PBool "false"]

  test "isTypeOfRolOnTelescopeOf" do
    assertEqual "The rolinstance $binnenRolBeschrijving of psp:Property has type psp:Context$binnenRolBeschrijving"
      do
        -- r = "model:Perspectives$Property$binnenRolBeschrijving_0001"
        r <- p "Property" ##>> searchUnqualifiedRol "binnenRolBeschrijving"
        unwrap r ##= isTypeOfRolOnTelescopeOf (p "Context$binnenRolBeschrijving")
      [PBool "true"]
    assertEqual "The rolinstance $binnenRolBeschrijving of psp:Property has type psp:Rol$buitenRolBeschrijving"
      do
        -- r = "model:Perspectives$Property$binnenRolBeschrijving_0001"
        -- binding r = "model:Perspectives$Property$binnenRolBeschrijving_buitenRol"
        r <- p "Property" ##>> searchUnqualifiedRol "binnenRolBeschrijving"
        unwrap r ##= isTypeOfRolOnTelescopeOf (p "Rol$buitenRolBeschrijving")
      [PBool "true"]
    assertEqual "The rolinstance $binnenRolBeschrijving of psp:Property does not have type psp:Property$range"
      do
        r <- p "Property" ##>> searchUnqualifiedRol "binnenRolBeschrijving"
        unwrap r ##= isTypeOfRolOnTelescopeOf (p "Property$range")
      [PBool "false"]
    assertEqual "The rolinstance $binnenRolBeschrijving of psp:ContextPrototype does have type psp:Property$range"
      do
        r <- p "Property" ##>> searchUnqualifiedRol "binnenRolBeschrijving"
        unwrap r ##= isTypeOfRolOnTelescopeOf (p "Property$range")
      [PBool "false"]

  test "contextInstanceCanBeBoundToRolType" do
    assertEqual "psp:ContextPrototype$binnenRolBeschrijving `contextInstanceCanBeBoundToRolType` psp:Context$binnenRolBeschrijving"
      ((RolDef $ p "Context$binnenRolBeschrijving") ##= contextInstanceCanBeBoundToRolType (p "ContextPrototype$binnenRolBeschrijving"))
      [PBool "true"]
    assertEqual "psp:PerspectivesSysteem$modelsInUse `contextInstanceCanBeBoundToRolType` psp:PerspectivesSysteem$Context$rolInContext"
      do
        rt <- ContextRol "model:Perspectives$PerspectivesSysteem$Context$rolInContext_0006" ##>> rolType
        (rt ##= contextInstanceCanBeBoundToRolType (p "PerspectivesSysteem$modelsInUse"))
      [PBool "true"]
    assertEqual "De mogelijkeBinding van het type van model:Perspectives$PerspectivesSysteem$Context$rolInContext_0006 is een Context type."
      do
        rt <- ContextRol "model:Perspectives$PerspectivesSysteem$Context$rolInContext_0006" ##>> rolType
        (rt ##= some (mogelijkeBinding >-> sumToSequence >-> (equalsOrIsAspectOf "model:Perspectives$Context")))
      [PBool "true"]
    assertEqual "psp:PerspectivesSysteem$buitenRolBeschrijving `contextInstanceCanBeBoundToRolType` rolType psp:PerspectivesSysteem$Context$buitenRolBeschrijving_0001"
      do
        rt <- ContextRol "model:Perspectives$PerspectivesSysteem$Context$buitenRolBeschrijving_0001" ##>> rolType
        (rt ##= contextInstanceCanBeBoundToRolType "model:Perspectives$PerspectivesSysteem$buitenRolBeschrijving")
      [PBool "true"]
    assertEqual "De mogelijkeBinding van het type van model:Perspectives$PerspectivesSysteem$Context$buitenRolBeschrijving_0001 is een Context type."
      do
        rt <- ContextRol "model:Perspectives$PerspectivesSysteem$Context$buitenRolBeschrijving_0001" ##>> rolType
        -- rt = model:Perspectives$Context$buitenRolBeschrijving
        (rt ##= some (mogelijkeBinding >-> sumToSequence >-> (equalsOrIsAspectOf "model:Perspectives$Context")))
      [PBool "true"]

  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "testBotActie.crl"
  ---------------------------------------------------------------------------------
  test "Loading testBotActie.crl" do
    loadTestModel "testBotActie.crl"
  test "expressionType" do
    assertEqual "The expression type of tba:Test$botCopiesV1ToV2$self is a Function"
      ("model:TestBotActie$Test$botCopiesV1ToV2$self" ##= expressionType)
      [tba "Test"]
    assertEqual "tba:Test$botCopiesV1ToV2$self a function result type of tba:Test"
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
        (allowedBinding ##= hasRolType (unwrap boundValue))
      [PBool "true"]
    assertEqual "PerspectivesSysteem$gebruiker `hasContextTypeOnEachRolTelescopeOf` usr:MijnSysteem$gebruiker(1)"
      do
        allowedBinding <- pure (p "PerspectivesSysteem$gebruiker")
        boundValue <- ((tba "Test") ##>> getRolInContext (RolDef $ p "Context$gebruikerRol") >-> binding)
        (allowedBinding ##= hasContextTypeOnEachRolTelescopeOf (unwrap boundValue))
      [PBool "true"]

  test "botActiesInContext" do
    loadTestModel "testBotActie.crl"
    assertEqual "tba:Test has a single action for the contextBot."
      (ContextDef (tba "Test") ##= botActiesInContext)
      [ContextDef $ tba "Test$botCopiesV1ToV2"]
    -- unLoadTestModel "model:TestBotActie"

  test "Tearing down" do
    unLoadTestModel "model:TestBotActie"

-- part of the definition of nonQueryRollen, we need it here to test it seperately.
-- returns true iff the inclusive closure of aspectRol of the RolDef contains psp:Rol.
isNotAQuery :: forall e. (RolDef **> PBool)
isNotAQuery = containedIn (RolDef "model:Perspectives$Rol") (unwrap `before` (closure_ directAspects) `followedBy` RolDef)
