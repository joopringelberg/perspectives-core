module Test.Perspectives.ModelBasedStringTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (unwrap)
import Perspectives.DataTypeTripleGetters (getUnqualifiedProperty, identity)
import Perspectives.Identifiers (buitenRol)
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijving)
import Perspectives.ModelBasedStringTripleGetters (buitenRolBeschrijvingDef) as MBST
import Perspectives.ModelBasedStringTripleGetters (hasContextTypeOnEachRolTelescopeOf, mogelijkeBinding, propertiesDef, searchView)
import Perspectives.ModelBasedTripleGetters (buitenRolBeschrijvingDef, hasContextType, isContextTypeOf, sumToSequence)
import Perspectives.PerspectivesTypes (BuitenRol(..), PBool(..))
import Perspectives.QueryCombinators (notEmpty, cond)
import Perspectives.QueryCompiler (getPropertyFunction)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterComposition (followedBy, (>->))
import Test.Perspectives.Utils (assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

theSuite :: Free TestF Unit
theSuite = suiteSkip "ModelBasedStringTripleGetters" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "TestOGC.crl"
  ---------------------------------------------------------------------------------
  test "hasContextType" do
    assertEqual "t:myContextPrototype has type psp:Context"
      (p "Context" ##= hasContextType (t "myContextPrototype"))
      [PBool "true"]
    assertEqual "t:myContextPrototype has type t:myAspect"
      ((t "myAspect") ##= hasContextType (t "myContextPrototype"))
      [PBool "true"]
    assertEqual "t:myContextPrototype has type t:myUrAspect"
      ((t "myUrAspect") ##= hasContextType (t "myContextPrototype"))
      [PBool "true"]
    assertEqual "t:myContextPrototype does not have type psp:Property"
      ((p "Property") ##= hasContextType (t "myContextPrototype"))
      [PBool "false"]

  test "hasContextTypeOnEachRolTelescopeOf (allowedBinding ## (`hasContextTypeOnEachRolTelescopeOf` t))" do
    assertEqual "psp:SimpleValue should have type psp:Context"
      (p "Context" ##= hasContextType (p "SimpleValue"))
      [PBool "true"]
    assertEqual "psp:SimpleValue should have type psp:Context"
      (p "Context" ##= hasContextTypeOnEachRolTelescopeOf (p "SimpleValue"))
      [PBool "true"]
    -- assertEqual "t:myContextDef6$rol1 is in its own rolTelescope."
    --   ((t "myContextDef6$rol1") ##= (hasContextTypeOnEachRolTelescopeOf (t "myContextDef6$rol1")))
    --   [PBool "true"]
    assertEqual "t:myContextDef6$rol1 does have a value for mogelijkeBinding"
      ((t "myContextDef6$rol1") ##= mogelijkeBinding)
      [t "myContextDef5$rol1"]
    assertEqual "t:myContextDef6$rol1 does have a value for mogelijkeBinding"
      ((t "myContextDef6$rol1") ##= (notEmpty (mogelijkeBinding >-> sumToSequence)))
      [PBool "true"]
    -- assertEqual "On each path through the mogelijkeBinding graph of myContextDef6$rol1 there is a type x for which holds: x isContextTypeOf myContextDef5$rol1"
    --   ((t "myContextDef6$rol1") ##= (hasContextTypeOnEachRolTelescopeOf (t "myContextDef5$rol1")))
    --   [PBool "true"]
    assertEqual "t:myContextDef$rol1 is NOT in each rolTelescope that starts with t:myContextDef6$rol1"
      ((t "myContextDef6$rol1") ##= (hasContextTypeOnEachRolTelescopeOf (t "myContextDef$rol1")))
      [PBool "false"]
    assertEqual "t:myContextDef5$rol1 is NOT in each rolTelescope that starts with t:myContextDef7$rol1"
      ((t "myContextDef7$rol1") ##= (hasContextTypeOnEachRolTelescopeOf (t "myContextDef5$rol1")))
      [PBool "false"]
    -- assertEqual "t:myContextDef5$rol1 is in each rolTelescope that starts with t:myContextDef9$rol1"
    --   ((t "myContextDef9$rol1") ##= (hasContextTypeOnEachRolTelescopeOf (t "myContextDef5$rol1")))
    --   [PBool "true"]
    -- assertEqual "psp:PerspectivesSysteem$gebruiker is in each rolTelescope that starts with psp:TrustedCluster$clusterGenoot"
    --   ((p "TrustedCluster$clusterGenoot") ##= (hasContextTypeOnEachRolTelescopeOf (p "PerspectivesSysteem$gebruiker")))
    --   [PBool "true"]

  testSkip "searchUnqualifiedRolDefinition" do
    assertEqual "model:Perspectives$PerspectivesSysteem$gebruiker has a view VolledigeNaam"
      ((p "PerspectivesSysteem$gebruiker") ##= searchView "VolledigeNaam")
      [p "PerspectivesSysteem$gebruiker$VolledigeNaam"]

  test "propertiesDef1" do
    assertEqual "psp:Systeem$modelsInUse heeft als effectieve mogelijkeBinding de beschrijving van de buitenrol van psp:Context"
      ((p "PerspectivesSysteem$modelsInUse") ##= (mogelijkeBinding >-> sumToSequence >-> cond (isContextTypeOf "model:Perspectives$Context") MBST.buitenRolBeschrijvingDef identity))
      ["model:Perspectives$ContextPrototype$buitenRolBeschrijving"]

  test "propertiesDef2" do
    assertEqual "psp:Context has an external property with local name 'contextLabel'."
      ((p "Context") ##= buitenRolBeschrijvingDef `followedBy` unwrap >-> propertiesDef)
      ["model:Perspectives$BuitenRolPrototype$contextLabel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isFunctioneel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht"]
    assertEqual "psp:Systeem has an external property with local name 'contextLabel'."
      ((p "Systeem") ##= buitenRolBeschrijvingDef `followedBy` unwrap >-> propertiesDef)
      ["model:Perspectives$BuitenRolPrototype$contextLabel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isFunctioneel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht"]
    assertEqual "psp:PerspectivesSysteem has an external property with local name 'contextLabel'."
      -- ((p "PerspectivesSysteem") ##= buitenRolBeschrijvingDef `followedBy` unwrap >-> propertiesDef)
      ("model:Perspectives$PerspectivesSysteem$buitenRolBeschrijving" ##= propertiesDef)
      [p "PerspectivesSysteem$buitenRolBeschrijving$modelOphaalTeller"
      , "model:Perspectives$BuitenRolPrototype$contextLabel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isFunctioneel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht"]
    assertEqual "psp:Model has an external property with local name 'contextLabel'."
      ((p "Model") ##= buitenRolBeschrijvingDef `followedBy` unwrap >-> propertiesDef)
      ["model:Perspectives$BuitenRolPrototype$contextLabel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isFunctioneel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht"]
    assertEqual "The mogelijkeBinding of psp:PerspectivesSysteem$modelsInUse is psp:Model."
      ((p "PerspectivesSysteem$modelsInUse") ##= mogelijkeBinding )
      [p "Model"]
    assertEqual "The buitenRolBeschrijving of the mogelijkeBinding of psp:PerspectivesSysteem$modelsInUse should have a property with local name 'contextLabel'."
      ((p "PerspectivesSysteem$modelsInUse") ##= mogelijkeBinding >-> buitenRolBeschrijvingDef `followedBy` unwrap >-> propertiesDef)
      ["model:Perspectives$BuitenRolPrototype$contextLabel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isFunctioneel"
      ,"model:Perspectives$Rol$buitenRolBeschrijving$isVerplicht"]
    assertEqual "psp:PerspectivesSysteem$modelsInUse should have a property with local name 'contextLabel'."
      ((p "PerspectivesSysteem$modelsInUse") ##= propertiesDef)
      [p "BuitenRolPrototype$contextLabel"]
    assertEqual "The contextLabel value of the BuitenRol of psp:Perspectives is "
      do
        contextLabel <- getPropertyFunction (p "BuitenRolPrototype$contextLabel")
        (buitenRol "model:Perspectives" ##= contextLabel)
      ["model:Perspectives"]


  -- testOnly "" do
  --   loadTestModel "TestOGC.crl"
  --
  --   unLoadTestModel "model:TestOGC"

  test "Changing testfile" do
    unLoadTestModel "model:TestOGC"
    -- loadTestModel "testTypeDefChecker.crl"

  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "testTypeDefChecker.crl"
  ---------------------------------------------------------------------------------

  -- testOnly "" do
  --   loadTestModel "testTypeDefChecker.crl"
  --
  --   unLoadTestModel "model:TestTDC"

  -- test "Tearing down" do
  --   unLoadTestModel "model:TestTDC"
