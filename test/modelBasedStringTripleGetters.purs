module Test.Perspectives.ModelBasedStringTripleGetters (theSuite) where

import Prelude

import Control.Monad.Free (Free)
import Perspectives.ModelBasedStringTripleGetters (hasOnEachRolTelescopeTheTypeOf, mogelijkeBinding)
import Perspectives.ModelBasedTripleGetters (hasType, sumToSequence)
import Perspectives.PerspectivesTypes (PBool(..))
import Perspectives.QueryCombinators (notEmpty)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterComposition ((>->))
import Test.Perspectives.Utils (TestEffects, TestModelLoadEffects, assertEqual, loadTestModel, p, unLoadTestModel)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)

t :: String -> String
t s = "model:TestOGC$" <> s

t2 :: String -> String
t2 s = "model:TestTDC$" <> s

theSuite :: forall e. Free (TestF (TestEffects (TestModelLoadEffects e))) Unit
theSuite = suiteSkip "ModelBasedStringTripleGetters" do
  test "Setting up" do
    loadTestModel "TestOGC.crl"
  ---------------------------------------------------------------------------------
  -- TESTS ON THE FILE "TestOGC.crl"
  ---------------------------------------------------------------------------------
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

  test "hasOnEachRolTelescopeTheTypeOf (allowedBinding ## (`hasOnEachRolTelescopeTheTypeOf` t))" do
    assertEqual "psp:SimpleValue should have type psp:Context"
      (p "SimpleValue" ##= hasType (p "Context"))
      [PBool "true"]
    assertEqual "psp:SimpleValue should have type psp:Context"
      (p "Context" ##= hasOnEachRolTelescopeTheTypeOf (p "SimpleValue"))
      [PBool "true"]
    assertEqual "t:myContextDef6$rol1 is in its own rolTelescope."
      ((t "myContextDef6$rol1") ##= (hasOnEachRolTelescopeTheTypeOf (t "myContextDef6$rol1")))
      [PBool "true"]
    assertEqual "t:myContextDef6$rol1 does have a value for mogelijkeBinding"
      ((t "myContextDef6$rol1") ##= mogelijkeBinding)
      [t "myContextDef5$rol1"]
    assertEqual "t:myContextDef6$rol1 does have a value for mogelijkeBinding"
      ((t "myContextDef6$rol1") ##= (notEmpty (mogelijkeBinding >-> sumToSequence)))
      [PBool "true"]
    assertEqual "t:myContextDef5$rol1 is in each rolTelescope that starts with t:myContextDef6$rol1"
      ((t "myContextDef6$rol1") ##= (hasOnEachRolTelescopeTheTypeOf (t "myContextDef5$rol1")))
      [PBool "true"]
    assertEqual "t:myContextDef$rol1 is NOT in each rolTelescope that starts with t:myContextDef6$rol1"
      ((t "myContextDef6$rol1") ##= (hasOnEachRolTelescopeTheTypeOf (t "myContextDef$rol1")))
      [PBool "false"]
    assertEqual "t:myContextDef5$rol1 is NOT in each rolTelescope that starts with t:myContextDef7$rol1"
      ((t "myContextDef7$rol1") ##= (hasOnEachRolTelescopeTheTypeOf (t "myContextDef5$rol1")))
      [PBool "false"]
    assertEqual "t:myContextDef5$rol1 is in each rolTelescope that starts with t:myContextDef9$rol1"
      ((t "myContextDef9$rol1") ##= (hasOnEachRolTelescopeTheTypeOf (t "myContextDef5$rol1")))
      [PBool "true"]
    assertEqual "psp:PerspectivesSysteem$gebruiker is in each rolTelescope that starts with psp:TrustedCluster$clusterGenoot"
      ((p "TrustedCluster$clusterGenoot") ##= (hasOnEachRolTelescopeTheTypeOf (p "PerspectivesSysteem$gebruiker")))
      [PBool "true"]

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
