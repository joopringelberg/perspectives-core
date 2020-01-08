module Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.Representation.Class.PersistentType (getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations" do

  test "Constant condition: true"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile "setAffectedContextCalculations.arc" testDirectory
      if null modelErrors
        then
          do
          EnumeratedRole{onRoleDelta_binding, onRoleDelta_binder, onContextDelta_context, onContextDelta_role} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase1$ARole")
          liftAff $ assert "There cannot be AffectedContextQUeries on ARole" (null onRoleDelta_binding && null onRoleDelta_binder && null onContextDelta_context && null onContextDelta_role)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "Constant condition: RoleName >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile "setAffectedContextCalculations.arc" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase2$ARole$Prop1")
          liftAff $ assert "There should be a single AffectedContextQuery on ARole$Prop1" (length onPropertyDelta == 1)
          EnumeratedRole{onContextDelta_context} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase2$ARole")
          liftAff $ assert "There should be a single AffectedContextQuery in onContextDelta_context on ARole" (length onContextDelta_context == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "Nested context condition: RoleName >> binding >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile "setAffectedContextCalculations.arc" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase3$SubCase$External$Prop2")
          liftAff $ assert "There should be a single AffectedContextQuery on SubCase$External$Prop2" (length onPropertyDelta == 1)
          EnumeratedRole{onRoleDelta_binder} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase3$SubCase1$External")
          liftAff $ assert "There should be a single AffectedContextQuery in OnRoleDelta_binder on ARole" (length onRoleDelta_binder == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
  test "Nested context condition: RoleName >> binding >> context >> RoleName >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile "setAffectedContextCalculations.arc" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase4$SubCase2$SubCaseRole1$Prop2")
          liftAff $ assert "There should be a single AffectedContextQuery on SubCase2$External$Prop2" (length onPropertyDelta == 1)
          EnumeratedRole{onContextDelta_context} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase4$SubCase2$SubCaseRole1")
          liftAff $ assert "There should be a single AffectedContextQuery in onContextDelta_context on SubCase2$SubCaseRole1" (length onContextDelta_context == 1)
          EnumeratedRole{onRoleDelta_binder} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase4$SubCase2$External")
          liftAff $ assert "There should be a single AffectedContextQuery in OnRoleDelta_binder on NestedContext" (length onRoleDelta_binder == 1)
          EnumeratedRole{onContextDelta_context} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase4$NestedContext")
          liftAff $ assert "There should be a single AffectedContextQuery in onContextDelta_context on NestedContext" (length onContextDelta_context == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
  test "On the external role of the current context: extern >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile "setAffectedContextCalculations.arc" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase5$SubCase3$External$Prop2")
          liftAff $ assert "There should be a single AffectedContextQuery on SubCase3$External$Prop2" (length onPropertyDelta == 1)
          EnumeratedRole{onContextDelta_context} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase5$SubCase3$External")
          liftAff $ assert "There should be a single AffectedContextQuery in onContextDelta_context on ARole" (length onContextDelta_context == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
  test "On a role of the enclosing context: extern >> binder XX >> context >> RoleName >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile "setAffectedContextCalculations.arc" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase6$AnotherRole$Prop3")
          liftAff $ assert "There should be a single AffectedContextQuery on AnotherRole$Prop3" (length onPropertyDelta == 1)
          EnumeratedRole{onContextDelta_context} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase6$AnotherRole")
          liftAff $ assert "There should be a single AffectedContextQuery in onContextDelta_context on AnotherRole" (length onContextDelta_context == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
