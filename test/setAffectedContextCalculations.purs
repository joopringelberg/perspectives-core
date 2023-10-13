module Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (fromFoldable, length, null)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Foreign.Object (keys) as OBJ
import Perspectives.Data.EncodableMap (keys)
import Perspectives.Representation.Class.PersistentType (getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile')
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations" do

  test "Constant condition: true"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile' "setAffectedContextCalculations" testDirectory
      if null modelErrors
        then
          do
          EnumeratedRole{fillerInvertedQueries, filledInvertedQueries, contextInvertedQueries} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase1$ARole")
          -- log (prettyPrint filledByInvertedQueries)
          -- log (prettyPrint fillsInvertedQueries)
          -- log (prettyPrint contextInvertedQueries)
          -- log (prettyPrint onContextDelta_role)
          liftAff $ assert "There should be one AffectedContextQueries on ARole" (((length $ fromFoldable $ keys fillerInvertedQueries) + (length $ fromFoldable $ keys filledInvertedQueries) + (length $ OBJ.keys contextInvertedQueries)) == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "Constant condition: RoleName >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile' "setAffectedContextCalculations" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase2$ARole$Prop1")
          -- log $ prettyPrint onPropertyDelta
          liftAff $ assert "There should be two AffectedContextQueries on ARole$Prop1" ((length $ OBJ.keys onPropertyDelta) == 1)
          EnumeratedRole{contextInvertedQueries} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase2$ARole")
          -- log $ prettyPrint contextInvertedQueries
          liftAff $ assert "There should two AffectedContextQueries in contextInvertedQueries on ARole" ((length $ OBJ.keys contextInvertedQueries) == 2)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "Nested context condition: RoleName >> binding >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile' "setAffectedContextCalculations" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase3$SubCase1$External$Prop2")
          -- log (prettyPrint onPropertyDelta)
          liftAff $ assert "There should be a single AffectedContextQuery on SubCase$External$Prop2" ((length $ OBJ.keys onPropertyDelta) == 1)
          EnumeratedRole{filledInvertedQueries} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase3$SubCase1$External")
          liftAff $ assert "There should be a single AffectedContextQuery in OnRoleDelta_binder on ARole" ((length $ fromFoldable $ keys filledInvertedQueries) == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
  test "Nested context condition: RoleName >> binding >> context >> RoleName >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile' "setAffectedContextCalculations" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase4$SubCase2$SubCaseRole1$Prop2")
          liftAff $ assert "There should be a single AffectedContextQuery on SubCase2$External$Prop2" ((length $ OBJ.keys onPropertyDelta) == 1)
          EnumeratedRole{contextInvertedQueries} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase4$SubCase2$SubCaseRole1")
          liftAff $ assert "There should be a single AffectedContextQuery in contextInvertedQueries on SubCase2$SubCaseRole1" (((length $ OBJ.keys contextInvertedQueries)) == 1)
          EnumeratedRole{filledInvertedQueries} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase4$SubCase2$External")
          liftAff $ assert "There should be a single AffectedContextQuery in OnRoleDelta_binder on NestedContext" ((length $ fromFoldable $ keys filledInvertedQueries) == 1)
          -- KLOPT DIT WEL? in de code staat: geen inverted queries op externe rol.
          er@(EnumeratedRole{contextInvertedQueries}) <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase4$NestedContext")
          -- logShow er
          liftAff $ assert "There should be a single AffectedContextQuery in contextInvertedQueries on NestedContext" ((length $ OBJ.keys contextInvertedQueries) == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
  test "On the external role of the current context: extern >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile' "setAffectedContextCalculations" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase5$SubCase3$External$Prop2")
          -- logShow $ (length $ keys onPropertyDelta)
          liftAff $ assert "There should be two AffectedContextQueries on SubCase3$External$Prop2" ((length $ OBJ.keys onPropertyDelta) == 1)
          EnumeratedRole{contextInvertedQueries} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase5$SubCase3$External")
          liftAff $ assert "There should be a single AffectedContextQuery in contextInvertedQueries on ARole" ((length $ OBJ.keys contextInvertedQueries) == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
  test "On a role of the enclosing context: extern >> binder XX >> context >> RoleName >> PropName"
    (runP do
      modelErrors <- loadCompileAndCacheArcFile' "setAffectedContextCalculations" testDirectory
      if null modelErrors
        then
          do
          EnumeratedProperty{onPropertyDelta} <- getEnumeratedProperty (EnumeratedPropertyType "model:Test$TestCase6$AnotherRole$Prop3")
          -- logShow onPropertyDelta
          liftAff $ assert "There should be a single AffectedContextQuery on AnotherRole$Prop3" ((length $ OBJ.keys onPropertyDelta) == 1)
          EnumeratedRole{contextInvertedQueries} <- getEnumeratedRole (EnumeratedRoleType "model:Test$TestCase6$AnotherRole")
          -- logShow contextInvertedQueries
          liftAff $ assert "There should be a single AffectedContextQuery in contextInvertedQueries on AnotherRole" ((length $ OBJ.keys contextInvertedQueries) == 1)
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
