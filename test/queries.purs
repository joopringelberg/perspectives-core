module Test.Queries where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CoreTypes ((##=))
import Perspectives.Query.UnsafeCompiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Queries" do

  test "Two results over: RoleA >> binding" (runP do
    -- Load the test Arc file from the testDirectory. Parse the file completely. Cache it.
    modelErrors <- loadCompileAndCacheArcFile "queries" testDirectory
    if null modelErrors
      then do
        getComputed <- getRoleFunction "model:Test$Case1$Computed"
        cs <- ((ContextInstance "model:User$TC1") ##= getComputed)
        logShow cs
        liftAff $ assert "There should be two role instances for Computed" (length cs == 2)
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "Three results over RoleB >> binder RoleA" (runP do
    -- Load the test Arc file from the testDirectory. Parse the file completely. Cache it.
    modelErrors <- loadCompileAndCacheArcFile "queries" testDirectory
    if null modelErrors
      then do
        getComputed <- getRoleFunction "model:Test$Case2$Computed"
        cs <- ((ContextInstance "model:User$TC2") ##= getComputed)
        logShow cs
        liftAff $ assert "There should be two role instances for Computed" (length cs == 3)
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
  test "One result over filter RoleE with Prop1" (runP do
    -- Load the test Arc file from the testDirectory. Parse the file completely. Cache it.
    modelErrors <- loadCompileAndCacheArcFile "queries" testDirectory
    if null modelErrors
      then do
        getComputed <- getRoleFunction "model:Test$Case3$Computed"
        cs <- ((ContextInstance "model:User$TC3") ##= getComputed)
        logShow cs
        liftAff $ assert "There should be one role instance for Computed" (length cs == 1)
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "One result over filter RoleF with binding >> Prop1" (runP do
    -- Load the test Arc file from the testDirectory. Parse the file completely. Cache it.
    modelErrors <- loadCompileAndCacheArcFile "queries" testDirectory
    if null modelErrors
      then do
        getComputed <- getRoleFunction "model:Test$Case4$Computed"
        cs <- ((ContextInstance "model:User$TC4") ##= getComputed)
        logShow cs
        liftAff $ assert "There should be one role instance for Computed" (length cs == 1)
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "One result over filter RoleF with not binding >> Prop1" (runP do
    -- Load the test Arc file from the testDirectory. Parse the file completely. Cache it.
    modelErrors <- loadCompileAndCacheArcFile "queries" testDirectory
    if null modelErrors
      then do
        getComputed <- getRoleFunction "model:Test$Case5$Computed"
        cs <- ((ContextInstance "model:User$TC5") ##= getComputed)
        logShow cs
        liftAff $ assert "There should be one role instance for Computed" (length cs == 1)
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
