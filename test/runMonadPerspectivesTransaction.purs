module Test.RunMonadPerspectivesTransaction where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes ((##=))
import Perspectives.Instances.ObjectGetters (getProperty, getRole)
import Perspectives.LoadCRL (loadAndSaveCrlFile, loadCrlFile_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (clearUserDatabase, runP, setupUser)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Actions" do

  test "contextDelta_context" (runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "onContextDelta_context.crl" testDirectory
          if null instanceErrors
            then do
              sr <- (ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseContextDelta_context$SomeRole")
              liftAff $ assert "There should be an instance of SomeRole" (length sr == 1)
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseContextDelta_context$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseContextDelta_context$RoleToInspect$Flag"))
              liftAff $ assert "Flag should be true." (n1 == [Value "true"])
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "contextDelta_role" (runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "onContextDelta_role.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MySubcase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseContextDelta_rol$SubCase$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseContextDelta_rol$SubCase$RoleToInspect$Flag"))
              liftAff $ assert "Flag should be true." (n1 == [Value "true"])
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "roleDelta_binding" (runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "onRoleDelta_binding.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseRoleDelta_binding$BinderRole") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseRoleDelta_binding$BinderRole$Flag"))
              liftAff $ assert "Flag should be true." (n1 == [Value "true"])
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "roleDelta_binder" (runP do
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    setupUser
    modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
    if null modelErrors
      then do
        instanceErrors <- loadCrlFile_ "onRoleDelta_binder.crl" testDirectory
        if null instanceErrors
          then do
            n1 <- ((ContextInstance "model:User$MySubcase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseRoleDelta_binder$SubCase2$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseRoleDelta_binder$SubCase2$RoleToInspect$Flag"))
            liftAff $ assert "Flag should be true." (n1 == [Value "true"])
          else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "propertyDelta" (runP do
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    setupUser
    modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
    if null modelErrors
      then do
        instanceErrors <- loadCrlFile_ "onPropertyDelta.crl" testDirectory
        if null instanceErrors
          then do
            n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCasePropertyDelta$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCasePropertyDelta$RoleToInspect$Flag"))
            liftAff $ assert "Flag should be true." (n1 == [Value "true"])
          else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "TestCaseInCouchdb" do
    (runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
      if null modelErrors
        then do
          instanceErrors <- loadAndSaveCrlFile "TestCaseInCouchdb.crl" testDirectory
          if null instanceErrors
            then pure unit
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )
    (runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
      if null modelErrors
        then do
          -- Now set Prop on ARole to be true
          aRoleInstance <- (ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseInCouchdb$ARole")
          _ <- runMonadPerspectivesTransaction (setProperty aRoleInstance (EnumeratedPropertyType "model:Test$TestCaseInCouchdb$ARole$Prop") [(Value "true")])
          -- NOTE: on inspecting Couchdb we see the Flag hoisted even if we comment out the lines below.
          n1 <- ((ContextInstance "model:User$MySubcase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseInCouchdb$SubCase3$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseInCouchdb$SubCase3$RoleToInspect$Flag"))
          liftAff $ assert "Flag should be true." (n1 == [Value "true"])
          clearUserDatabase
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )
