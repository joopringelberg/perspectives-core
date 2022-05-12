module Test.RunMonadPerspectivesTransaction where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Effect.Aff.Class (liftAff)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes ((##=))
import Perspectives.Instances.ObjectGetters (getProperty, getEnumeratedRoleInstances)
import Perspectives.LoadCRL.FS (loadAndSaveCrlFile)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (clearUserDatabase, runP, withModel', withSystem, runMonadPerspectivesTransaction)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "RunMonadPerspectivesTransaction" do

  test "contextDelta_context" (runP $ withSystem do
    modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
    if null modelErrors
      then do
        -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
        -- logShow eff
        instanceErrors <- loadAndSaveCrlFile "onContextDelta_context.crl" testDirectory
        if null instanceErrors
          then do
            sr <- (ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCaseContextDelta_context$SomeRole")
            liftAff $ assert "There should be an instance of SomeRole" (length sr == 1)
            n1 <- ((ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCaseContextDelta_context$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseContextDelta_context$RoleToInspect$Flag"))
            liftAff $ assert "Flag should be true." (n1 == [Value "true"])
          else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "contextDelta_role" (runP $ withSystem do
    modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
    if null modelErrors
      then do
        -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
        -- logShow eff
        instanceErrors <- loadAndSaveCrlFile "onContextDelta_role.crl" testDirectory
        if null instanceErrors
          then do
            n1 <- ((ContextInstance "model:User$MySubcase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCaseContextDelta_rol$SubCase$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseContextDelta_rol$SubCase$RoleToInspect$Flag"))
            liftAff $ assert "Flag should be true." (n1 == [Value "true"])
          else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "roleDelta_binding" (runP $ withSystem do
    modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
    if null modelErrors
      then do
        -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
        -- logShow eff
        instanceErrors <- loadAndSaveCrlFile "onRoleDelta_binding.crl" testDirectory
        if null instanceErrors
          then do
            n1 <- ((ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCaseRoleDelta_binding$BinderRole") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseRoleDelta_binding$BinderRole$Flag"))
            liftAff $ assert "Flag should be true." (n1 == [Value "true"])
          else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "roleDelta_binder" (runP $ withSystem do
    modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
    if null modelErrors
      then do
        instanceErrors <- loadAndSaveCrlFile "onRoleDelta_binder.crl" testDirectory
        if null instanceErrors
          then do
            n1 <- ((ContextInstance "model:User$MySubcase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCaseRoleDelta_binder$SubCase2$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseRoleDelta_binder$SubCase2$RoleToInspect$Flag"))
            liftAff $ assert "Flag should be true." (n1 == [Value "true"])
          else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "propertyDelta" (runP $ withSystem do
    modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
    if null modelErrors
      then do
        instanceErrors <- loadAndSaveCrlFile "onPropertyDelta.crl" testDirectory
        if null instanceErrors
          then do
            n1 <- ((ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCasePropertyDelta$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCasePropertyDelta$RoleToInspect$Flag"))
            liftAff $ assert "Flag should be true." (n1 == [Value "true"])
          else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "TestCaseInCouchdb" do
    (runP $ withModel' (DomeinFileId "model:System") do
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
      void $ loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      modelErrors <- loadCompileAndCacheArcFile' "runMonadPerspectivesTransaction" testDirectory
      if null modelErrors
        then do
          -- Now set Prop on ARole to be true
          aRoleInstance <- (ContextInstance "model:User$MyTestCase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCaseInCouchdb$ARole")
          _ <- runMonadPerspectivesTransaction (setProperty aRoleInstance (EnumeratedPropertyType "model:Test$TestCaseInCouchdb$ARole$Prop") [(Value "true")])
          -- NOTE: on inspecting Couchdb we see the Flag hoisted even if we comment out the lines below.
          n1 <- ((ContextInstance "model:User$MySubcase") ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCaseInCouchdb$SubCase3$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseInCouchdb$SubCase3$RoleToInspect$Flag"))
          liftAff $ assert "Flag should be true." (n1 == [Value "true"])
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      clearUserDatabase)
