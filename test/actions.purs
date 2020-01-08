module Test.Actions where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Array (length, null)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CoreTypes ((##=))
import Perspectives.IndentParser (getRoleInstances)
import Perspectives.Instances.ObjectGetters (allRoleBinders, binding, getProperty, getRole)
import Perspectives.LoadCRL (loadAndSaveCrlFile, loadCrlFile, loadCrlFile_)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Representation.Class.Action (effect)
import Perspectives.Representation.Class.PersistentType (getAction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ActionType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile, loadCompileAndSaveArcFile)
import Test.Perspectives.Utils (runP, setupUser)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.Actions" do

  test "compileAssignment: Remove" do
    r <- runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- logShow "No model errors"
          instanceErrors <- loadCrlFile_ "actionsTestcase1.crl" testDirectory
          if null instanceErrors
            then Right <$> ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCase1$ARole"))
            else pure $ Left instanceErrors
        else pure $ Left modelErrors
    case r of
      Left e -> assert ("There are errors:\n" <> show e) false
      Right instances -> assert "There should be no instances of ARole." (length instances == 0)

  test "compileAssignment: CreateRole" do
    r <- runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- logShow "No model errors"
          instanceErrors <- loadCrlFile_ "actionsTestcase2.crl" testDirectory
          if null instanceErrors
            then Right <$> ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCase2$ARole"))
            else pure $ Left instanceErrors
        else pure $ Left modelErrors
    case r of
      Left e -> assert ("There are errors:\n" <> show e) false
      Right instances -> assert "There should be a single (new) instance of ARole." (length instances == 1)

  test "compileAssignment: Move" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          instanceErrors <- loadCrlFile_ "actionsTestcase3.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase$MyNested1") ##= getRole (EnumeratedRoleType "model:Test$TestCase3$NestedContext$ARole"))
              liftAff $ assert "There should be a no instance of ARole in MyNested1." (length n1 == 0)
              n2 <- ((ContextInstance "model:User$MyTestCase$MyNested2") ##= getRole (EnumeratedRoleType "model:Test$TestCase3$NestedContext$ARole"))
              liftAff $ assert "There should be a no instance of ARole in MyNested2." (length n2 == 1)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: Bind" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestBind.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseBind$ARole") >=> binding)
              liftAff $ assert "ARole should have a binding." (length n1 == 1)
              n2 <- ((ContextInstance "model:User$MyTestCase$MyNestedCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseBind$NestedCase$ARole") >=> binding)
              liftAff $ assert "ARole in the NestedCase should have a binding." (length n2 == 1)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: Bind_" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestBind_.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseBind_$ARole4") >=> binding)
              liftAff $ assert "ARole4 should have a binding." (length n1 == 1)
              n2 <- ((ContextInstance "model:User$MyTestCase$MyNestedCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseBind_$NestedCase4$ARole4") >=> binding)
              liftAff $ assert "ARole4 in the NestedCase should have a binding." (length n2 == 1)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: Unbind" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestUnbind.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseUnbind$AnotherRole5") >=> allRoleBinders)
              liftAff $ assert "AnotherRole5 should have no binders." (length n1 == 0)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: UnbindQualified" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestUnbindQualified.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseUnbindQualified$AnotherRole6") >=> allRoleBinders)
              liftAff $ assert "AnotherRole6 should have a single binder." (length n1 == 1)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: Unbind_" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestUnbind_.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseUnbind_$AnotherRole7") >=> allRoleBinders)
              liftAff $ assert "AnotherRole7 should have a single binder." (length n1 == 1)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: DeleteProperty" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestDeleteProperty.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseDeleteProp$ARole8") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseDeleteProp$ARole8$Prop1"))
              liftAff $ assert "Prop1 should have no values." (length n1 == 0)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: RemoveProperty" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestRemoveProperty.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseRemoveProp$ARole9") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseRemoveProp$ARole9$Prop2"))
              -- logShow n1
              liftAff $ assert "Prop2 should have a single value." (length n1 == 1)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: AddProperty" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestAddProperty.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseAddProp$ARole10") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseAddProp$ARole10$Prop3"))
              -- logShow n1
              liftAff $ assert "Prop2 should have three values." (length n1 == 3)
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  test "compileAssignment: SetProperty" (runP do
      _ <- loadCompileAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadCompileAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "actionsTestSetProperty.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseSetProp$ARole11") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseSetProp$ARole11$Prop4"))
              -- logShow n1
              liftAff $ assert "Prop2 should have three values." (n1 == [Value "mies"])
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  testSkip "Load files into couchdb" (runP do
      _ <- loadCompileAndSaveArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      _ <- loadCompileAndSaveArcFile "actions.arc" testDirectory
      _ <- loadAndSaveCrlFile "actionsTestBind.crl" testDirectory
      pure unit
      )
