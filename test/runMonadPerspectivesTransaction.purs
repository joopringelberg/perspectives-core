module Test.RunMonadPerspectivesTransaction where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CoreTypes ((##=))
import Perspectives.Instances.ObjectGetters (getProperty, getRole)
import Perspectives.LoadCRL (loadCrlFile_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadAndCacheArcFile)
import Test.Perspectives.Utils (runP, setupUser)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Actions" do

  test "contextDelta_context" (runP do
      _ <- loadAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadAndCacheArcFile "runMonadPerspectivesTransaction.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "onContextDelta.crl" testDirectory
          if null instanceErrors
            then do
              sr <- (ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseContextDelta_context$SomeRole")
              liftAff $ assert "There should be an instance of SomeRole" (length sr == 1)
              n1 <- ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseContextDelta_context$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseContextDelta_context$RoleToInspect$Flag"))
              liftAff $ assert "Flag should be true." (n1 == [Value "true"])
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )

  testOnly "contextDelta_role" (runP do
      _ <- loadAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadAndCacheArcFile "runMonadPerspectivesTransaction.arc" testDirectory
      if null modelErrors
        then do
          -- eff <- (getAction $ ActionType "model:Test$TestCaseBind$Self_bot$ChangeSelf") >>= effect
          -- logShow eff
          instanceErrors <- loadCrlFile_ "onContextDelta.crl" testDirectory
          if null instanceErrors
            then do
              n1 <- ((ContextInstance "model:User$MySubcase") ##= getRole (EnumeratedRoleType "model:Test$TestCaseContextDelta_rol$SubCase$RoleToInspect") >=> getProperty (EnumeratedPropertyType "model:Test$TestCaseContextDelta_rol$SubCase$RoleToInspect$Flag"))
              liftAff $ assert "Flag should be true." (n1 == [Value "true"])
            else liftAff $ assert ("There are instance errors: " <> show instanceErrors) false
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
        )
