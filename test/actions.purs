module Test.Actions where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Effect.Class.Console (logShow)
import Foreign.Object (Object)
import Perspectives.Actions (setupBotActions)
import Perspectives.CoreTypes ((##=))
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.LoadCRL (loadAndSaveCrlFile, loadCrlFile, loadCrlFile_)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadAndCacheArcFile)
import Test.Perspectives.Utils (clearUserDatabase, runP, setupUser)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Actions" do

  test "compileAssignment: Remove" do
    r <- runP do
      _ <- loadAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          logShow "No model errors"
          instanceErrors <- loadCrlFile_ "actionsTestcase1.crl" testDirectory
          if null instanceErrors
            then Right <$> ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCase1$ARole"))
            else pure $ Left instanceErrors
        else pure $ Left modelErrors
    case r of
      Left e -> assert ("There are errors:\n" <> show e) false
      Right instances -> assert "There should be no instances of ARole." (length instances == 0)

  testOnly "compileAssignment: CreateRole" do
    r <- runP do
      _ <- loadAndCacheArcFile "perspectivesSysteem.arc" modelDirectory
      setupUser
      modelErrors <- loadAndCacheArcFile "actions.arc" testDirectory
      if null modelErrors
        then do
          logShow "No model errors"
          instanceErrors <- loadCrlFile_ "actionsTestcase2.crl" testDirectory
          if null instanceErrors
            then Right <$> ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:Test$TestCase2$ARole"))
            else pure $ Left instanceErrors
        else pure $ Left modelErrors
    case r of
      Left e -> assert ("There are errors:\n" <> show e) false
      Right instances -> assert "There should be a single (new) instance of ARole." (length instances == 1)
