module Test.LoadCRL where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Data.Either (Either)
import Data.Tuple (Tuple)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Foreign.Object (Object)
import Perspectives.CoreTypes ((##=))
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.LoadCRL (loadAndSaveCrlFile, loadCrlFile)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile, loadCompileAndCacheArcFile', loadCompileAndSaveArcFile')
import Test.Perspectives.Utils (clearUserDatabase, runP, setupUser)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.loadCRL" do
  test "Load a file with a context instance in cache" do
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- runP $ loadCrlFile "combinators.crl" testDirectory
    -- logShow r
    pure unit

  test "Load a file with a context instance in couchdb with a light check" do
    r <- runP $ loadAndSaveCrlFile "contextAndRole.crl" testDirectory
    if null r
      then do
        srole <- runP ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole"))
        assert "There should not be an instance of SomeRole." (length srole == 0)
        runP clearUserDatabase
      else do
        logShow r
        assert "Expected to load a file into couchdb" false

  test "Load a file with a context instance, setup bot action" do
    r <- runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      setupUser
      _ <- loadCompileAndCacheArcFile' "contextAndRole" testDirectory
      _ <- loadCrlFile "contextAndRole2.crl" testDirectory
      ((ContextInstance "model:User$MyTestCase") ##= getRole (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole"))
    assert "There should be an instance of SomeRole." (length r == 1)

  test "Load a file with a context instance in couchdb" (runP do
    _ <- loadCompileAndSaveArcFile' "perspectivesSysteem" modelDirectory
    r <- loadAndSaveCrlFile "systemInstances.crl" modelDirectory
    if null r
      then liftAff $ assert "OK" true
      else do
        logShow r
        liftAff $ assert "A CRL file should load without problems" false
)
