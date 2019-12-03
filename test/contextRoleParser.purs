module Test.ContextRoleParser where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple)
import Effect.Class.Console (logShow)
import Foreign.Object (Object, lookup)
import Perspectives.ContextAndRole (rol_gevuldeRollen)
import Perspectives.CoreTypes ((##>>))
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.LoadCRL (loadAndSaveCrlFile, loadCrlFile)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadAndCacheArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "ContextRoleParser" do
  testOnly "inverse binding" do
    ra <- runP do
      _ <- loadAndCacheArcFile "contextRoleParser.arc" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "contextRoleParser.crl" testDirectory
      (rl :: RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##>> getRole (EnumeratedRoleType "model:Test$TestCase$Self")
      getPerspectRol rl >>= pure <<< rol_gevuldeRollen
    assert "There should be two inverse bindings for model:Test$TestCase$NestedCase$NestedSelf" (Just 2 == (length <$> (lookup "model:Test$TestCase$NestedCase$NestedSelf" ra)))
    assert "There should be an inverse binding for model:Test$TestCase$NestedCase2$NestedSelf" (isJust (lookup "model:Test$TestCase$NestedCase2$NestedSelf" ra))

  test "Load a file with a context instance in couchdb" do
    r <- runP $ loadAndSaveCrlFile "contextRoleParser.crl" testDirectory
    logShow r
    assert "A CRL file should load without problems" (null r)
