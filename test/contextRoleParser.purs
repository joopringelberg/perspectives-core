module Test.ContextRoleParser where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple)
import Effect.Class.Console (logShow)
import Foreign.Object (Object, lookup)
import Perspectives.Assignment.Update (removeBinding, setBinding)
import Perspectives.ContextAndRole (context_me, rol_gevuldeRollen, rol_isMe)
import Perspectives.CoreTypes ((##>>))
import Perspectives.Deltas (runTransactie)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.LoadCRL (loadAndSaveCrlFile, loadCrlFile)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Persistent (getPerspectRol, getPerspectContext)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.SetupUser (setupUser) as SU
import Perspectives.TypePersistence.LoadArc (loadAndCacheArcFile, loadAndSaveArcFile)
import Test.Perspectives.Utils (runP, setupUser)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "ContextRoleParser" do
  test "inverse binding" do
    ra <- runP do
      _ <- loadAndCacheArcFile "contextRoleParser.arc" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "contextRoleParser.crl" testDirectory
      (rl :: RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##>> getRole (EnumeratedRoleType "model:Test$TestCase$Self")
      getPerspectRol rl >>= pure <<< rol_gevuldeRollen
    assert "There should be two inverse bindings for model:Test$TestCase$NestedCase$NestedSelf" (Just 2 == (length <$> (lookup "model:Test$TestCase$NestedCase$NestedSelf" ra)))
    assert "There should be an inverse binding for model:Test$TestCase$NestedCase2$NestedSelf" (isJust (lookup "model:Test$TestCase$NestedCase2$NestedSelf" ra))

  test "Load both a model and instances in couchdb" do
    runP SU.setupUser
    _ <- runP $ loadAndSaveArcFile "contextRoleParser.arc" testDirectory
    r <- runP $ loadAndSaveCrlFile "contextRoleParser.crl" testDirectory
    logShow r
    assert "A CRL file should load without problems" (null r)

  test "isMe for a role constructed with binding that represents the user." do
    ra <- runP do
      _ <- setupUser
      _ <- loadAndCacheArcFile "contextRoleParser.arc" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "contextRoleParser.crl" testDirectory
      (rl :: RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##>> getRole (EnumeratedRoleType "model:Test$TestCase$Self")
      getPerspectRol rl
    assert "Self should have isMe == true" (rol_isMe ra)

  test "me for a context constructed with a role that represents the user." do
    c <- runP do
      _ <- setupUser
      _ <- loadAndCacheArcFile "contextRoleParser.arc" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "contextRoleParser.crl" testDirectory
      getPerspectContext (ContextInstance "model:User$MyTestCase")
    assert "MyTestCase should have 'me' equal to model:User$MyTestCase$Self_0001" (context_me c == Just (RoleInstance "model:User$MyTestCase$Self_0001"))

  test "isMe for a role we bind with a role that represents the user." do
    ra <- runP do
      _ <- setupUser
      _ <- loadAndCacheArcFile "contextRoleParser.arc" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "contextRoleParser.crl" testDirectory
      void $ runMonadPerspectivesTransaction $ setBinding
        (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
        (RoleInstance "model:User$MijnSysteem$User_0001")
      getPerspectRol (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
    assert "Self should have isMe == true" (rol_isMe ra)

  test "me for a role we bind with a role that represents the user." do
    c <- runP do
      _ <- setupUser
      _ <- loadAndCacheArcFile "contextRoleParser.arc" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "contextRoleParser.crl" testDirectory
      void $ runMonadPerspectivesTransaction $ setBinding
        (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
        (RoleInstance "model:User$MijnSysteem$User_0001")
      getPerspectContext (ContextInstance "model:User$MyTestCase$MyNestedCase3")
    -- logShow c
    assert "MyNestedCase3 should have 'me' equal to model:User$MyTestCase$MyNestedCase3$NestedSelf_0001" (context_me c == Just (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001"))

  testOnly "me for the context of a role from which we remove the binding." do
    c <- runP do
      _ <- setupUser
      _ <- loadAndCacheArcFile "contextRoleParser.arc" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "contextRoleParser.crl" testDirectory
      void $ runMonadPerspectivesTransaction $ removeBinding
        (RoleInstance "model:User$MyTestCase$MyNestedCase2$NestedSelf_0001")
      getPerspectContext (ContextInstance "model:User$MyTestCase$MyNestedCase3")
    assert "MyNestedCase2 should have 'me' equal to Nothing" (context_me c == Nothing)
