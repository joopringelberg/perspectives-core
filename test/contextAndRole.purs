module Test.ContextAndRole where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null, union)
import Data.Either (Either)
import Data.Tuple (Tuple)
import Effect.Class.Console (logShow)
import Foreign.Object (Object)
import Perspectives.Actions (setupBotActions)
import Perspectives.ContextAndRole (addContext_rolInContext, context_rolInContext, modifyContext_rolInContext)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances (getPerspectContext)
import Perspectives.LoadCRL (loadAndSaveCrlFile, loadCrlFile)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Representation.Class.PersistentType (getContext, getEnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suiteSkip "ContextAndRole" do
  test "Access a Role" do
    ra <- runP do
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "test1.crl" testDirectory
      -- logShow r
      -- now get the role
      getPerspectContext (ContextInstance "model:User$MyTestCase") >>= pure <<< flip context_rolInContext (EnumeratedRoleType "model:Test$TestCase$Self")
    assert "There should be an instance of Self" (length ra == 1)

  test "Add a Role" do
    ra <- runP do
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "test1.crl" testDirectory
      -- logShow r
      -- now add a Role
      getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
        (pure <<< \c -> addContext_rolInContext c  (EnumeratedRoleType "model:Test$TestCase$Self") (RoleInstance "blabla")) >>=
          (pure <<< flip context_rolInContext (EnumeratedRoleType "model:Test$TestCase$Self"))
    assert "There should be two instances of Self" (length ra == 2)

  test "Add an array of Role instances" do
    ra <- runP do
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "test1.crl" testDirectory
      -- logShow r
      -- now add a Role
      getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
        (pure <<< \c -> modifyContext_rolInContext c (EnumeratedRoleType "model:Test$TestCase$Self") (flip union [(RoleInstance "blabla")])) >>=
          (pure <<< flip context_rolInContext (EnumeratedRoleType "model:Test$TestCase$Self"))
    assert "There should be two instances of Self" (length ra == 2)

  test "Retrieve non-existing role instances" do
    ra <- runP do
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "test1.crl" testDirectory
      -- logShow r
      -- now add a Role
      getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
          (pure <<< flip context_rolInContext (EnumeratedRoleType "model:Test$TestCase$SomeRole"))
    assert "There should be an empty array of instances of Self" (length ra == 0)

  test "Add an array of Role instances to non-existing role instances" do
    ra <- runP do
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "test1.crl" testDirectory
      -- logShow r
      -- now add a Role
      getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
        (pure <<< \c -> modifyContext_rolInContext c (EnumeratedRoleType "model:Test$TestCase$SomeRole") (flip union [(RoleInstance "blabla")])) >>=
          (pure <<< flip context_rolInContext (EnumeratedRoleType "model:Test$TestCase$SomeRole"))
    assert "There should be one instance of Self" (length ra == 1)

  test "Add a Role instance to non-existing role instances" do
    ra <- runP do
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadCrlFile "test1.crl" testDirectory
      -- logShow r
      -- now add a Role
      getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
        (pure <<< \c -> addContext_rolInContext c (EnumeratedRoleType "model:Test$TestCase$SomeRole") (RoleInstance "blabla")) >>=
          (pure <<< flip context_rolInContext (EnumeratedRoleType "model:Test$TestCase$SomeRole"))
    assert "There should be one instance of Self" (length ra == 1)
