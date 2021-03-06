module Test.ContextAndRole where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, union)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Perspectives.ContextAndRole (addContext_rolInContext, addRol_gevuldeRollen, addRol_property, changeRol_binding, context_rolInContext, deleteContext_rolInContext, deleteRol_property, modifyContext_rolInContext, removeContext_rolInContext, removeRol_binding, removeRol_gevuldeRollen, removeRol_property, rol_binding, rol_gevuldeRol, rol_property, setRol_property)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile)
import Test.Perspectives.Utils (runP, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "ContextAndRole" do
  test "Access a Role" $ runP $ withSystem do
    (r :: (Array PerspectivesError)) <- loadCompileAndCacheArcFile "contextAndRole" testDirectory
    -- logShow r
    -- now get the role
    ra <- getPerspectContext (ContextInstance "model:User$MyTestCase") >>= pure <<< flip context_rolInContext (EnumeratedRoleType "model:ContextAndRole$TestCase$Self")
    liftAff $ assert "There should be an instance of Self" (length ra == 1)

  test "Add a Role" $ runP $ withSystem do
    (r :: (Array PerspectivesError)) <- loadCompileAndCacheArcFile "contextAndRole" testDirectory
    -- logShow r
    -- now add a Role
    ra <- getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
      (\c -> addContext_rolInContext c  (EnumeratedRoleType "model:ContextAndRole$TestCase$Self") (RoleInstance "blabla")) >>=
        (pure <<< flip context_rolInContext (EnumeratedRoleType "model:ContextAndRole$TestCase$Self"))
    liftAff $ assert "There should be two instances of Self" (length ra == 2)

  test "Add an array of Role instances" $ runP $ withSystem do
    (r :: (Array PerspectivesError)) <- loadCompileAndCacheArcFile "contextAndRole" testDirectory
    -- logShow r
    -- now add a Role
    ra <- getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
      (\c -> modifyContext_rolInContext c (EnumeratedRoleType "model:ContextAndRole$TestCase$Self") (flip union [(RoleInstance "blabla")])) >>=
        (pure <<< flip context_rolInContext (EnumeratedRoleType "model:ContextAndRole$TestCase$Self"))
    liftAff $ assert "There should be two instances of Self" (length ra == 2)

  test "Retrieve non-existing role instances" $ runP $ withSystem do
    (r :: (Array PerspectivesError)) <- loadCompileAndCacheArcFile "contextAndRole" testDirectory
    -- logShow r
    -- now add a Role
    ra <- getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
        (pure <<< flip context_rolInContext (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole"))
    liftAff $ assert "There should be an empty array of instances of Self" (length ra == 0)

  test "Add an array of Role instances to non-existing role instances" $ runP $ withSystem do
    (r :: (Array PerspectivesError)) <- loadCompileAndCacheArcFile "contextAndRole" testDirectory
    -- logShow r
    -- now add a Role
    ra <- getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
      (\c -> modifyContext_rolInContext c (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") (flip union [(RoleInstance "blabla")])) >>=
        (pure <<< flip context_rolInContext (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole"))
    liftAff $ assert "There should be one instance of Self" (length ra == 1)

  test "Add a Role instance to non-existing role instances" $ runP $ withSystem do
    (r :: (Array PerspectivesError)) <- loadCompileAndCacheArcFile "contextAndRole" testDirectory
    -- logShow r
    -- now add a Role
    ra <- getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
      (\c -> addContext_rolInContext c (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") (RoleInstance "blabla")) >>=
        (pure <<< flip context_rolInContext (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole"))
    liftAff $ assert "There should be one instance of Self" (length ra == 1)

  test "Role assignments" $ runP $ withSystem do
    (r :: (Array PerspectivesError)) <- loadCompileAndCacheArcFile "contextAndRole" testDirectory
      -- logShow r

    context1 <- getPerspectContext (ContextInstance "model:User$MyTestCase") >>=
      (\c -> addContext_rolInContext c (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") (RoleInstance "blabla"))
    liftAff $ assert "addContext_rolInContext fails" (length (context_rolInContext context1 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole")) == 1)

    context2 <- pure $ removeContext_rolInContext context1 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") (RoleInstance "blabla")
    liftAff $ assert "removeContext_rolInContext fails" (length (context_rolInContext context2 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole")) == 0)

    context4 <- pure $ deleteContext_rolInContext context2 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole")
    liftAff $ assert "deleteContext_rolInContext fails" (length (context_rolInContext context4 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole")) == 0)

    context5 <- modifyContext_rolInContext context4 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") (const [(RoleInstance "blabla")])
    liftAff $ assert "modifyContext_rolInContext fails" (length (context_rolInContext context5 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole")) == 1)

  test "Property assignments" $ runP $ withSystem do
    (r :: (Array PerspectivesError)) <- loadCompileAndCacheArcFile "contextAndRole" testDirectory
    -- logShow r

    role1 <- getPerspectRol (RoleInstance "model:User$MyTestCase$Self_0001") >>=
      (pure <<< \c -> addRol_property c (EnumeratedPropertyType "model:ContextAndRole$TestCase$Self$Prop1") [Value "1", Value "2"])
    liftAff $ assert "addRol_property fails" (length (rol_property role1 (EnumeratedPropertyType "model:ContextAndRole$TestCase$Self$Prop1")) == 2)

    role2 <- pure $ removeRol_property role1 (EnumeratedPropertyType "model:ContextAndRole$TestCase$Self$Prop1") [Value "2"]
    liftAff $ assert "removeRol_property fails" (length (rol_property role2 (EnumeratedPropertyType "model:ContextAndRole$TestCase$Self$Prop1")) == 1)

    role3 <- pure $ deleteRol_property role2 (EnumeratedPropertyType "model:ContextAndRole$TestCase$Self$Prop1")
    liftAff $ assert "deleteRol_property fails" (length (rol_property role3 (EnumeratedPropertyType "model:ContextAndRole$TestCase$Self$Prop1")) == 0)

    role4 <- pure $ setRol_property role3 (EnumeratedPropertyType "model:ContextAndRole$TestCase$Self$Prop1") [Value "1", Value "2"]
    liftAff $ assert "setRol_property fails" (length (rol_property role4 (EnumeratedPropertyType "model:ContextAndRole$TestCase$Self$Prop1")) == 2)

    role5 <- pure $ changeRol_binding (RoleInstance "blabla") role4
    liftAff $ assert "changeRol_binding fails" ((rol_binding role5 == Just (RoleInstance "blabla")))

    role6 <- pure $ removeRol_binding role4
    liftAff $ assert "removeRol_binding fails" ((rol_binding role6 == Nothing))

    role7 <- pure $ addRol_gevuldeRollen role6 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") (RoleInstance "blabla")
    liftAff $ assert "addRol_gevuldeRollen fails" ((rol_gevuldeRol role7 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") == [(RoleInstance "blabla")]))

    role8 <- pure $ removeRol_gevuldeRollen role7 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") (RoleInstance "blabla")
    liftAff $ assert "removeRol_gevuldeRollen fails" ((rol_gevuldeRol role8 (EnumeratedRoleType "model:ContextAndRole$TestCase$SomeRole") == []))
