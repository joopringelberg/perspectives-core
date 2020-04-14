module Test.ContextRoleParser where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (head, length)
import Data.Array.NonEmpty (singleton)
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Foreign.Object (Object, fromFoldable, lookup, empty)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (removeBinding, setBinding)
import Perspectives.ContextAndRole (context_me, rol_gevuldeRollen, rol_isMe)
import Perspectives.ContextRoleParser (expandedName, roleBindingByReference)
import Perspectives.CoreTypes ((##>>))
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.Identifiers (QualifiedName(..))
import Perspectives.IndentParser (runIndentParser)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.LoadCRL (loadAndCacheCrlFile)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Persistent (getPerspectRol, getPerspectContext)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (clearUserDatabase, runP, setupUser)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "ContextRoleParser" do
  test "inverse binding" do
    ra <- runP do
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
      (rl :: RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##>> getRole (EnumeratedRoleType "model:Test$TestCase$Self")
      getPerspectRol rl >>= pure <<< rol_gevuldeRollen
    assert "There should be two inverse bindings for model:Test$TestCase$NestedCase$NestedSelf" (Just 2 == (length <$> (lookup "model:Test$TestCase$NestedCase$NestedSelf" ra)))
    assert "There should be an inverse binding for model:Test$TestCase$NestedCase2$NestedSelf" (isJust (lookup "model:Test$TestCase$NestedCase2$NestedSelf" ra))

  test "Load both a model and instances" do
    r <- runP do
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    -- logShow r
    assert "A CRL file should load without problems" (isRight r)

  test "isMe for a role constructed with binding that represents the user." do
    ra <- runP do
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
      (rl :: RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##>> getRole (EnumeratedRoleType "model:Test$TestCase$Self")
      getPerspectRol rl
    assert "Self should have isMe == true" (rol_isMe ra)

  test "me for a context constructed with a role that represents the user." do
    c <- runP do
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
      getPerspectContext (ContextInstance "model:User$MyTestCase")
    assert "MyTestCase should have 'me' equal to model:User$MyTestCase$Self_0001" (context_me c == Just (RoleInstance "model:User$MyTestCase$Self_0001"))

  test "isMe for a role we bind with a role that represents the user." do
    ra <- runP do
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
      void $ runMonadPerspectivesTransaction $ setBinding
        (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
        (RoleInstance "model:User$test$User")
      getPerspectRol (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
    assert "Self should have isMe == true" (rol_isMe ra)
    (runP clearUserDatabase)

  test "me for a role we bind with a role that represents the user." do
    c <- runP do
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
      void $ runMonadPerspectivesTransaction $ setBinding
        (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
        (RoleInstance "model:User$test$User")
      getPerspectContext (ContextInstance "model:User$MyTestCase$MyNestedCase3")
    -- logShow c
    assert "MyNestedCase3 should have 'me' equal to model:User$MyTestCase$MyNestedCase3$NestedSelf_0001" (context_me c == Just (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001"))
    (runP clearUserDatabase)

  test "me for the context of a role from which we remove the binding." do
    c <- runP do
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
      void $ runMonadPerspectivesTransaction $ removeBinding false
        (RoleInstance "model:User$MyTestCase$MyNestedCase2$NestedSelf_0001")
      getPerspectContext (ContextInstance "model:User$MyTestCase$MyNestedCase3")
    assert "MyNestedCase2 should have 'me' equal to Nothing" (context_me c == Nothing)
    (runP clearUserDatabase)

  test "me for a constructed context (isMe by implication)." do
    c <- runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      _ <- runMonadPerspectivesTransaction $ constructContext $ ContextSerialization
        { id: "model:User$MyTestCase$MyNestedCase4"
        , prototype: Nothing
        , ctype: "model:Test$TestCase$NestedCase"
        , rollen: fromFoldable [Tuple "model:Test$TestCase$NestedCase$NestedSelf"
          (SerializableNonEmptyArray $ singleton (RolSerialization
            { id: Nothing
            , properties: PropertySerialization empty
            , binding: Just "model:User$test$User"}))]
        , externeProperties: PropertySerialization empty

      }
      c' <- getPerspectContext (ContextInstance "model:User$MyTestCase$MyNestedCase4")
      clearUserDatabase
      pure c'
    -- logShow c
    assert "MyNestedCase4 should have 'me' equal to model:User$MyTestCase$MyNestedCase4$NestedSelf_0000" (context_me c == Just (RoleInstance "model:User$MyTestCase$MyNestedCase4\
    \$NestedSelf_0000"))

  test "isMe for a constructed role." do
    ra <- runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      _ <- setupUser
      _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
      (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
      roleIdArray <- runMonadPerspectivesTransaction $ createAndAddRoleInstance (EnumeratedRoleType "model:Test$TestCase$NestedCase$NestedSelf")
        "model:User$MyTestCase$MyNestedCase4"
        (RolSerialization
            { id: Nothing
            , properties: PropertySerialization empty
            , binding: Just "model:User$test$User"})
      r' <- traverse getPerspectRol roleIdArray
      clearUserDatabase
      pure r'
    -- logShow ra
    case head ra of
      Nothing -> assert "No role created" false
      Just role -> assert "The constructed Role should have 'isMe' == true" (rol_isMe role)

  test "expandedName on role instance name" do
    (r :: Either ParseError QualifiedName) <- runP $ runIndentParser "model:User$test$User" expandedName
    case r of
      (Left e) -> assert (show e) false
      (Right id) -> do
        assert "'model:User$test$User' should be parsed as a valid identifier" (id == QualifiedName "model:User" "test$User")

  test "roleBindingByReference on role instance name" (runP (do
    (r :: Either ParseError ((Tuple RolName RoleInstance))) <- runIndentParser "$Tester -> model:User$test$User(1)" (roleBindingByReference (QualifiedName "model:User" "MyTests"))
    case r of
      (Left e) -> liftAff $ assert (show e) false
      (Right (Tuple rn ri)) -> do
        (PerspectRol{binding}) <- getPerspectRol ri
        -- logShow rn -- "model:System$Tester"
        -- logShow ri -- "model:User$MyTests$Tester_0001"
        -- logShow binding
        liftAff $ assert "In '$Tester -> model:User$test$User' the parser should recognise a role instance" (binding == Just (RoleInstance "model:User$test$User"))
))
