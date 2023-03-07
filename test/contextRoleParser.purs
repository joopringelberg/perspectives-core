module Test.ContextRoleParser where

import Prelude

import Control.Monad.Except (runExceptT) 
import Control.Monad.Free (Free)
import Data.Array (catMaybes, head, length, null)
import Data.Array.NonEmpty (singleton)
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object, fromFoldable, lookup, empty)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.ContextAndRole (context_me, rol_gevuldeRol, rol_gevuldeRollen, rol_isMe)
import Perspectives.ContextRoleParser (expandedName, roleBindingByReference)
import Perspectives.CoreTypes ((##>>))
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.Identifiers (QualifiedName(..))
import Perspectives.IndentParser (runIndentParser)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.LoadCRL.FS (loadAndCacheCrlFile)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Persistent (getPerspectRol, getPerspectContext)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Perspectives.SaveUserData (removeBinding, setBinding)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (runP, withSystem, runMonadPerspectivesTransaction)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)
import Text.Parsing.Parser (ParseError)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "ContextRoleParser" do

  test "inverse binding" $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    (rl :: RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##>> getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCase$Self")
    ra <- getPerspectRol rl
    liftAff $ assert "There should be two inverse bindings for model:Test$TestCase$NestedCase$NestedSelf" (2 == (length $ _.instances $ rol_gevuldeRol ra (ContextType "model:Test$TestCase$NestedCase") (EnumeratedRoleType "model:Test$TestCase$NestedCase$NestedSelf")))

    liftAff $ assert "There should be an inverse binding for model:Test$TestCase$NestedCase2$NestedSelf" (not $ null $ _.instances $ rol_gevuldeRol ra (ContextType "model:Test$TestCase$NestedCase2") (EnumeratedRoleType "model:Test$TestCase$NestedCase2$NestedSelf"))


  test "Load both a model and instances" $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    r <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    liftAff $ assert "A CRL file should load without problems" (isRight r)

  test "isMe for a role constructed with binding that represents the user." $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    (rl :: RoleInstance) <- (ContextInstance "model:User$MyTestCase") ##>> getEnumeratedRoleInstances (EnumeratedRoleType "model:Test$TestCase$Self")
    ra <- getPerspectRol rl
    liftAff $ assert "Self should have isMe == true" (rol_isMe ra)

  test "me for a context constructed with a role that represents the user." $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    c <- getPerspectContext (ContextInstance "model:User$MyTestCase")
    liftAff $ assert "MyTestCase should have 'me' equal to model:User$MyTestCase$Self_0001" (context_me c == Just (RoleInstance "model:User$MyTestCase$Self_0001"))

  test "isMe for a role we bind with a role that represents the user." $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    void $ runMonadPerspectivesTransaction $ setBinding
      (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
      (RoleInstance "model:User$test$User")
      Nothing
    ra <- getPerspectRol (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
    liftAff $ assert "Self should have isMe == true" (rol_isMe ra)

  test "me for a role we bind with a role that represents the user." $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    void $ runMonadPerspectivesTransaction $ setBinding
      (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001")
      (RoleInstance "model:User$test$User")
      Nothing
    c <- getPerspectContext (ContextInstance "model:User$MyTestCase$MyNestedCase3")
    liftAff $ assert "MyNestedCase3 should have 'me' equal to model:User$MyTestCase$MyNestedCase3$NestedSelf_0001" (context_me c == Just (RoleInstance "model:User$MyTestCase$MyNestedCase3$NestedSelf_0001"))

  test "me for the context of a role from which we remove the binding." $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    void $ runMonadPerspectivesTransaction $ removeBinding
      (RoleInstance "model:User$MyTestCase$MyNestedCase2$NestedSelf_0001")
    c <- getPerspectContext (ContextInstance "model:User$MyTestCase$MyNestedCase3")
    liftAff $ assert "MyNestedCase2 should have 'me' equal to Nothing" (context_me c == Nothing)

  test "me for a constructed context (isMe by implication)." $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    _ <- runMonadPerspectivesTransaction $ runExceptT $ constructContext Nothing $ ContextSerialization
      { id: Just "model:User$MyTestCase$MyNestedCase4"
      , prototype: Nothing
      , ctype: "model:Test$TestCase$NestedCase"
      , rollen: fromFoldable [Tuple "model:Test$TestCase$NestedCase$NestedSelf"
        (SerializableNonEmptyArray $ singleton (RolSerialization
          { id: Nothing
          , properties: PropertySerialization empty
          , binding: Just "model:User$test$User"}))]
      , externeProperties: PropertySerialization empty

    }
    c <- getPerspectContext (ContextInstance "model:User$MyTestCase$MyNestedCase4")
    liftAff $ assert "MyNestedCase4 should have 'me' equal to model:User$MyTestCase$MyNestedCase4$NestedSelf_0000" (context_me c == Just (RoleInstance "model:User$MyTestCase$MyNestedCase4\
    \$NestedSelf_0000"))

  test "isMe for a constructed role." $ runP $ withSystem do
    _ <- loadCompileAndCacheArcFile' "contextRoleParser" testDirectory
    (r :: Either (Array PerspectivesError) (Tuple (Object PerspectContext)(Object PerspectRol))) <- loadAndCacheCrlFile "contextRoleParser.crl" testDirectory
    roleIdArray <- runMonadPerspectivesTransaction $ createAndAddRoleInstance (EnumeratedRoleType "model:Test$TestCase$NestedCase$NestedSelf")
      "model:User$MyTestCase$MyNestedCase4"
      (RolSerialization
          { id: Nothing
          , properties: PropertySerialization empty
          , binding: Just "model:User$test$User"})
    ra <- traverse getPerspectRol (catMaybes roleIdArray)
    case head ra of
      Nothing -> liftAff $ assert "No role created" false
      Just role -> liftAff $ assert "The constructed Role should have 'isMe' == true" (rol_isMe role)

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
        liftAff $ assert "In '$Tester -> model:User$test$User' the parser should recognise a role instance" (binding == Just (RoleInstance "model:User$test$User_0001"))
))
