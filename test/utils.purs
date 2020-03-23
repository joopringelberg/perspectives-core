module Test.Perspectives.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Perspectives.ContextAndRole (changeContext_me, changeRol_isMe)
import Perspectives.CoreTypes (MonadPerspectives, (##>>))
import Perspectives.Couchdb.Databases (createDatabase, deleteDatabase)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.LoadCRL (loadAndCacheCrlFile)
import Perspectives.Names (getMySystem)
import Perspectives.Persistent (entitiesDatabaseName, getPerspectContext, getPerspectRol)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.RunPerspectives (runPerspectives)
import Perspectives.SetupCouchdb (setupCouchdbForAnotherUser)
import Test.Unit.Assert as Assert


runP :: forall a.
  MonadPerspectives a ->
  Aff a
runP t = runPerspectives "test" "secret" "test" t

runPJoop :: forall a.
  MonadPerspectives a ->
  Aff a
runPJoop t = runPerspectives "joop" "geheim" "joop" t

p :: String -> String
p s = "model:Perspectives$" <> s

q :: String -> String
q s = "model:QueryAst$" <> s

u :: String -> String
u s = "model:User$" <> s

shouldEqual :: forall a. Eq a => a -> a -> Aff Boolean
shouldEqual a = \b -> pure (a == b)

type Message = String

assertEqual :: forall a. Eq a => Show a =>
  Message ->
  MonadPerspectives a ->
  a ->
  Aff Unit
assertEqual message test result = do
  r <- runP test
  case result == r of
    true -> Assert.assert message true
    false -> Assert.assert (message <> "\nExpected: " <>
      show result <> "\nReceived: " <>
      show r)
      false

clearUserDatabase :: MonadPerspectives Unit
clearUserDatabase = do
  userDatabaseName <- entitiesDatabaseName
  deleteDatabase userDatabaseName
  createDatabase userDatabaseName

setupCouchdbForTestUser :: MonadPerspectives Unit
setupCouchdbForTestUser = setupCouchdbForAnotherUser "test" "secret"

setupUser :: MonadPerspectives Unit
setupUser = setupUser_ "perspectivesSysteem.crl"

setupUser_ :: String -> MonadPerspectives Unit
setupUser_ userFile = do
  void $ loadAndCacheCrlFile userFile "./test"
  mySysteem <- getMySystem
  (user :: RoleInstance) <- ContextInstance mySysteem ##>> getRole (EnumeratedRoleType "model:System$PerspectivesSystem$User")
  (userRol :: PerspectRol) <- getPerspectRol user
  void $ cacheEntity user (changeRol_isMe userRol true)
  -- And set 'me' of mySystem
  (mijnSysteem :: PerspectContext) <- getPerspectContext (ContextInstance mySysteem)
  void $ cacheEntity (ContextInstance mySysteem) (changeContext_me mijnSysteem (Just user))
