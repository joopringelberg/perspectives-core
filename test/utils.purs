module Test.Perspectives.Utils where

import Prelude

import Effect.Aff (Aff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb.Databases (createDatabase, deleteDatabase)
import Perspectives.DomeinFile (DomeinFileId(..))
import Perspectives.Extern.Couchdb (addModelToLocalStore)
import Perspectives.Persistent (entitiesDatabaseName, postDatabaseName, removeEntiteit)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.RunMonadPerspectivesTransaction (runSterileTransaction)
import Perspectives.RunPerspectives (runPerspectives)
import Perspectives.SetupCouchdb (setupCouchdbForAnotherUser)
import Perspectives.User (getCouchdbBaseURL)
import Test.Unit.Assert as Assert


runP_ :: forall a. String -> MonadPerspectives a -> Aff a
runP_ username = runPerspectives username "geheim" username

runP :: forall a. MonadPerspectives a -> Aff a
runP = runP_ "test"

runPJoop :: forall a. MonadPerspectives a -> Aff a
runPJoop = runP_ "joop"

runPCor :: forall a. MonadPerspectives a -> Aff a
runPCor = runP_ "cor"

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

clearPostDatabase :: MonadPerspectives Unit
clearPostDatabase = do
  db <- postDatabaseName
  deleteDatabase db
  createDatabase db

setupCouchdbForTestUser :: MonadPerspectives Unit
setupCouchdbForTestUser = setupCouchdbForAnotherUser "test" "geheim"

-- | Load the model, compute the value in MonadPerspectives, unload the model and remove the instances.
-- | Notice: dependencies of the model are not automatically removed!
withModel :: forall a. DomeinFileId -> MonadPerspectives a -> MonadPerspectives a
withModel m@(DomeinFileId id) a = do
  result <- withModel' m a
  clearUserDatabase
  pure result

-- | Load the model, compute the value in MonadPerspectives, unload the model.
withModel' :: forall a. DomeinFileId -> MonadPerspectives a -> MonadPerspectives a
withModel' m@(DomeinFileId id) a = do
  cdbUrl <- getCouchdbBaseURL
  void $ runSterileTransaction (addModelToLocalStore [cdbUrl <> "repository/" <> id] (RoleInstance ""))
  result <- a
  void $ removeEntiteit m
  pure result

withSystem :: forall a. MonadPerspectives a -> MonadPerspectives a
withSystem = withModel (DomeinFileId "model:System")

withSimpleChat :: forall a. MonadPerspectives a -> MonadPerspectives a
withSimpleChat = withModel (DomeinFileId "model:SimpleChat")
