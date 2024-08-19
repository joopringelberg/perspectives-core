module Test.Perspectives.Utils where

import Prelude

import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError, try)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.DomeinCache (cascadeDeleteDomeinFile)
import Perspectives.Extern.Couchdb (addModelToLocalStore, addModelToLocalStore')
import Perspectives.Persistence.API (createDatabase, deleteDatabase)
import Perspectives.Persistence.State (getCouchdbBaseURL)
import Perspectives.Persistent (entitiesDatabaseName, postDatabaseName)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runSterileTransaction, runMonadPerspectivesTransaction')
import Perspectives.RunPerspectives (runPerspectives)
import Test.Unit.Assert as Assert

developmentRepository :: MonadPerspectives String
developmentRepository = pure "http://localhost:5984/repository"

developmentRepository_ :: String
developmentRepository_ = "http://localhost:5984/repository"

couchdbHost :: String
couchdbHost = "https://localhost"

couchdbPort :: Int
couchdbPort = 5984

runP_ :: forall a. String -> MonadPerspectives a -> Aff a
runP_ username = runPerspectives username "geheim" username username couchdbHost couchdbPort
-- "http://joopringelberg.nl/cbd/repository"

runP :: forall a. MonadPerspectives a -> Aff a
runP = runPerspectives "dev2" "geheim" "dev2" "dev21" couchdbHost couchdbPort

runTestadmin :: forall a. MonadPerspectives a -> Aff a
runTestadmin = runP_ "testadmin"

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

-- | Load the model, compute the value in MonadPerspectives, unload the model and remove the instances.
-- | Notice: dependencies of the model are not automatically removed!
withModel :: forall a. DomeinFileId -> MonadPerspectives a -> MonadPerspectives a
withModel m@(DomeinFileId id) a = withModel_ m true a

-- | Load the model, compute the value in MonadPerspectives, unload the model.
-- | Either removes instances, or lets them sit in the database.
withModel_ :: forall a. DomeinFileId -> Boolean -> MonadPerspectives a -> MonadPerspectives a
withModel_ m@(DomeinFileId id) clear a = do
  result <- try $ withModel' m a
  if clear then clearUserDatabase else pure unit
  case result of
    Left e -> throwError e
    Right r -> pure r

-- | Load the model, compute the value in MonadPerspectives, unload the model.
-- | Leaves instances from the computation in the database.
withModel' :: forall a. DomeinFileId -> MonadPerspectives a -> MonadPerspectives a
withModel' m@(DomeinFileId id) a = do
  mcdbUrl <- getCouchdbBaseURL
  case mcdbUrl of
    Just cdbUrl -> do
      void $ runSterileTransaction (addModelToLocalStore' (DomeinFileId $ cdbUrl <> "repository/" <> id))
      result <- try a
      void $ cascadeDeleteDomeinFile m
      case result of
        Left e -> throwError e
        Right r -> pure r
    Nothing -> throwError $ error "Expected a couchdb url"

withSystem :: forall a. MonadPerspectives a -> MonadPerspectives a
withSystem = withModel (DomeinFileId "model:System")

withSimpleChat :: forall a. MonadPerspectives a -> MonadPerspectives a
withSimpleChat = withModel (DomeinFileId "model:SimpleChat")

-- | Runs an update function (a function in MonadPerspectivesTransaction that produces deltas),
-- | runs actions as long as they are triggered, sends deltas to other participants and re-runs active queries
runMonadPerspectivesTransaction :: forall o.
  MonadPerspectivesTransaction o
  -> (MonadPerspectives (Array o))
runMonadPerspectivesTransaction a = singleton <$> runMonadPerspectivesTransaction' true (ENR $ EnumeratedRoleType "model:Perspectives$PerspectivesSystem$User") a
