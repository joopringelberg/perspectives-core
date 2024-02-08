module Test.RabbitMQ where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (head)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Effect.Aff.Class (liftAff)
import Partial.Unsafe (unsafePartial)
import Perspectives.AMQP.RabbitMQManagement (RabbitState, createQueue, createUser, deleteQueue, deleteUser, getNodes, getUser, runRabbitState', setPermissions)
import Perspectives.Extern.RabbitMQ (deleteAMQPaccount, deleteQueue_, prepareAMQPaccount, setBindingKey)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Test.Perspectives.Utils (runMonadPerspectivesTransaction, runP)
import Test.Unit (TestF, suite, test, testSkip)
import Test.Unit.Assert (assert)

-- ATTENTION: in order to run this test, evaluate the line below first, on the command line:
--            export NODE_TLS_REJECT_UNAUTHORIZED="0"
-- This disables certificate checking.

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

rabbitState :: RabbitState
rabbitState = 
  { virtualHost: "mycontexts"
  , brokerServiceUrl: "https://mycontexts.com/rbmq/"
  , adminUserName: "joopring"
  , adminPassword: "dOnkered1g"
  }

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.AMQP.RabbitMQManagement" do

  testSkip "Retrieve the nodes" $ runP do
    nodes <- runRabbitState' rabbitState getNodes
    liftAff $ assert "The only node should be ''" ("rabbit@ubuntu-1cpu-1gb-nl-ams1" == (unsafePartial fromJust $ head nodes))
  
  testSkip "Remove a user account" $ runP do 
    runRabbitState' rabbitState (deleteUser "rabbit_test_user")
  
  testSkip "Retrieve a user account" $ runP do
    mUser <- runRabbitState' rabbitState (getUser "rabbit_test_user")
    liftAff $ assert "Expected a user with the name 'rabbit_test_user'" (case mUser of
      Nothing -> false
      Just {name} -> name == "rabbit_test_user") 
  
  testSkip "Add and delete a user account" $ runP do
    runRabbitState' rabbitState (do
      createUser {password: "verysecret", tags: ""} "rabbit_test_user"
      deleteUser "rabbit_test_user"
      mUser <- getUser "rabbit_test_user"
      liftAff $ assert "There should be no user with the name 'rabbit_test_user'" (isNothing mUser)
      )

  testSkip "Add a queue" $ runP do
    runRabbitState' rabbitState (createQueue "rabbit_test_queue")

  testSkip "Delete a queue" $ runP do
    runRabbitState' rabbitState (deleteQueue "rabbit_test_queue")
  
  testSkip "Create user and set permissions" $ runP do
    runRabbitState' rabbitState do
      createUser {password: "verysecret", tags: ""} "rabbit_test_user"
      setPermissions "rabbit_test_user" {configure: "rabbit_test_queue", write: "rabbit_test_queue|amq\\.topic", read: "rabbit_test_queue|amq\\.topic"}
      deleteUser "rabbit_test_user"
  
  test "Perspectives.Extern.RabbitMQ.prepareAMQPaccount" $ runP $ do 
    void $ runMonadPerspectivesTransaction do
      prepareAMQPaccount 
        ["https://mycontexts.com/rbmq/"]
        ["joopring"]
        ["dOnkered1g"]
        ["rabbit_test_user"]
        ["geheim"]
        ["rabbit_test_queue"]
        (RoleInstance "ignore")
    runRabbitState' rabbitState do
      mUser <- getUser "rabbit_test_user"
      liftAff $ assert "There should be no user with the name 'rabbit_test_user'" (isJust mUser)

  test "Perspectives.Extern.RabbitMQ.setBindingKey" $ runP $ do
    void $ runMonadPerspectivesTransaction $
      setBindingKey
        ["https://mycontexts.com/rbmq/"]
        ["rabbit_test_user"]
        ["geheim"]
        ["rabbit_test_queue"]
        ["def:#rabbit_test_user$User"]
        (RoleInstance "ignore")

  test "Perspectives.Extern.RabbitMQ.deleteAMQPaccount" $ runP $ do
    void $ runMonadPerspectivesTransaction $
      deleteAMQPaccount 
        ["https://mycontexts.com/rbmq/"]
        ["joopring"]
        ["dOnkered1g"]
        ["rabbit_test_user"]
        (RoleInstance "ignore")
 
  test "Perspectives.Extern.RabbitMQ.deleteQueue" $ runP $ do
    void $ runMonadPerspectivesTransaction $
      deleteQueue_
        ["https://mycontexts.com/rbmq/"]
        ["joopring"]
        ["dOnkered1g"]
        ["rabbit_test_queue"]
        (RoleInstance "ignore")
 