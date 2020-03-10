module Test.Sync.Channel where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Writer (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CoreTypes ((##>), (##>>))
import Perspectives.Couchdb.Databases (deleteDatabase)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (externalRole, isMe)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Query.Compiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.Sync.Channel (addUserToChannel, createChannel, setMyAddress)
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (clearUserDatabase, runP, setupUser)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.Sync.Channel" do

  test "createChannel" (runP do
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    setupUser
    void $ runMonadPerspectivesTransaction createChannel

    -- There must be an instance of "model:System$PerspectivesSystem$Channels" in "model:User$MijnSysteem"
    -- If we can retrieve a value for the property model:System$PerspectivesSystem$User$Channel, everything is OK.
    getter <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
    mdbname <- RoleInstance "model:User$MijnSysteem$User_0001" ##> getter
    lift $ logShow mdbname
    liftAff $ assert "We should be able to calculate the value of the Channel property for `me`" (isJust mdbname)
    case mdbname of
      Nothing -> pure unit
      Just dbname -> deleteDatabase (unwrap dbname)
    clearUserDatabase
    )

  test "create channel, add user" (runP do
    -- _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    -- setupUser
    achannel <- runMonadPerspectivesTransaction createChannel
    case head achannel of
      Nothing -> liftAff $ assert "Failed to create a channel" false
      Just channel -> do
        -- load a second user
        void $ loadAndSaveCrlFile "userJoop.crl" testDirectory
        void $ runMonadPerspectivesTransaction $ addUserToChannel (RoleInstance "model:User$JoopsSysteem$User_0001") channel

    getter <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
    mdbname <- RoleInstance "model:User$JoopsSysteem$User_0001" ##> getter
    lift $ logShow mdbname

    liftAff $ assert "We should be able to calculate the value of the Channel property for `model:User$JoopsSysteem$User_0001`" (isJust mdbname)

    -- Comment out to prepare for a test of Transaction distribution.
    case mdbname of
      Nothing -> pure unit
      Just dbname -> deleteDatabase (unwrap dbname)
    clearUserDatabase
    )

  testOnly "setMyAddress" (runP do
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    setupUser
    achannel <- runMonadPerspectivesTransaction createChannel
    case head achannel of
      Nothing -> liftAff $ assert "Failed to create a channel" false
      Just channel -> do
        void $ runMonadPerspectivesTransaction (setMyAddress "localhost" 5984 channel)
        connectedPartner <- (getRoleFunction "sys:Channel$ConnectedPartner")
        me <- (channel ##>> filter connectedPartner (lift <<< lift <<< isMe))
        host <- getPropertyFunction "sys:Channel$ConnectedPartner$Host"
        hostValue <- me ##> host
        liftAff $ assert "Host should be 'localhost'" (maybe false ((==) (Value "localhost")) hostValue)
        port <- getPropertyFunction "sys:Channel$ConnectedPartner$Port"
        portValue <- me ##> port
        liftAff $ assert "Port should be '5984'" (maybe false ((==) (Value "5984")) portValue)

        getChannelId <- getPropertyFunction "model:System$Channel$External$ChannelDatabaseName"
        (Value channelId) <- channel ##>> externalRole >=> getChannelId
        deleteDatabase channelId
        clearUserDatabase
    )
