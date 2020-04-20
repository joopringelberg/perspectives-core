module Test.Sync.Channel where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Writer (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes ((##>), (##>>))
import Perspectives.Couchdb.Databases (addDocument, createDatabase, deleteDatabase, endReplication, getDocument)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (externalRole, isMe)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Names (getUserIdentifier)
import Perspectives.Query.Compiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.Sync.Channel (addUserToChannel, createChannel, localReplication, postDbName, setChannelReplication, setMyAddress, setYourAddress)
import Perspectives.Sync.Transaction (Transaction, createTransactie)
import Test.Perspectives.Utils (runP, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Sync.Channel" do

  test "createChannel" $ runP $ withSystem do
    void $ runMonadPerspectivesTransaction createChannel

    -- There must be an instance of "model:System$PerspectivesSystem$Channels" in "model:User$test"
    -- If we can retrieve a value for the property model:System$PerspectivesSystem$User$Channel, everything is OK.
    getter <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
    me <- getUserIdentifier
    mdbname <- (RoleInstance me) ##> getter
    lift $ logShow mdbname
    liftAff $ assert "We should be able to calculate the value of the Channel property for `me`" (isJust mdbname)
    case mdbname of
      Nothing -> pure unit
      Just dbname -> deleteDatabase (unwrap dbname)

  test "create channel, add user" $ runP $ withSystem do
    achannel <- runMonadPerspectivesTransaction createChannel
    case head achannel of
      Nothing -> liftAff $ assert "Failed to create a channel" false
      Just channel -> do
        -- load a second user
        -- channelContext <- getPerspectEntiteit channel
        -- logShow channelContext
        void $ loadAndSaveCrlFile "userJoop.crl" testDirectory
        void $ runMonadPerspectivesTransaction $ addUserToChannel (RoleInstance "model:User$joop$User") channel

    getter <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
    mdbname <- RoleInstance "model:User$joop$User" ##> getter
    lift $ logShow mdbname

    liftAff $ assert "We should be able to calculate the value of the Channel property for `model:User$joop$User`" (isJust mdbname)

    -- Comment out to prepare for a test of Transaction distribution.
    case mdbname of
      Nothing -> pure unit
      Just dbname -> deleteDatabase (unwrap dbname)

  test "setMyAddress" $ runP $ withSystem do
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

  test "local replication" (runP do
    createDatabase "channel"
    createDatabase "post"
    (user :: String) <- getUserIdentifier
    localReplication "channel" "post" (Just user)
    t <- liftAff $ createTransactie user
    addDocument "channel" t "emptyTransaction"

    -- Wait a bit
    liftAff $ delay (Milliseconds 5000.0)
    -- Now retrieve the document
    (muser :: Maybe Transaction) <- getDocument "post" "emptyTransaction"
    liftAff $ assert "The 'emptyTransaction' document should now be in the post database!" (isJust muser)

    void $ endReplication "channel" "post"
    deleteDatabase "channel"
    deleteDatabase "post"
    )

  testSkip "endReplication" (runP do
    success <- endReplication "channel" "post"
    liftAff $ assert "It should be gone!" success)

  test "setChannelReplication" $ runP $ withSystem do
    achannel <- runMonadPerspectivesTransaction do
      channel <- createChannel
      void $ lift2 $ loadAndSaveCrlFile "userJoop.crl" testDirectory
      addUserToChannel (RoleInstance "model:User$joop$User") channel
      setYourAddress "http://127.0.0.1" 5984 channel
      -- We now have a channel with two partners.
      lift2 $ setChannelReplication channel
      -- Because both partners are local, we just replicate the channel to post.
      pure channel
    -- Now we test whether a document put into the channel appears in the post.
    case head achannel of
      Nothing -> liftAff $ assert "There should be a channel" false
      Just channel -> do
        getChannelId <- getPropertyFunction "model:System$Channel$External$ChannelDatabaseName"
        mchannel <- channel ##> externalRole >=> getChannelId
        case mchannel of
          Nothing -> pure unit
          Just (Value channelId) -> do
            t <- liftAff $ createTransactie "model:User$joop$User"
            addDocument channelId t "emptyTransaction"
            -- Wait a bit
            liftAff $ delay (Milliseconds 5000.0)
            -- Now retrieve the document
            post <- postDbName
            (mt :: Maybe Transaction) <- getDocument post "emptyTransaction"
            liftAff $ assert "The 'emptyTransaction' document should now be in the post database!" (isJust mt)

            -- Clean up: end replication, remove the channel, clear the user entities database
            void $ endReplication channelId post
            deleteDatabase channelId