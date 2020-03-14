module Test.Sync.Channel where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Free (Free)
import Control.Monad.Writer (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes ((##>), (##>>))
import Perspectives.Couchdb.Databases (addDocument, createDatabase, deleteDatabase, deleteDocument, endReplication, getDocument)
import Perspectives.CouchdbState (CouchdbUser(..))
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (externalRole, isMe)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Query.Compiler (getPropertyFunction, getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.Sync.Channel (addUserToChannel, createChannel, localReplication, postDbName, setChannelReplication, setMyAddress, setYourAddress)
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile')
import Perspectives.User (getUser)
import Test.Perspectives.Utils (clearUserDatabase, runP, setupUser)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Sync.Channel" do

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

  test "setMyAddress" (runP do
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

  test "local replication" (runP do
    createDatabase "channel"
    createDatabase "post"
    localReplication "channel" "post" Nothing
    (user :: CouchdbUser) <- gets $ _.userInfo
    addDocument "channel" user "user"

    -- Wait a bit
    liftAff $ delay (Milliseconds 5000.0)
    -- Now retrieve the document
    (muser :: Maybe CouchdbUser) <- getDocument "post" "user"
    liftAff $ assert "The 'user' document should now be in the post database!" (isJust muser)

    void $ endReplication "channel" "post"
    deleteDatabase "channel"
    deleteDatabase "post"
    )

  test "endReplication" (runP do
    success <- endReplication "channel" "post"
    liftAff $ assert "It should be gone!" success)

  test "setChannelReplication" (runP do
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    setupUser
    achannel <- runMonadPerspectivesTransaction do
      channel <- createChannel
      void $ lift2 $ loadAndSaveCrlFile "userJoop.crl" testDirectory
      addUserToChannel (RoleInstance "model:User$JoopsSysteem$User_0001") channel
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
            (user :: CouchdbUser) <- gets $ _.userInfo
            addDocument channelId user "user"
            -- Wait a bit
            liftAff $ delay (Milliseconds 5000.0)
            -- Now retrieve the document
            post <- postDbName
            (muser :: Maybe CouchdbUser) <- getDocument post "user"
            liftAff $ assert "The 'user' document should now be in the post database!" (isJust muser)

            -- Clean up: end replication, remove the channel, clear the user entities database
            void $ endReplication channelId post
            deleteDatabase channelId
            clearUserDatabase
    )
