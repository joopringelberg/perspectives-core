module Test.Sync.Channel where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Writer (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CoreTypes ((##>))
import Perspectives.Couchdb.Databases (deleteDatabase)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Query.Compiler (getPropertyFunction)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.Sync.Channel (addUserToChannel, createChannel)
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile')
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
    -- case mdbname of
    --   Nothing -> pure unit
    --   Just dbname -> deleteDatabase (unwrap dbname)
    -- clearUserDatabase

    )
