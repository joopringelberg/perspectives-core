module Test.Sync.HandleTransaction where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (difference, head, length)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.CoreTypes ((##>), (##>>), (##=))
import Perspectives.Couchdb.Databases (deleteDatabase, documentNamesInDatabase, getDocument)
import Perspectives.Instances.ObjectGetters (binding, context, getRole, getRoleBinders)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Names (getMySystem)
import Perspectives.Query.Compiler (getPropertyFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.Sync.Channel (addUserToChannel, createChannel)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (clearUserDatabase, runP, runPJoop, setupUser, setupUser_)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.Sync.HandleTransaction" do

  test "create channel, add user, check for channel on the other side" do
    mdbName <- (runP do
      _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
      setupUser
      achannel <- runMonadPerspectivesTransaction createChannel
      case head achannel of
        Nothing -> liftAff $ assert "Failed to create a channel" false
        Just channel -> do
          -- load a second user
          -- channelContext <- getPerspectEntiteit channel
          -- logShow channelContext
          void $ loadAndSaveCrlFile "userJoop.crl" testDirectory
          void $ runMonadPerspectivesTransaction $ addUserToChannel (RoleInstance "model:User$joop$User_0001") channel
      getter <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
      mdbName <- RoleInstance "model:User$joop$User_0001" ##> getter
      clearUserDatabase
      pure mdbName
      )
    (runPJoop do
      setupUser_ "userJoop.crl"
      case mdbName of
        Nothing -> liftAff $ assert "There should be a channel" false
        Just (Value dbName) -> do
          _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
          setupUser
          -- get the document name
          transactionDocNames <- documentNamesInDatabase dbName
          case head transactionDocNames of
            Nothing -> liftAff $ assert "There should be a transaction document" false
            Just docName -> do
              mt <- getDocument dbName docName
              case mt of
                Nothing -> liftAff $ assert "There should be a transaction document" false
                Just t -> do
                  executeTransaction t
                  -- Now check:
                  --  * there should be a channel document
                  --  * with two instances of model:System$Channel$ConnectedPartner
                  --  * bound to respectively "model:User$test$User_0001" and "model:User$joop$User_0001"
                  --  * `me` of that context should be the latter.
                  mySysteem <- getMySystem
                  (user :: RoleInstance) <- ContextInstance mySysteem ##>> getRole (EnumeratedRoleType "model:System$PerspectivesSystem$User")
                  mchannel <- user ##> (getRoleBinders (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> context)
                  case mchannel of
                    Nothing -> liftAff $ assert "There should be a channel on this side" false
                    Just channel -> do
                      connectedPartners <- channel ##= (getRole (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> binding)
                      logShow connectedPartners
                      liftAff $ assert "The user of model:System$test and of model:System$joop should be the binding of the ConnectedPartners" ((length $ difference connectedPartners (RoleInstance <$> ["model:User$test$User_0001","model:User$joop$User_0001"])) == 0)
          clearUserDatabase
          deleteDatabase dbName
    )
