module Test.Sync.HandleTransaction where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Array (difference, head, length)
import Data.Maybe (Maybe(..))
import Effect.AVar (AVar)
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (PerspectivesState, (##=), (##>), (##>>))
import Perspectives.Couchdb.Databases (deleteDatabase, documentNamesInDatabase, endReplication, getDocument)
import Perspectives.Instances.ObjectGetters (binding, context, externalRole, getRole, getRoleBinders)
import Perspectives.LoadCRL (loadAndCacheCrlFile_, loadAndSaveCrlFile)
import Perspectives.Names (getMySystem)
import Perspectives.Query.Compiler (getPropertyFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.Sync.Channel (addUserToChannel, createChannel, localReplication)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.IncomingPost (incomingPost)
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (clearPostDatabase, clearUserDatabase, runP, runPCor, runPJoop, setupUser, setupUser_, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Sync.HandleTransaction" do

  test "create channel, add user, check for channel on the other side" do
    mdbName <- (runP $ withSystem do
      achannel <- runMonadPerspectivesTransaction createChannel
      case head achannel of
        Nothing -> liftAff $ assert "Failed to create a channel" false
        Just channel -> do
          -- channelContext <- getPerspectEntiteit channel
          -- logShow channelContext
          -- load a second user
          void $ loadAndSaveCrlFile "userJoop.crl" testDirectory
          void $ runMonadPerspectivesTransaction $ addUserToChannel (RoleInstance "model:User$joop$User") channel
      getter <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
      RoleInstance "model:User$joop$User" ##> getter
      )
    (runPJoop $ withSystem do
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
                  --  * bound to respectively "model:User$test$User" and "model:User$joop$User"
                  --  * `me` of that context should be the latter.
                  mySysteem <- getMySystem
                  (user :: RoleInstance) <- ContextInstance mySysteem ##>> getRole (EnumeratedRoleType "model:System$PerspectivesSystem$User")
                  mchannel <- user ##> (getRoleBinders (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> context)
                  case mchannel of
                    Nothing -> liftAff $ assert "There should be a channel on this side" false
                    Just channel -> do
                      connectedPartners <- channel ##= (getRole (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> binding)
                      logShow connectedPartners
                      liftAff $ assert "The user of model:System$test and of model:System$joop should be the binding of the ConnectedPartners" ((length $ difference connectedPartners (RoleInstance <$> ["model:User$test$User","model:User$joop$User"])) == 0)
          deleteDatabase dbName
    )

  test "create channel between two users, add user on one side, check for channel context on the other side" do
    channelId <- runPCor $ withSystem do
      (channelA :: Array ContextInstance) <- runMonadPerspectivesTransaction do
        channel <- createChannel
        void $ lift2 $ loadAndSaveCrlFile "userJoop.crl" testDirectory
        addUserToChannel (RoleInstance "model:User$joop$User") channel
        -- setYourAddress "http://127.0.0.1" 5984 channel
        -- We now have a channel with two partners.
        pure channel
      case head channelA of
        Nothing -> (liftAff $ assert "There should be a channel context" false) *> pure Nothing
        Just channel -> do
          getChannelId <- getPropertyFunction "model:System$Channel$External$ChannelDatabaseName"
          mchannel <- channel ##> externalRole >=> getChannelId
          case mchannel of
            Nothing -> (liftAff $ assert "There should be a channel identifier" false) *> pure Nothing
            Just (Value channelId) -> do
              -- We have to artificially replicate the channel to the post of Joop,
              -- replicating just transactions coming from Cor.
              localReplication channelId "joop_post" (Just "model:User$cor$User")
              pure $ Just channelId
    runPJoop $ withSystem do
      -- Without this, the test user is unknown at Joop's side.
      -- void $ loadAndCacheCrlFile_ "userCor.crl" testDirectory
      (pstate :: AVar PerspectivesState) <- ask
      -- Handle post in parallel
      -- TODO. Dit proces stopt niet, ondanks killFiber
      postFiber <- lift $ forkAff (runPerspectivesWithState incomingPost pstate)
      -- Wait a little
      liftAff $ delay (Milliseconds 8000.0)
      -- Check if there is a channel document, starting with the user.
      mySysteem <- getMySystem
      (user :: RoleInstance) <- ContextInstance mySysteem ##>> getRole (EnumeratedRoleType "model:System$PerspectivesSystem$User")
      mchannel <- user ##> (getRoleBinders (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> context)
      case mchannel of
        Nothing -> liftAff $ assert "There should be a channel on this side" false
        Just channel -> do
          connectedPartners <- channel ##= (getRole (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> binding)
          logShow connectedPartners
          liftAff $ assert "The user of model:System$test and of model:System$joop should be the binding of the ConnectedPartners" ((length $ difference connectedPartners (RoleInstance <$> ["model:User$cor$User","model:User$joop$User"])) == 0)

      -- Clean up
      lift $ killFiber (error "Stop") postFiber
      -- clear the post database
      clearPostDatabase

      case channelId of
        Nothing -> pure unit
        Just c -> do
          deleteDatabase c
          void $ endReplication c "joop_post"
