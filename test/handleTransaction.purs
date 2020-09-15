module Test.Sync.HandleTransaction where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Free (Free)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Array (difference, head, length)
import Data.Maybe (Maybe(..))
import Effect.AVar (AVar)
import Effect.Aff (Milliseconds(..), delay, forkAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Perspectives.CollectAffectedContexts (lift2)
import Perspectives.CoreTypes (PerspectivesState, (##=), (##>), (##>>))
import Perspectives.Couchdb.ChangesFeed (EventSource, closeEventSource)
import Perspectives.Couchdb.Databases (deleteDatabase, documentNamesInDatabase, endReplication, getDocument)
import Perspectives.Instances.ObjectGetters (binding, context, externalRole, getEnumeratedRoleInstances, getRoleBinders)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Names (getMySystem)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.Sync.Channel (addPartnerToChannel, createChannel, localReplication)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.IncomingPost (incomingPost)
import Perspectives.User (getHost, getPort)
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (clearPostDatabase, runP, runPCor, runPJoop, withSystem, runMonadPerspectivesTransaction)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteSkip "Perspectives.Sync.HandleTransaction" do

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
          host <- getHost
          port <- getPort
          void $ runMonadPerspectivesTransaction $ addPartnerToChannel (RoleInstance "model:User$joop$User") channel host port
      getter <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
      RoleInstance "model:User$joop$User" ##> getter
      )
    (runPJoop $ withSystem do
      case mdbName of
        Nothing -> liftAff $ assert "There should be a channel" false
        Just (Value dbName) -> do
          -- get the document name
          transactionDocNames <- documentNamesInDatabase dbName
          case head transactionDocNames of
            Nothing -> liftAff $ assert "There should be a transaction document" false
            Just docName -> do
              -- NOTE. If this test fails, research getDocument. It has been refactored to throw errors when
              -- the database is approached without proper credentials, where previously it just yielded Nothing.
              mt <- getDocument dbName docName
              case mt of
                Nothing -> liftAff $ assert "There should be a transaction document" false
                Just t -> do
                  -- log $ prettyPrint t
                  executeTransaction t
                  -- Now check:
                  --  * there should be a channel document
                  --  * with two instances of model:System$Channel$ConnectedPartner
                  --  * bound to respectively "model:User$test$User" and "model:User$joop$User"
                  --  * `me` of that context should be the latter.
                  mySysteem <- getMySystem
                  (user :: RoleInstance) <- ContextInstance mySysteem ##>> getEnumeratedRoleInstances (EnumeratedRoleType "model:System$PerspectivesSystem$User")
                  mchannel <- user ##> (getRoleBinders (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> context)
                  case mchannel of
                    Nothing -> liftAff $ assert "There should be a channel on this side" false
                    Just channel -> do
                      connectedPartners <- channel ##= (getEnumeratedRoleInstances (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> binding)
                      logShow connectedPartners
                      liftAff $ assert "The user of model:System$test and of model:System$joop should be the binding of the ConnectedPartners" ((length $ difference connectedPartners (RoleInstance <$> ["model:User$test$User","model:User$joop$User"])) == 0)
          deleteDatabase dbName
    )

  test "create channel between two users, add user on one side, check for channel context on the other side" do
    channelId <- runPCor $ withSystem do
      (channelA :: Array ContextInstance) <- runMonadPerspectivesTransaction do
        channel <- createChannel
        void $ lift2 $ loadAndSaveCrlFile "userJoop.crl" testDirectory
        host <- lift2 getHost
        port <- lift2 getPort
        addPartnerToChannel (RoleInstance "model:User$joop$User") channel host port
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
      (pstate :: AVar PerspectivesState) <- ask
      -- Handle post in parallel
      -- TODO. Dit proces stopt niet, ondanks killFiber
      log "1"
      postFiber <- lift $ forkAff (runPerspectivesWithState incomingPost pstate)
      log "2"
      -- Wait a little
      liftAff $ delay (Milliseconds 8000.0)

      -- Check if there is a channel document, starting with the user.
      mySysteem <- getMySystem
      (user :: RoleInstance) <- ContextInstance mySysteem ##>> getEnumeratedRoleInstances (EnumeratedRoleType "model:System$PerspectivesSystem$User")
      mchannel <- user ##> (getRoleBinders (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> context)
      case mchannel of
        Nothing -> liftAff $ assert "There should be a channel on this side" false
        Just channel -> do
          connectedPartners <- channel ##= (getEnumeratedRoleInstances (EnumeratedRoleType "model:System$Channel$ConnectedPartner") >=> binding)
          logShow connectedPartners
          liftAff $ assert "The user of model:System$test and of model:System$joop should be the binding of the ConnectedPartners" ((length $ difference connectedPartners (RoleInstance <$> ["model:User$cor$User","model:User$joop$User"])) == 0)

      -- Clean up
      -- lift $ killFiber (error "Stop") postFiber
      (post :: Maybe EventSource) <- gets _.post
      case post of
        Nothing -> do
          pure unit
        Just p -> do
          liftEffect $ closeEventSource p
      -- clear the post database
      clearPostDatabase

      case channelId of
        Nothing -> pure unit
        Just c -> do
          deleteDatabase c
          void $ endReplication c "joop_post"
