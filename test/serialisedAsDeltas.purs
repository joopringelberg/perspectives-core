module Test.SerialisedAsDeltas where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (head, null)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.CoreTypes ((##>>))
import Perspectives.Persistence.API (deleteDatabase)
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.LoadCRL.FS (loadAndSaveCrlFile)
import Perspectives.Persistence.State (withCouchdbUrl)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter, getRoleFunction)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.SaveUserData (handleNewPeer, setBinding)
import Perspectives.Sync.Channel (addPartnerToChannel, createChannel)
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile)
import Perspectives.User (getHost, getPort)
import Test.Perspectives.Utils (clearUserDatabase, runP, withSystem, runMonadPerspectivesTransaction)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "SerialisedAsDeltas" do

  -- test "Bind a user to a role in a context" (runP $ withModel' (DomeinFileId "model:System") $ do
  test "Bind a user to a role in a context" (runP $ withSystem $ do
    -- load userdata.
    modelErrors <- loadCompileAndCacheArcFile "serialisedAsDeltas" testDirectory
    if not $ null modelErrors
      then do
        logShow modelErrors
        liftAff $ assert "There are modelErrors" false
      else pure unit
    machannel <- withCouchdbUrl \url -> runMonadPerspectivesTransaction (createChannel url)
    case machannel of
      Nothing -> liftAff $ assert "Failed to create a channel" false
      Just achannel -> case head achannel of
        Nothing -> liftAff $ assert "Failed to create a channel" false
        Just channel -> do
          -- load a second user
          void $ loadAndSaveCrlFile "userJoop.crl" testDirectory
          void $ runMonadPerspectivesTransaction $ addPartnerToChannel (RoleInstance "model:User$joop$User") channel

    getter <- getRoleFunction "model:Test$TestCase$Other"
    unboundOtherRole <- (ContextInstance "model:User$MyTestCase") ##>> getter
    userGetter <- getRoleFunction "model:System$PerspectivesSystem$User"
    joop <- (ContextInstance "model:User$joop") ##>> userGetter
    void $ runMonadPerspectivesTransaction do
      handleNewPeer unboundOtherRole
      void $ setBinding unboundOtherRole joop Nothing
      -- t@(Transaction{universeContextDeltas, universeRoleDeltas, contextDeltas, roleDeltas, propertyDeltas}) <- lift get
      -- -- log $ prettyPrint t
      --
      -- x1 <- pure $ find
      --   (\(UniverseContextDelta{contextType, sequenceNumber}) -> contextType == (ContextType "model:Test$TestCase") && sequenceNumber == 0)
      --   universeContextDeltas
      -- case x1 of
      --   Nothing -> liftAff $ assert "There should be a UniverseContextDelta with model:Test$TestCase and sequenceNumber 0" false
      --   Just _ -> liftAff $ assert "There should be 3 UniverseContextDeltas" (length universeContextDeltas == 3)
      --
      -- x2 <- pure $ find
      --   (\(UniverseRoleDelta{roleType, sequenceNumber}) -> roleType == (EnumeratedRoleType "model:Test$TestCase$ARole") && sequenceNumber == 2)
      --   universeRoleDeltas
      -- case x2 of
      --   Nothing -> liftAff $ assert "There should be a UniverseRoleDelta for model:Test$TestCase$ARole and sequenceNumber 0" false
      --   Just _ -> liftAff $ assert "There should be 8 UniverseRoleDeltas" (length universeRoleDeltas == 8)
      --
      -- x3 <- pure $ find
      --   (\(RolePropertyDelta{property, sequenceNumber}) -> (property == (EnumeratedPropertyType "model:Test$TestCase$ARole$Prop1") && sequenceNumber == 3))
      --   propertyDeltas
      -- case x3 of
      --   Nothing -> liftAff $ assert "There should be a RolePropertyDelta for model:Test$TestCase$ARole$Prop1 and sequenceNumber 3" false
      --   Just _ -> liftAff $ assert "There should be 8 RolePropertyDeltas" (length propertyDeltas == 7)
      --
      -- x4 <- pure $ find
      --   (\(ContextDelta{roleType, sequenceNumber}) -> (roleType == (EnumeratedRoleType "model:Test$TestCase$ARole") && sequenceNumber == 4))
      --   contextDeltas
      -- case x4 of
      --   Nothing -> liftAff $ assert "There should be a ContextDelta for model:Test$TestCase$ARole and sequenceNumber 4" false
      --   Just _ -> liftAff $ assert "There should be 8 RolePropertyDeltas" (length contextDeltas == 7)
      --
      -- x5 <- pure $ find
      --   (\(RoleBindingDelta{id, sequenceNumber}) -> (id == (RoleInstance "model:User$MyTestCase$Other_0001") && sequenceNumber == 119))
      --   roleDeltas
      -- case x5 of
      --   Nothing -> liftAff $ assert "There should be a RoleBindingDelta for model:User$MyTestCase$Other_0001 and sequenceNumber 119" false
      --   Just _ -> liftAff $ assert "There should be 3 RoleBindingDeltas" (length roleDeltas == 3)

      -- case head roleDeltas of
      --   Nothing -> liftAff $ assert "There should be one RoleBindingDelta" false
      --   Just (RoleBindingDelta{id, sequenceNumber}) -> liftAff $ assert "There should finally be a RoleBindingDelta on role model:User$MyTestCase$Other_0001 and its sequenceNumber should be 5" (id ==
      --   (RoleInstance "model:User$MyTestCase$Other_0001") && sequenceNumber == 5)
    userType <- roleType_ joop
    getChannel <- getDynamicPropertyGetter "model:System$PerspectivesSystem$User$Channel" (ST userType)
    channel <- joop ##>> getChannel
    deleteDatabase (unwrap channel)
    clearUserDatabase
    )
