module Test.SerialisedAsDeltas where

import Prelude

import Control.Monad.AvarMonadAsk (get)
import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, head, length, null)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.Assignment.Update (handleNewPeer, setBinding)
import Perspectives.CoreTypes ((##>>))
import Perspectives.Couchdb.Databases (deleteDatabase)
import Perspectives.Instances.GetPropertyOnRoleGraph (getPropertyGetter)
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Query.UnsafeCompiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.Sync.Channel (addPartnerToChannel, createChannel)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile)
import Perspectives.TypesForDeltas (ContextDelta(..), RoleBindingDelta(..), RolePropertyDelta(..), UniverseContextDelta(..), UniverseRoleDelta(..))
import Perspectives.User (getHost, getPort)
import Perspectives.Utilities (prettyPrint)
import Test.Perspectives.Utils (clearUserDatabase, runP, withModel', withSystem)
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
    achannel <- runMonadPerspectivesTransaction createChannel
    case head achannel of
      Nothing -> liftAff $ assert "Failed to create a channel" false
      Just channel -> do
        -- load a second user
        void $ loadAndSaveCrlFile "userJoop.crl" testDirectory
        host <- getHost
        port <- getPort
        void $ runMonadPerspectivesTransaction $ addPartnerToChannel (RoleInstance "model:User$joop$User") channel host port

    getter <- getRoleFunction "model:Test$TestCase$Other"
    unboundOtherRole <- (ContextInstance "model:User$MyTestCase") ##>> getter
    userGetter <- getRoleFunction "model:System$PerspectivesSystem$User"
    joop <- (ContextInstance "model:User$joop") ##>> userGetter
    void $ runMonadPerspectivesTransaction do
      handleNewPeer unboundOtherRole
      void $ setBinding unboundOtherRole joop
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
    getChannel <- getPropertyGetter "model:System$PerspectivesSystem$User$Channel" userType
    channel <- joop ##>> getChannel
    deleteDatabase (unwrap channel)
    clearUserDatabase
    )
