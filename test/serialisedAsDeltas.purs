module Test.SerialisedAsDeltas where

import Prelude

import Control.Monad.AvarMonadAsk (get)
import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, length, null)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Perspectives.Assignment.Update (handleNewPeer, setBinding)
import Perspectives.CoreTypes ((##>>))
import Perspectives.Couchdb.Databases (deleteDatabase)
import Perspectives.DomeinFile (DomeinFileId(..))
import Perspectives.Instances.GetPropertyOnRoleGraph (getPropertyGetter)
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Query.Compiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.Sync.Channel (addUserToChannel, createChannel)
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile)
import Perspectives.TypesForDeltas (ContextDelta(..), RoleBindingDelta(..), RolePropertyDelta(..), UniverseContextDelta(..), UniverseRoleDelta(..))
import Test.Perspectives.Utils (clearUserDatabase, runP, withModel')
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteOnly "SerialisedAsDeltas" do

  test "Bind a user to a role in a context" (runP $ withModel' (DomeinFileId "model:System") $ do
    -- load userdata.
    modelErrors <- loadCompileAndCacheArcFile "serialisedAsDeltas" testDirectory
    if not $ null modelErrors
      then do
        logShow modelErrors
        liftAff $ assert "There are modelErrors" false
      else pure unit
    achannel <- runMonadPerspectivesTransaction createChannel
    -- Hierboven gaat het fout
    case head achannel of
      Nothing -> liftAff $ assert "Failed to create a channel" false
      Just channel -> do
        -- load a second user
        void $ loadAndSaveCrlFile "userJoop.crl" testDirectory
        void $ runMonadPerspectivesTransaction $ addUserToChannel (RoleInstance "model:User$joop$User") channel

    getter <- getRoleFunction "model:Test$TestCase$Other"
    unboundOtherRole <- (ContextInstance "model:User$MyTestCase") ##>> getter
    userGetter <- getRoleFunction "model:System$PerspectivesSystem$User"
    joop <- (ContextInstance "model:User$joop") ##>> userGetter
    void $ runMonadPerspectivesTransaction do
      handleNewPeer unboundOtherRole
      void $ setBinding unboundOtherRole joop
      t@(Transaction{universeContextDeltas, universeRoleDeltas, contextDeltas, roleDeltas, propertyDeltas}) <- lift get
      -- logShow t
      -- There should be five deltas.
      case head universeContextDeltas of
        Nothing -> liftAff $ assert "There should be one UniverseContextDelta" false
        Just (UniverseContextDelta{contextType, sequenceNumber}) -> liftAff $ assert "The contextType should be model:Test$TestCase and the sequenceNumber should be 0" (contextType == (ContextType "model:Test$TestCase") && sequenceNumber == 0 && length universeContextDeltas == 1)
      case head universeRoleDeltas of
        Nothing -> liftAff $ assert "There should be two UniverseRoleDeltas" false
        Just x@(UniverseRoleDelta{roleType, sequenceNumber}) -> do
          -- logShow x
          liftAff $ assert "The roleType should be model:Test$TestCase$ARole and the sequenceNumber should be 2" (roleType == (EnumeratedRoleType "model:Test$TestCase$ARole") && sequenceNumber == 2 && length universeRoleDeltas == 2)
      case head propertyDeltas of
        Nothing -> liftAff $ assert "There should be one RolePropertyDelta" false
        Just (RolePropertyDelta{property, sequenceNumber}) -> liftAff $ assert "The propertyType should be model:Test$TestCase$ARole$Prop1 and the sequenceNumber should be 3" (property == (EnumeratedPropertyType "model:Test$TestCase$ARole$Prop1") && sequenceNumber == 3 && length propertyDeltas == 1)
      case head contextDeltas of
        Nothing -> liftAff $ assert "There should be one ContextDelta" false
        Just (ContextDelta{roleType, sequenceNumber}) -> liftAff $ assert "The roleType should be model:Test$TestCase$ARole and the sequenceNumber should be 4" (roleType == (EnumeratedRoleType "model:Test$TestCase$ARole") && sequenceNumber == 4 && length contextDeltas == 1)
      case head roleDeltas of
        Nothing -> liftAff $ assert "There should be one RoleBindingDelta" false
        Just (RoleBindingDelta{id, sequenceNumber}) -> liftAff $ assert "There should finally be a RoleBindingDelta on role model:User$MyTestCase$Other_0001 and its sequenceNumber should be 5" (id ==
        (RoleInstance "model:User$MyTestCase$Other_0001") && sequenceNumber == 5)
    userType <- roleType_ joop
    getChannel <- getPropertyGetter "model:System$PerspectivesSystem$User$Channel" userType
    channel <- joop ##>> getChannel
    deleteDatabase (unwrap channel)
    clearUserDatabase
    )
