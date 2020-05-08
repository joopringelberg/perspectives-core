module Test.Assignment.SerialiseAsJson
where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Free (Free)
import Data.Array (findIndex, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign (unsafeToForeign)
import Foreign.Class (decode, encode)
import Global.Unsafe (unsafeStringify)
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization, RolSerialization)
import Perspectives.CoreTypes ((##=))
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.Instances.SerialiseAsJson (serialiseAsJsonFor)
import Perspectives.LoadCRL (loadAndCacheCrlFile)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runSterileTransaction)
import Perspectives.Sync.Channel (addPartnerToChannel, createChannelContext)
import Simple.JSON (readImpl)
import Test.Perspectives.Utils (clearUserDatabase, runP, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Instances.SerialiseAsJson" do

  test "serialiseAsJsonFor" (runP $ withSystem $ do
    achannel <- runSterileTransaction $ createChannelContext "MyTestChannel"
    case head achannel of
      Nothing -> liftAff $ assert "Failed to create a channel" false
      Just channel -> do
        -- load a second user
        void $ loadAndCacheCrlFile "userJoop.crl" testDirectory
        void $ runSterileTransaction $ addPartnerToChannel (RoleInstance "model:User$joop$User") channel
        -- Serialise as JSON
        -- Get one of the roles
        partners <- channel ##= getEnumeratedRoleInstances (EnumeratedRoleType "model:System$Channel$ConnectedPartner")
        case head partners of
          Nothing -> liftAff $ assert "There should be two ConnectedPartners in the Channel instance" false
          Just p -> do
            (channelSerialiation :: Array ContextSerialization) <- serialiseAsJsonFor p channel
            log $ unsafeStringify channelSerialiation
            -- log $ "\n" <> (prettyPrint channelSerialiation)
            liftAff $ assert "The context 'model:User$joop' should have been serialised" (isJust $ findIndex (\(ContextSerialization{id}) -> id == "model:User$joop") channelSerialiation)
            liftAff $ assert "The context 'model:User$MyTestChannel' should have been serialised" (isJust $ findIndex (\(ContextSerialization{id}) -> id == "model:User$MyTestChannel") channelSerialiation)
            encodedChannelSerialization <- pure $ encode channelSerialiation
            case unwrap $ runExceptT (decode encodedChannelSerialization) of
              Left e -> liftAff $ assert "Encoded Array ContextSerialization should be decodable" false
              Right (channelSerialiation' :: Array ContextSerialization) -> liftAff $ assert "encode-decode should roundtrip" (channelSerialiation == channelSerialiation')
        clearUserDatabase
        )

  test "decodeProperties '{}'" (do
    s <- pure $ unsafeToForeign {}
    p <- pure $ unwrap $ runExceptT $ readImpl s
    case p of
      Left e -> assert ("Error decoding '{}'" <> show e) false
      Right (ps :: PropertySerialization) -> pure unit
    )

  test "decodeProperties '{prop1: [\"1\"]}'" (do
    s <- pure $ unsafeToForeign {prop1: ["1"]}
    p <- pure $ unwrap $ runExceptT $ readImpl s
    case p of
      Left e -> assert ("Error decoding '{prop1: [\"1\"]}'" <> show e) false
      Right (ps :: PropertySerialization) -> pure unit
    )

  test "decodeRol: '{properties: {}, binding: \"model:User$bla\"}'" (do
    s <- pure $ unsafeToForeign {properties: {}, binding: "model:User$bla"}
    p <- pure $ unwrap $ runExceptT $ readImpl s
    case p of
      Left e -> assert ("Error decoding '{properties: {}, binding: \"model:User$bla\"}'" <> show e)false
      Right (ps :: RolSerialization) -> pure unit
    )
