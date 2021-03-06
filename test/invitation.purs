module Test.Invitation where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Free (Free)
import Data.Array (find, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import Foreign.Class (decode)
import Perspectives.ApiTypes (ContextSerialization(..))
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes ((##=), (##>))
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (buitenRol)
import Perspectives.LoadCRL.FS (loadAndSaveCrlFile)
import Perspectives.Persistence.API (deleteDatabase)
import Perspectives.Query.UnsafeCompiler (getPropertyFunction)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Simple.JSON (parseJSON)
import Test.Perspectives.Utils (runP, withSimpleChat, withSystem, runMonadPerspectivesTransaction)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Invitation" do

  -- testOnly "Bot serialises invitation" $ runP $ withModel_ (DomeinFileId "model:System") false do
  test "Bot serialises invitation" $ runP $ withSystem do
    addAllExternalFunctions
    -- Create an Invitation instance with an Inviter
    errs <- loadAndSaveCrlFile "invitation.crl" testDirectory
    let inviteProp = "model:System$Invitation$External$IWantToInviteAnUnconnectedUser"
    let serialisedProp = "model:System$Invitation$External$SerialisedInvitation"
    case head errs of
      Just e -> liftAff $ assert (show errs) false
      Nothing -> do
        -- Set its IWantToInviteAnUnconnectedUser to true
        void $ runMonadPerspectivesTransaction $ setProperty [RoleInstance $ buitenRol "model:User$MyInvitation"] (EnumeratedPropertyType inviteProp) [Value "true"]
        getter <- getPropertyFunction serialisedProp
        -- Get its SerialisedInvitation
        serialised <- (RoleInstance $ buitenRol "model:User$MyInvitation") ##= getter
        case head serialised of
          Nothing -> liftAff $ assert "There should have been a serialization string" false
          Just (Value s) -> do
            log s
            case unwrap $ runExceptT (parseJSON s >>= decode)  of
              Left (e :: MultipleErrors) -> liftAff $ assert ("Cannot decode serialization result: " <> show e) false
              Right (deserialised :: Array ContextSerialization) ->
                case find (\(ContextSerialization{id}) -> id == "model:User$MyInvitation") deserialised of
                  Nothing -> liftAff $ assert "There should have been an instance named model:User$MyInvitation" false
                  otherwise -> pure unit
        getChannelId <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
        mchannel <- (RoleInstance "model:User$test$User") ##> getChannelId
        case mchannel of
          Nothing -> pure unit
          Just (Value channelId) -> deleteDatabase channelId


  test "Bot serialises Chat" $ runP $ withSimpleChat do
  -- testOnly "Bot serialises invitation" $ runP $ withModel_ (DomeinFileId "model:SimpleChat") false do
    addAllExternalFunctions
    -- Create a Chat instance with an Initiator
    errs <- loadAndSaveCrlFile "chatInvitation.crl" testDirectory
    let inviteProp = "model:System$Invitation$External$IWantToInviteAnUnconnectedUser"
    let serialisedProp = "model:System$Invitation$External$SerialisedInvitation"
    case head errs of
      Just e -> liftAff $ assert (show errs) false
      Nothing -> do
        -- Set its IWantToInviteAnUnconnectedUser to true
        void $ runMonadPerspectivesTransaction $ setProperty [RoleInstance $ buitenRol "model:User$MyChatInvitation"] (EnumeratedPropertyType inviteProp) [Value "true"]
        getter <- getPropertyFunction serialisedProp
        -- Get its SerialisedInvitation
        serialised <- (RoleInstance $ buitenRol "model:User$MyChatInvitation") ##= getter
        case head serialised of
          Nothing -> liftAff $ assert "There should have been a serialization string" false
          Just (Value s) -> do
            log s
            case unwrap $ runExceptT (parseJSON s >>= decode)  of
              Left (e :: MultipleErrors) -> liftAff $ assert ("Cannot decode serialization result: " <> show e) false
              Right (deserialised :: Array ContextSerialization) ->
                case find (\(ContextSerialization{id}) -> id == "model:User$MyChatInvitation") deserialised of
                  Nothing -> liftAff $ assert "There should have been an instance named model:User$MyChatInvitation" false
                  otherwise -> pure unit
        getChannelId <- getPropertyFunction "model:System$PerspectivesSystem$User$Channel"
        mchannel <- (RoleInstance "model:User$test$User") ##> getChannelId
        case mchannel of
          Nothing -> pure unit
          Just (Value channelId) -> deleteDatabase channelId
