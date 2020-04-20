module Test.Invitation where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Free (Free)
import Data.Array (find, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Foreign (MultipleErrors)
import Foreign.Class (decode)
import Perspectives.ApiTypes (ContextSerialization(..))
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes ((##=))
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (buitenRol)
import Perspectives.LoadCRL (loadAndSaveCrlFile)
import Perspectives.Query.Compiler (getPropertyFunction)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Simple.JSON (parseJSON)
import Test.Perspectives.Utils (runP, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteOnly "Invitation" do

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
