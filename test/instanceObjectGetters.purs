module Test.Instances.ObjectGetters
where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Free (Free)
import Data.Array (head, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Foreign.Object (empty)
import Perspectives.ApiTypes (PropertySerialization(..))
import Perspectives.CoreTypes ((##>), (##=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.InstanceRepresentation (PerspectContext(..))
import Perspectives.Instances.Builders (constructEmptyContext)
import Perspectives.Instances.ObjectGetters (binds, bottom)
import Perspectives.LoadCRL.FS (loadAndSaveCrlFile)
import Perspectives.Query.UnsafeCompiler (getMyType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runSterileTransaction)
import Test.Perspectives.Utils (runP, withSimpleChat, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Test.Instances.ObjectGetters" do

  test "getMyType" (runP $ withSystem $ do
    c <- runSterileTransaction $ runExceptT $ constructEmptyContext (ContextInstance "model:User$c1") "sys:Invitation" "mycontext" (PropertySerialization empty) Nothing
    case c of
      (Left e) -> liftAff $ assert ("Error on constructing a context: " <> show e) false
      (Right (PerspectContext{_id})) -> do
        mytype <- _id ##> getMyType
        liftAff $ assert "my type should be sys:Invitation$Guest" (case mytype of
          Nothing -> false
          Just t -> t == (CR $ CalculatedRoleType "model:System$Invitation$Guest"))
      )

  test "getMyType" (runP $ withSimpleChat $ do
    c <- runSterileTransaction $ runExceptT $ constructEmptyContext (ContextInstance "model:User$ch1") "model:SimpleChat$Chat" "mychat" (PropertySerialization empty) Nothing
    case c of
      (Left e) -> liftAff $ assert ("Error on constructing a context: " <> show e) false
      (Right (PerspectContext{_id})) -> do
        mytype <- _id ##> getMyType
        liftAff $ assert "my type should be sys:Invitation$Guest" (case mytype of
          Nothing -> false
          Just t -> t == (CR $ CalculatedRoleType "model:System$Invitation$Guest"))
      )

  test "binds" $ runP $ withSimpleChat do
    -- Construct an instance of a Chat with an Initiator.
    -- The Initiator is filled with the Chatter of ChatApp.
    -- Chatter is filled by User.
    -- Then check that Initiator `binds` User is true.
    errs <- loadAndSaveCrlFile "chatInitiator.crl" testDirectory
    if null errs
      then do
        r <- (RoleInstance "model:User$test$User") ##= binds \_ -> pure (RoleInstance "model:User$Initiator")
        -- logShow r
        liftAff $ assert "Initator should bind User" (r == [Value "true"])

        s <- (RoleInstance "model:User$test$TheTrustedCluster") ##= binds \_ -> pure (RoleInstance "model:User$Initiator")
        liftAff $ assert "Initiator should not bind TheTrustedCluster" (s == [Value "false"])

      else liftAff $ assert ("There are model errors" <> (show errs)) false

  test "bottom" $ runP $ withSimpleChat do
    -- Construct an instance of a Chat with an Initiator.
    -- The Initiator is filled with the Chatter of ChatApp.
    -- Chatter is filled by User.
    -- Then check that the bottom of Initiator is User.
    errs <- loadAndSaveCrlFile "chatInitiator.crl" testDirectory
    if null errs
      then do
        i1 <- (RoleInstance "model:User$Initiator") ##> bottom
        liftAff $ assert "The bottom of Initiator should be User" (i1 == (Just $ RoleInstance "model:User$test$User"))

        i2 <- (unit ##= (\_ -> ArrayT (pure [(RoleInstance "model:User$Initiator")])) >=> bottom)
        -- logShow i2
        liftAff $ assert "(\\_ -> ArrayT (pure [(RoleInstance \"model:User$Initiator\")])) is an ObjectGetter" (i2 == [RoleInstance "model:User$test$User"])

      else liftAff $ assert ("There are model errors" <> (show errs)) false
