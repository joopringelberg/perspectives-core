module Test.Instances.ObjectGetters
where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Free (Free)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Foreign.Object (empty)
import Perspectives.ApiTypes (PropertySerialization(..))
import Perspectives.CoreTypes ((##>))
import Perspectives.Instances.Builders (constructEmptyContext)
import Perspectives.Instances.ObjectGetters (getMyType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
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
    c <- runSterileTransaction $ runExceptT $ constructEmptyContext (ContextInstance "model:User$c1") "sys:Invitation" "mycontext" (PropertySerialization empty)
    case head c of
      Nothing -> liftAff $ assert "Expected a contextinstance" false
      Just (Left e) -> liftAff $ assert ("Error on constructing a context: " <> show e) false
      Just (Right ctxt) -> do
        mytype <- ctxt ##> getMyType
        liftAff $ assert "my type should be sys:Invitation$Guest" (case mytype of
          Nothing -> false
          Just t -> t == (CR $ CalculatedRoleType "model:System$Invitation$Guest"))
      )

  test "getMyType" (runP $ withSimpleChat $ do
    c <- runSterileTransaction $ runExceptT $ constructEmptyContext (ContextInstance "model:User$ch1") "model:SimpleChat$Chat" "mychat" (PropertySerialization empty)
    case head c of
      Nothing -> liftAff $ assert "Expected a contextinstance" false
      Just (Left e) -> liftAff $ assert ("Error on constructing a context: " <> show e) false
      Just (Right ctxt) -> do
        mytype <- ctxt ##> getMyType
        liftAff $ assert "my type should be sys:Invitation$Guest" (case mytype of
          Nothing -> false
          Just t -> t == (CR $ CalculatedRoleType "model:System$Invitation$Guest"))
      )
