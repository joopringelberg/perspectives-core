module Test.Types.ObjectGetters where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, elemIndex)
import Data.Either (Either(..))
import Data.Maybe (isJust)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CoreTypes ((###=), (###>>))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (roleADT)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile)
import Perspectives.Types.ObjectGetters (allRoleTypesInContext, propertiesOfRole, specialisesRoleType)
import Test.Perspectives.Utils (runP, withSimpleChat, withSystem)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite  "Perspectives.Types.ObjectGetters" do

  testSkip "propertiesOfRole 1" (runP do
    -- messages <- loadAndCompileArcFile "perspectivesSysteem" modelDirectory
    -- case messages of
    --   Left m -> do
    --     liftAff $ logShow messages
    --     liftAff $ assert "The file could not be parsed or compiled" false
    --   _ -> do
    do
        -- r <- getEnumeratedRole (EnumeratedRoleType "model:System$PerspectivesSystem$External")
        -- theType <- roleADT r
        -- liftAff $ logShow theType

        props <- "model:System$PerspectivesSystem$External" ###= propertiesOfRole
        -- props <- "model:System$TrustedCluster$ClusterGenoot" ###= propertiesOfRole
        logShow props
        -- liftAff $ assert "There should be two properties for model:System$PerspectivesSystem$External" (length props == 2)
        -- liftAff $ assert "The properties of 'odel:System$PerspectivesSystem$External' should include 'model:System$NamedContext$External$Name'" (isJust $ elemIndex (ENP (EnumeratedPropertyType "model:System$NamedContext$External$Name")) props)
        )
  testSkip "propertiesOfRole 2" (runP do
        props <- "model:TestBotActie$Tests$External" ###= propertiesOfRole
        logShow props
        )

  test "allRoleTypesInContext" $ runP $ withSystem do
    r <- ContextType "model:System$PerspectivesSystem" ###= allRoleTypesInContext
    -- logShow r
    liftAff $ assert "There should be 9 roletypes" (length r == 9)

  test "chat:Chat$Initiator `specialisesRoleType` sys:Invitation$Inviter" $ runP $ withSimpleChat do
    r <- (ENR $ EnumeratedRoleType "model:System$Invitation$Inviter") ###>> specialisesRoleType (ENR $ EnumeratedRoleType "model:SimpleChat$Chat$Initiator")
    -- logShow r
    liftAff $ assert "chat:Chat$Initiator `specialisesRoleType` sys:Invitation$Inviter should be true" (r == Value "true")
