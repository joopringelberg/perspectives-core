module Test.Types.ObjectGetters where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, elemIndex)
import Data.Either (Either(..))
import Data.Maybe (isJust)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CoreTypes ((###=))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (roleADT)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), PropertyType(..))
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile)
import Perspectives.Types.ObjectGetters (propertiesOfRole)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite  "Perspectives.Types.ObjectGetters" do

  test "propertiesOfRole" (runP do
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
  test "propertiesOfRole" (runP do
        props <- "model:TestBotActie$Tests$External" ###= propertiesOfRole
        logShow props
        )
