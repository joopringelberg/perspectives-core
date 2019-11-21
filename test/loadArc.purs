module Test.LoadArc where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console (logShow)
import Foreign.Object (lookup)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc (loadArcFile)
import Simple.JSON (writeJSON)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.loadArc" do
  test "Load a model file and store it in Couchdb" do
    r <- runP $ loadArcFile "test1.arc" testDirectory
    case r of
      Left messages -> do
        logShow messages
        assert "Could not parse and process model" false
      Right df@(DomeinFile{contexts, enumeratedRoles}) -> do
        -- logShow df
        case lookup "model:Test$External" enumeratedRoles of
          Nothing -> pure unit
          Just e -> do
            serialised <- pure $ writeJSON (EMPTY :: ADT EnumeratedRoleType)
            pure unit
