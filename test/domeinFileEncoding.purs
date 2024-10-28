module Test.DomeinFile.Encoding where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Object (lookup)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Query.QueryTypes (Calculation)
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile)
import Simple.JSON (read, readImpl, write, writeImpl)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Extern.Couchdb" do

  testSkip "En-decode a DomeinFile" $ runP do
    addAllExternalFunctions
    errs <- loadCompileAndCacheArcFile "utilities" modelDirectory
    if null errs
      then do
        target <- retrieveDomeinFile $ DomeinFileId "model://perspectives.domains#Utilities"
        encodedTarget <- pure $ writeImpl target
        decodedTarget <- pure $ unwrap $ runExceptT $ readImpl encodedTarget
        case decodedTarget of
          Left e -> liftAff $ assert ("It should be possible to decode an encoded DomeinFile: " <> show e) false
          Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a DomeinFile instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#Utilities: " <> show errs) false

  testSkip "En-decode the System model" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile "sensor" modelDirectory
    _ <- loadCompileAndCacheArcFile "utilities" modelDirectory
    errs <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    if null errs
      then do
        target <- retrieveDomeinFile $ DomeinFileId "model://perspectives.domains#System"
        encodedTarget <- pure $ writeImpl target
        decodedTarget <- pure $ unwrap $ runExceptT $ readImpl encodedTarget
        case decodedTarget of
          Left e -> liftAff $ assert ("It should be possible to decode an encoded DomeinFile: " <> show e) false
          Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a DomeinFile instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#System: " <> show errs) false

  testSkip "En-decode the Bodies With Accounts model" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile "sensor" modelDirectory
    _ <- loadCompileAndCacheArcFile "utilities" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "BodiesWithAccounts" modelDirectory
    if null errs
      then do
        target <- retrieveDomeinFile $ DomeinFileId "model://perspectives.domains#BodiesWithAccounts"
        encodedTarget <- pure $ writeImpl target
        decodedTarget <- pure $ unwrap $ runExceptT $ readImpl encodedTarget
        case decodedTarget of
          Left e -> liftAff $ assert ("It should be possible to decode an encoded DomeinFile: " <> show e) false
          Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a DomeinFile instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#BodiesWithAccounts: " <> show errs) false

  test "En-decode the CouchdbManagement model" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile "sensor" modelDirectory
    _ <- loadCompileAndCacheArcFile "utilities" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    _ <- loadCompileAndCacheArcFile "BodiesWithAccounts" modelDirectory
    _ <- loadCompileAndCacheArcFile "parsing" modelDirectory
    _ <- loadCompileAndCacheArcFile "files" modelDirectory
    errs <- loadCompileAndCacheArcFile "couchdbManagement_new" modelDirectory
    if null errs
      then do
        target <- retrieveDomeinFile $ DomeinFileId "model://perspectives.domains#CouchdbManagement"
        encodedTarget <- pure $ writeImpl target
        decodedTarget <- pure $ unwrap $ runExceptT $ readImpl encodedTarget
        case decodedTarget of
          Left e -> liftAff $ assert ("It should be possible to decode an encoded DomeinFile: " <> show e) false
          Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a DomeinFile instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#CouchdbManagement: " <> show errs) false

  testSkip "En-decode a State" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    errs <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    if null errs
      then do
        (DomeinFile{states}) <- retrieveDomeinFile $ DomeinFileId "model:System"
        case lookup "model:System$PerspectivesSystem$IndexedContexts$Dangles" states of
          Nothing -> liftAff $ assert ("There should have been an entry in states for " <> "model:System$PerspectivesSystem$IndexedContexts$Dangles") false
          Just target -> do
            encodedTarget <- pure $ write target
            decodedTarget <- pure $ read encodedTarget
            case decodedTarget of
              Left e -> liftAff $ assert ("It should be possible to decode an encoded state: " <> show e) false
              Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a State instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model:System: " <> show errs) false

  testSkip "En-decode the query of a State" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    errs <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    if null errs
      then do
        (DomeinFile{states}) <- retrieveDomeinFile $ DomeinFileId "model:System"
        case lookup "model:System$PerspectivesSystem$IndexedContexts$Dangles" states of
          Nothing -> liftAff $ assert ("There should have been an entry in states for " <> "model:System$PerspectivesSystem$IndexedContexts$Dangles") false
          Just (State{query:target}) -> do
            encodedTarget <- pure $ write (target :: Calculation)
            -- logShow $ unsafeShow encodedTarget
            decodedTarget <- pure $ read encodedTarget
            case decodedTarget of
              Left e -> liftAff $ assert ("It should be possible to decode an encoded query: " <> show e) false
              Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a Query instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model:System: " <> show errs) false

foreign import unsafeShow :: Foreign -> String
