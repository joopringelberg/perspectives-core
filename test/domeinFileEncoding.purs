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
import Foreign.Class (decode, encode)
import Foreign.Object (lookup)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Query.QueryTypes (Calculation)
import Perspectives.Representation.State (State(..))
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.Extern.Couchdb" do
  testOnly "En-decode a DomeinFile" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    errs <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    if null errs
      then do
        target <- retrieveDomeinFile "model:System"
        encodedTarget <- pure $ encode target
        decodedTarget <- pure $ unwrap $ runExceptT $ decode encodedTarget
        case decodedTarget of
          Left e -> liftAff $ assert ("It should be possible to decode an encoded DomeinFile: " <> show e) false
          Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a DomeinFile instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model:System: " <> show errs) false

  test "En-decode a State" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    errs <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    if null errs
      then do
        (DomeinFile{states}) <- retrieveDomeinFile "model:System"
        case lookup "model:System$PerspectivesSystem$IndexedContexts$Dangles" states of
          Nothing -> liftAff $ assert ("There should have been an entry in states for " <> "model:System$PerspectivesSystem$IndexedContexts$Dangles") false
          Just target -> do
            encodedTarget <- pure $ encode target
            decodedTarget <- pure $ unwrap $ runExceptT $ decode encodedTarget
            case decodedTarget of
              Left e -> liftAff $ assert ("It should be possible to decode an encoded state: " <> show e) false
              Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a State instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model:System: " <> show errs) false

  test "En-decode the query of a State" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    errs <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    if null errs
      then do
        (DomeinFile{states}) <- retrieveDomeinFile "model:System"
        case lookup "model:System$PerspectivesSystem$IndexedContexts$Dangles" states of
          Nothing -> liftAff $ assert ("There should have been an entry in states for " <> "model:System$PerspectivesSystem$IndexedContexts$Dangles") false
          Just (State{query:target}) -> do
            encodedTarget <- pure $ encode (target :: Calculation)
            -- logShow $ unsafeShow encodedTarget
            decodedTarget <- pure $ unwrap $ runExceptT $ decode encodedTarget
            case decodedTarget of
              Left e -> liftAff $ assert ("It should be possible to decode an encoded query: " <> show e) false
              Right decodedTarget' -> liftAff $ assert ("It should be possible to roundtrip encoding-decoding a Query instance") (target == decodedTarget')
      else liftAff $ assert ("There are instance- or model errors for model:System: " <> show errs) false

foreign import unsafeShow :: Foreign -> String
