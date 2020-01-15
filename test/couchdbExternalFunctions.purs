module Test.Extern.Couchdb where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (length, null)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Perspectives.CoreTypes ((##=))
import Perspectives.Extern.Couchdb (addExternalFunctions) as ExternalCouchdb
import Perspectives.Query.Compiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (runP, setupUser)
import Test.Unit (TestF, suite, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Extern.Couchdb" do

  test "Modellen" (runP do
    ExternalCouchdb.addExternalFunctions
    modelErrors <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    if null modelErrors
      then do
        setupUser
        getModels <- getRoleFunction "model:System$PerspectivesSystem$Modellen"
        models <- ((ContextInstance "model:User$MijnSysteem") ##= getModels)
        logShow models
        liftAff $ assert "There should be some models" (length models > 0)
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
