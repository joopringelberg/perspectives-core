module Test.Extern.Couchdb where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (Free)
import Control.Monad.Writer (runWriterT)
import Data.Array (elemIndex, length, null)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Effect.Exception (error)
import Foreign.Object (lookup)
import Perspectives.CoreTypes (evalMonadPerspectivesQuery, (##=))
import Perspectives.Couchdb (designDocumentViews)
import Perspectives.Couchdb.Databases (getDesignDocument)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinFile (DomeinFileId(..))
import Perspectives.Extern.Couchdb (addExternalFunctions) as ExternalCouchdb
import Perspectives.Extern.Couchdb (addModelToLocalStore, models, uploadToRepository)
import Perspectives.Persistent (entitiesDatabaseName, removeEntiteit, tryGetPerspectEntiteit)
import Perspectives.Query.Compiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.RunMonadPerspectivesTransaction (runSterileTransaction)
import Perspectives.SetupCouchdb (setModelDescriptionsView, setRoleView)
import Perspectives.TypePersistence.LoadArc (loadCompileAndCacheArcFile, loadCompileAndCacheArcFile', loadCompileAndSaveArcFile)
import Perspectives.User (getCouchdbBaseURL)
import Test.Perspectives.Utils (assertEqual, clearUserDatabase, runP, setupUser)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testOnly, testSkip)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.Extern.Couchdb" do

  test "models" (runP do
    ExternalCouchdb.addExternalFunctions
    modelErrors <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    if null modelErrors
      then do
        getModels <- getRoleFunction "model:System$PerspectivesSystem$Modellen"
        models <- ((ContextInstance "model:User$test") ##= getModels)
        logShow models
        liftAff $ assert "There should be some models" (length models > 0)
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  test "upload model to repository and to perspect_models from files" (runP do
    ExternalCouchdb.addExternalFunctions
    -- setupUser
    modelErrors <- loadCompileAndSaveArcFile "perspectivesSysteem" modelDirectory
    if null modelErrors
      then do
        cdburl <- getCouchdbBaseURL
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:System") (cdburl <> "repository"))
        -- now run the query that retrieves the modelDescription field of all models in repository.
        -- The result must include "model:System$Model$External"
        (descriptions :: Array RoleInstance) <- evalMonadPerspectivesQuery "" \_ -> models
        logShow descriptions
        liftAff $ assert "There must be the model:System description" (isJust $ elemIndex (RoleInstance "model:User$PerspectivesSystemModel_External") descriptions)
      else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )

  testOnly "addModelToLocalStore" do
    (runP do
      -- model:Couchdb is a prerequisite.
      void $ loadCompileAndCacheArcFile' "couchdb" modelDirectory
      -- put model:System in cache.
      modelErrors <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
      if null modelErrors
        then do
          cdburl <- getCouchdbBaseURL
          void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:System") (cdburl <> "repository"))
        else liftAff $ assert ("There are model errors: " <> show modelErrors) false
      )
    runP do
      void $ runSterileTransaction $ addModelToLocalStore ["http://127.0.0.1:5984/repository/model%3ASystem"]
      r <- tryGetPerspectEntiteit (ContextInstance "model:User$test")
      liftAff $ assert "There should be an instance of model:User$test" (isJust r)
      clearUserDatabase
      void $ removeEntiteit (DomeinFileId "model:System")

  test "upload model to repository from files" (runP do
    -- setupUser
    cdburl <- getCouchdbBaseURL
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    -- errors <- loadCompileAndSaveArcFile "simpleChat" modelDirectory
    -- errors <- loadCompileAndSaveArcFile "perspectivesSysteem" modelDirectory
    -- liftAff $ assert ("There should be no errors" <> show errors) (null errors)
    void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:System") (cdburl <> "repository"))
    -- now run the query that retrieves the modelDescription field of all models in repository.
    -- The result must include "model:System$Model$External"
      )

  test "setModelDescriptionsView" do
    assertEqual "The retrieved document should equal the sent document"
      (do
        setModelDescriptionsView
        mddoc <- getDesignDocument "repository" "defaultViews"
        case mddoc of
          Nothing -> throwError (error "No design doc, impossible!")
          Just ddoc -> do
            views <- pure $ designDocumentViews ddoc
            pure $ isJust $ lookup "modeldescriptions" views)
      true

  test "setRoleView" do
    assertEqual "The retrieved document should equal the sent document"
      (do
        entitiesDatabaseName >>= setRoleView
        mddoc <- entitiesDatabaseName >>= \db -> getDesignDocument db "defaultViews"
        case mddoc of
          Nothing -> throwError (error "No design doc, impossible!")
          Just ddoc -> do
            views <- pure $ designDocumentViews ddoc
            pure $ isJust $ lookup "roleView" views)
      true

--   test "setRoleView" (runP do
--     users <- roleInstances ["model:System$PerspectivesSystem$User"]
--     logShow users
--     liftAff $ assert "There should be two users" (length users == 2)
-- )
