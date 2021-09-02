module Test.Extern.Couchdb where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Free (Free)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Data.Array (elemIndex, length, null)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Effect.Exception (error)
import Foreign.Object (lookup)
import Perspectives.CoreTypes (evalMonadPerspectivesQuery, (##=))
import Perspectives.Couchdb (designDocumentViews)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinCache (cascadeDeleteDomeinFile)
import Perspectives.DomeinFile (DomeinFileId(..))
import Perspectives.Extern.Couchdb (addModelToLocalStore, createDatabase, createUser, models, uploadToRepository)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Persistence.API (tryGetDocument)
import Perspectives.Persistent (entitiesDatabaseName, tryGetPerspectEntiteit)
import Perspectives.PerspectivesState (developmentRepository)
import Perspectives.Query.UnsafeCompiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.RunMonadPerspectivesTransaction (runSterileTransaction)
import Perspectives.SetupCouchdb (setModelDescriptionsView, setRoleView)
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile)
import Test.Perspectives.Utils (assertEqual, clearUserDatabase, runP, withSystem, runMonadPerspectivesTransaction)
import Test.Unit (TestF, suite, suiteOnly, suiteSkip, test, testSkip, testOnly)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.Extern.Couchdb" do

  test "models" $ runP $ withSystem do

    getModels <- getRoleFunction "model:System$PerspectivesSystem$Modellen"
    models <- ((ContextInstance "model:User$test") ##= getModels)
    -- logShow models
    liftAff $ assert "There should be some models" (length models > 0)

  test "upload model to repository and to perspect_models from files" $ runP $ withSystem do

    cdburl <- developmentRepository
    void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:System") cdburl)
    -- now run the query that retrieves the modelDescription field of all models in repository.
    -- The result must include "model:System$Model$External"
    (descriptions :: Array RoleInstance) <- evalMonadPerspectivesQuery "" \_ -> models (ContextInstance "")
    logShow descriptions
    liftAff $ assert "There must be the model:System description" (isJust $ elemIndex (RoleInstance "model:User$PerspectivesSystemModel_External") descriptions)

  test "addModelToLocalStore" do
    runP $ withSystem do
      cdburl <- developmentRepository
      void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:System") cdburl)
    runP do
      void $ runSterileTransaction $ addModelToLocalStore ["http://127.0.0.1:5984/repository/model:System"] (RoleInstance "")
      r <- tryGetPerspectEntiteit (ContextInstance "model:User$test")
      liftAff $ assert "There should be an instance of model:User$test" (isJust r)
      clearUserDatabase
      void $ cascadeDeleteDomeinFile (DomeinFileId "model:System")

  test "upload model:Serialise to repository from files" $ runP do
    errs <- loadCompileAndCacheArcFile "serialise" modelDirectory
    if null errs
      then do
        pure unit
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:Serialise") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:Serialise: " <> show errs) false

  test "upload model:Couchdb to repository from files" $ runP do
    errs <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    if null errs
      then do
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:Couchdb") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:Couchdb: " <> show errs) false

  test "upload model:System to repository from files" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    errs <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    if null errs
      then do
        pure unit
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:System") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:System: " <> show errs) false

  test "upload model:Parsing to repository from files" $ runP do
    errs <- loadCompileAndCacheArcFile "parsing" modelDirectory
    if null errs
      then do
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:Parsing") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:Parsing: " <> show errs) false

  test "upload model:BodiesWithAccounts to repository from files (without testuser)" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "bodiesWithAccounts" modelDirectory
    if null errs
      then do
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:BodiesWithAccounts") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:BodiesWithAccounts: " <> show errs) false

  testOnly "upload model:Models to repository from files (without testuser)" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "models" modelDirectory
    if null errs
      then do
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:Models") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:Models: " <> show errs) false

  test "upload model:ModelManagement to repository from files (without testuser)" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    _ <- loadCompileAndCacheArcFile "parsing" modelDirectory
    errs <- loadCompileAndCacheArcFile "modelManagement" modelDirectory
    if null errs
      then do
        pure unit
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:ModelManagement") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:ModelManagement: " <> show errs) false

  test "upload model:BrokerServices to repository from files (without testuser)" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "BrokerServices" modelDirectory
    if null errs
      then do
        -- pure unit
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:BrokerServices") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:BrokerServices: " <> show errs) false

  test "upload model:SimpleChat to repository from files" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "simpleChat" modelDirectory
    if null errs
      then do
        cdburl <- developmentRepository
        -- log $ "Repository url = " <> cdburl
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:SimpleChat") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:SimpleChat: " <> show errs) false

  test "upload model:Competition to repository from files (without testuser)" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "competition" modelDirectory
    if null errs
      then do
        pure unit
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:Competition") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:Competition: " <> show errs) false

  test "upload model:CouchdbManagement to repository from files (without testuser)" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "couchdbManagement" modelDirectory
    if null errs
      then do
        pure unit
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:CouchdbManagement") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:CouchdbManagement: " <> show errs) false

  test "upload model:TestBotActie to repository from files" $ runP $ withSystem do
    errs <- loadCompileAndCacheArcFile "testBotActie" modelDirectory
    if null errs
      then do
        cdburl <- developmentRepository
        void $ runWriterT $ runArrayT (uploadToRepository (DomeinFileId "model:TestBotActie") cdburl)
      else liftAff $ assert ("There are instance- or model errors for model:TestBotActie: " <> show errs) false

  test "setModelDescriptionsView" do
    assertEqual "The retrieved document should equal the sent document"
      (do
        setModelDescriptionsView
        mddoc <- tryGetDocument "repository" "_design/defaultViews"
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
        mddoc <- entitiesDatabaseName >>= \db -> tryGetDocument db "defaultViews"
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

  -- Run account "test" with password "geheim"
  -- PROBLEMEN: de test slaagt, maar een database wordt niet gemaakt.
  -- log statement in createDatabaseImpl vuurt.
  test "createDatabase" $ runP do
    void $ runMonadPerspectivesTransaction $ catchError
      (createDatabase ["https://localhost:6984/"] ["testdb"] (RoleInstance "ignored"))
      (\e -> logShow e)
    liftAff $ assert "Just testing" true

  -- Run account "test" with password "geheim"
  test "createUser" $ runP do
    void $ runMonadPerspectivesTransaction $ catchError
      (createUser ["https://localhost:6984/"] ["pipo1"] ["geheim"] (RoleInstance "ignored"))
      (\e -> logShow e)
    liftAff $ assert "Just testing" true
