module Test.Extern.Couchdb where

import Prelude

import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Free (Free)
import Data.Array (length, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Effect.Exception (error)
import Foreign.Object (insert, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (##=))
import Perspectives.Couchdb (designDocumentViews)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (createUser)
import Perspectives.Extern.Parsing (uploadToRepository_)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (isModelUri, modelUri2ModelUrl)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (tryGetDocument_)
import Perspectives.Persistence.Types (Credential(..))
import Perspectives.Persistent (entitiesDatabaseName, getDomeinFile)
import Perspectives.PerspectivesState (domeinCache)
import Perspectives.Query.UnsafeCompiler (getRoleFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.SetupCouchdb (setRoleView)
import Perspectives.TypePersistence.LoadArc.FS (loadCompileAndCacheArcFile, loadCompileAndCacheArcFile')
import Test.Perspectives.Utils (assertEqual, developmentRepository, runMonadPerspectivesTransaction, runP, runTestadmin, withSystem)
import Test.Unit (TestF, suiteOnly, test, testOnly)
import Test.Unit.Assert (assert)
 
-- developmentRepository :: MonadPerspectives String
-- developmentRepository = pure "http://localhost:5984/joopdev_models"

-- In order to use the self-signed certificate on perspectives.domains, run this in the terminal:
-- export NODE_EXTRA_CA_CERTS="$(mkcert -CAROOT)/rootCA.pem"
-- It exports the root certificate. Without that, the following error will be thrown:
-- "request to https://perspectives.domains/models_perspectives_domains/ failed, reason: unable to verify the first certificate","type":"system","errno":"UNABLE_TO_VERIFY_LEAF_SIGNATURE","code":"UNABLE_TO_VERIFY_LEAF_SIGNATURE"
-- Later, I experienced a  {"name":"FetchError","message":"request to https://perspectives.domains/models_perspectives_domains/ failed, reason: self signed certificate"} problem. Evaded it with:
-- export NODE_TLS_REJECT_UNAUTHORIZED="0"
-- NOTE that this script will only work properly if the /etc/hosts file contains an entry for perspectives.domains:
-- 127.0.0.1 perspectives.domains
-- 127.0.0.1 www.perspectives.domains

models_perspectives_domains :: String
models_perspectives_domains = "https://localhost:6984/models_perspectives_domains"

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

-- | Take a DomeinFile from the local perspect_models database and upload it to the repository database.
-- | Notice that repositoryUrl is derived from the DomeinFileId.
-- | Attachments are preserved: if they were in the repository before uploading,
-- | they will be in the repository after uploading.
uploadToRepository :: DomeinFileId -> MonadPerspectives Unit
uploadToRepository dfId@(DomeinFileId domeinFileName) = do
  if isModelUri domeinFileName
    then do
      mdf <- try $ getDomeinFile dfId
      case mdf of
        Left err -> logPerspectivesError $ DomeinFileErrorBoundary "uploadToRepository" (show err) 
        -- Notice that we  supply an empty array instead of the actually compiled InvertedQueries!
        Right df -> uploadToRepository_ (unsafePartial modelUri2ModelUrl domeinFileName) df []
    else logPerspectivesError $ DomeinFileErrorBoundary "uploadToRepository" ("This modelURI is not well-formed: " <> domeinFileName)


theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.Extern.Couchdb" do 

  test "models" $ runP $ withSystem do

    getModels <- getRoleFunction "model:System$PerspectivesSystem$Modellen"
    models <- ((ContextInstance "model:User$test") ##= getModels)
    -- logShow models
    liftAff $ assert "There should be some models" (length models > 0)

  test "upload model://perspectives.domains#Serialise to repository from files" $ runP do
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    errs <- loadCompileAndCacheArcFile' "serialise" modelDirectory
    if null errs
      then do
        pure unit
        cdburl <- developmentRepository
        uploadToRepository (DomeinFileId "model://perspectives.domains#Serialise")
      else liftAff $ assert ("There are instance- or model errors for model:Serialise: " <> show errs) false

  test "upload model://perspectives.domains#Sensor to repository from files" $ runP do
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    errs <- loadCompileAndCacheArcFile' "sensor" modelDirectory
    if null errs
      then do
        pure unit
        cdburl <- developmentRepository
        uploadToRepository (DomeinFileId "model://perspectives.domains#Sensor")
      else liftAff $ assert ("There are instance- or model errors for perspectives.domains#Sensor: " <> show errs) false

  test "upload model://perspectives.domains#Couchdb to repository from files" $ runP do
    -- Add a password for "https://localhost:6984/"
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    errs <- loadCompileAndCacheArcFile' "couchdb" modelDirectory
    if null errs
      then do
        -- cdburl <- developmentRepository
        uploadToRepository (DomeinFileId "model://perspectives.domains#Couchdb")
      else liftAff $ assert ("There are instance- or model errors for perspectives.domains#Couchdb: " <> show errs) false

  test "upload model://perspectives.domains#Utilities to repository from files" $ runP do
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    errs <- loadCompileAndCacheArcFile' "utilities" modelDirectory
    if null errs
      then do
        cdburl <- developmentRepository
        uploadToRepository (DomeinFileId "model://perspectives.domains#Utilities")
      else liftAff $ assert ("There are instance- or model errors for perspectives.domains#Utilities: " <> show errs) false

  test "upload model://perspectives.domains#System to repository from files" $ runP do
    -- Add a password for "https://localhost:6984/"
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile' "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile' "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile' "sensor" modelDirectory
    _ <- loadCompileAndCacheArcFile' "utilities" modelDirectory
    errs <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    dCache <- domeinCache
    if null errs
      then uploadToRepository (DomeinFileId "model://perspectives.domains#System")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#System: " <> show errs) false

  test "upload model://perspectives.domains#Parsing to repository from files" $ runP do
    errs <- loadCompileAndCacheArcFile' "parsing" modelDirectory
    if null errs
      then uploadToRepository (DomeinFileId "model://perspectives.domains#Parsing")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#Parsing: " <> show errs) false

  test "upload model://perspectives.domains#TestPublicRole to repository from files" $ runP do
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile' "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile' "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile' "sensor" modelDirectory
    _ <- loadCompileAndCacheArcFile' "utilities" modelDirectory
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    dCache <- domeinCache
    errs <- loadCompileAndCacheArcFile' "testPublicRole" modelDirectory
    if null errs
      then uploadToRepository (DomeinFileId "model://perspectives.domains#TestPublicRole")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#TestPublicRole: " <> show errs) false

  test "upload model://perspectives.domains#TestFields to repository from files" $ runP do
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile' "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile' "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile' "sensor" modelDirectory
    _ <- loadCompileAndCacheArcFile' "utilities" modelDirectory
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile' "testFields" modelDirectory
    if null errs
      then uploadToRepository (DomeinFileId "model://perspectives.domains#TestFields")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#TestFields: " <> show errs) false

  test "upload model:perspectives.domains#BodiesWithAccounts to repository from files (without testuser)" $ runP do
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile' "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile' "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile' "sensor" modelDirectory
    _ <- loadCompileAndCacheArcFile' "utilities" modelDirectory
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile' "bodiesWithAccounts" modelDirectory
    if null errs
      then uploadToRepository (DomeinFileId "model://perspectives.domains#BodiesWithAccounts")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#BodiesWithAccounts: " <> show errs) false

  testOnly "upload model://perspectives.domains#CouchdbManagement_new to repository from files (without testuser)" $ runP do
    modify \s@({couchdbCredentials}) -> s {couchdbCredentials = insert "https://localhost:6984/" (Credential "joopdev" "geheim") couchdbCredentials}
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile' "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile' "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile' "sensor" modelDirectory
    _ <- loadCompileAndCacheArcFile' "utilities" modelDirectory
    _ <- loadCompileAndCacheArcFile' "perspectivesSysteem" modelDirectory
    _ <- loadCompileAndCacheArcFile' "bodiesWithAccounts" modelDirectory
    _ <- loadCompileAndCacheArcFile' "parsing" modelDirectory
    errs <- loadCompileAndCacheArcFile' "couchdbManagement_new" modelDirectory
    if null errs
      then uploadToRepository (DomeinFileId "model://perspectives.domains#CouchdbManagement")
      else liftAff $ assert ("There are instance- or model errors for model//perspectives.domains#CouchdbManagement: " <> show errs) false

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
        uploadToRepository (DomeinFileId "model://perspectives.domains#ModelManagement")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#ModelManagement: " <> show errs) false

  test "upload model://perspectives.domains#BrokerServices to repository from files (without testuser)" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "serialise" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "BrokerServices" modelDirectory
    if null errs
      then do
        -- pure unit
        cdburl <- developmentRepository
        uploadToRepository (DomeinFileId "model://perspectives.domains#BrokerServices")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#BrokerServices: " <> show errs) false

  test "upload model://perspectives.domains#SimpleChat to repository from files" $ runP do
    addAllExternalFunctions
    _ <- loadCompileAndCacheArcFile "couchdb" modelDirectory
    _ <- loadCompileAndCacheArcFile "perspectivesSysteem" modelDirectory
    errs <- loadCompileAndCacheArcFile "simpleChat" modelDirectory
    if null errs
      then do
        cdburl <- developmentRepository
        -- log $ "Repository url = " <> cdburl
        uploadToRepository (DomeinFileId "model://perspectives.domains#SimpleChat")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#SimpleChat: " <> show errs) false

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
        uploadToRepository (DomeinFileId "model://perspectives.domains#Competition")
      else liftAff $ assert ("There are instance- or model errors for model://perspectives.domains#Competition: " <> show errs) false

  test "setRoleView" do
    assertEqual "The retrieved document should equal the sent document"
      (do
        entitiesDatabaseName >>= setRoleView
        mddoc <- entitiesDatabaseName >>= \db -> tryGetDocument_ db "defaultViews"
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

  -- Run account "test" with password (Credential "joopdev" "geheim")
  -- PROBLEMEN: de test slaagt, maar een database wordt niet gemaakt.
  -- log statement in createDatabaseImpl vuurt.
  -- test "createDatabase" $ runP do
  --   void $ runMonadPerspectivesTransaction $ catchError
  --     (createDatabase ["https://localhost:6984/"] ["testdb"] (RoleInstance "ignored"))
  --     (\e -> logShow e)
  --   liftAff $ assert "Just testing" true

  -- Run account "test" with password (Credential "joopdev" "geheim")
  test "createUser" $ runP do
    void $ runMonadPerspectivesTransaction $ catchError
      (createUser ["https://localhost:6984/"] ["pipo1"] ["geheim"] (RoleInstance "ignored"))
      (\e -> logShow e)
    liftAff $ assert "Just testing" true
