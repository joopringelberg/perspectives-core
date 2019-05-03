module Perspectives.DomeinCache
-- ( retrieveDomeinResourceDefinition )

where

import Affjax (Request, request, put) as AX
import Affjax (ResponseFormatError, printResponseFormatError)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (class MonadError, runExcept, throwError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Effect.Aff (Aff, catchError)
import Effect.Aff.AVar (AVar, empty, put, read, take)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error, Error)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (DocReference(..), GetCouchdbAllDocs(..), PutCouchdbDocument, onAccepted, onCorrectCallAndResponse)
import Perspectives.Couchdb.Databases (defaultPerspectRequest, retrieveDocumentVersion)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.EntiteitAndRDFAliases (ContextID, RolID, ID)
import Perspectives.Identifiers (Namespace, escapeCouchdbDocumentName)
import Perspectives.PerspectivesState (domeinCacheInsert, domeinCacheLookup, domeinCacheRemove)
import Perspectives.Syntax (PerspectContext, PerspectRol, revision)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($), (*>), (<$>), (<>), (==), (>>=))

type URL = String

storeDomeinFileInCache :: Namespace -> DomeinFile -> MonadPerspectives (AVar DomeinFile)
storeDomeinFileInCache ns df= do
  ev <- (liftAff empty) >>= domeinCacheInsert ns
  liftAff $ put df ev
  pure ev

-- | Change the domeinfile in cache. NOTA BENE: does not store the modified file in Couchdb!
modifyDomeinFileInCache :: Namespace -> (DomeinFile -> DomeinFile) -> MonadPerspectives Unit
modifyDomeinFileInCache ns modifier = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> throwError $ error $ "modifyDomeinFileInCache cannot find domeinfile in cache: " <> ns
    (Just avar) -> do
      df <- liftAff $ take avar
      liftAff $ put (modifier df) avar

-- | Fetch a PerspectContext asynchronously from its Domein, loading the Domein file if necessary.
retrieveContextFromDomein ::
  ContextID
  -> Namespace
  -> (MonadPerspectives PerspectContext)
retrieveContextFromDomein id ns = do
  (DomeinFile {contexts}) <- retrieveDomeinFile ns
  case lookup id contexts of
    Nothing -> throwError $ error ("retrieveContextFromDomein: cannot find definition of " <> id <> " in retrieveContextFromDomein for " <> ns)
    (Just context) -> pure context

-- | Fetch a PerspectRol asynchronously from its Domein, loading the Domein file if necessary.
retrieveRolFromDomein ::
  RolID
  -> Namespace
  -> (MonadPerspectives PerspectRol)
retrieveRolFromDomein id ns = do
  (DomeinFile {roles}) <- retrieveDomeinFile ns
  case lookup id roles of
    Nothing -> throwError $ error ("retrieveRolFromDomein: cannot find definition of " <> id <> " in retrieveRolFromDomein for " <> ns)
    (Just rol) -> pure rol

retrieveDomeinFile :: Namespace -> MonadPerspectives DomeinFile
retrieveDomeinFile ns = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> do
      ev <- (liftAff empty) >>= domeinCacheInsert ns
      -- forkAff hinders catchError.
      -- _ <- forkAff do
      res <- catchError
        (liftAff $ AX.request $ domeinRequest {url = modelsURL <> escapeCouchdbDocumentName ns})
        \e -> throwError $ error $ "Failure in retrieveDomeinFile for: " <> ns <> ". " <> show e
      onAccepted res.status [200, 304] "retrieveDomeinFile"
        (onCorrectCallAndResponse res.body (\(a :: DomeinFile) -> liftAff $ put a ev))
    (Just avar) -> liftAff $ read avar

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

documentsInDatabase :: DatabaseName -> Aff GetCouchdbAllDocs
documentsInDatabase database = do
  res <- AX.request $ domeinRequest {url = baseURL <> escapeCouchdbDocumentName database <> "/_all_docs"}
  onAccepted res.status [200] "documentsInDatabase"
    (onCorrectCallAndResponse res.body \(a :: GetCouchdbAllDocs) -> pure unit)

documentNamesInDatabase :: DatabaseName -> Aff (Array String)
documentNamesInDatabase database = do
  (GetCouchdbAllDocs cad) <- documentsInDatabase database
  pure $ (\(DocReference{id}) -> id) <$> cad.rows

saveCachedDomeinFile :: ID -> MonadPerspectives Unit
saveCachedDomeinFile ns = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> throwError $ error $ "saveCachedDomeinFile: cannot find domeinfile in cache: " <> ns
    (Just avar) -> do
      df <- liftAff $ read avar
      modifyDomeinFileInCouchdb df avar

-- | Either create or modify the DomeinFile in couchdb. Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
storeDomeinFileInCouchdb :: DomeinFile -> MonadPerspectives Unit
storeDomeinFileInCouchdb df@(DomeinFile {_id}) = do
  mAvar <- domeinCacheLookup _id
  -- mAvar <- liftEffect $ peek domeinCache _id
  case mAvar of
    Nothing -> createDomeinFileInCouchdb df
    (Just avar) -> modifyDomeinFileInCouchdb df avar

createDomeinFileInCouchdb :: DomeinFile -> MonadPerspectives Unit
createDomeinFileInCouchdb df@(DomeinFile dfr@{_id, contexts}) = do
  ev <- (liftAff empty) >>= domeinCacheInsert _id
  res <- liftAff $ AX.put ResponseFormat.string (modelsURL <> escapeCouchdbDocumentName _id) (RequestBody.string (encodeJSON df))
  if res.status == (StatusCode 409)
    then do
      rev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName _id)
      setRevision rev ev
      updatedDomeinFile <- liftAff $ read ev
      modifyDomeinFileInCouchdb updatedDomeinFile ev
    else onAccepted res.status [200, 201] "createDomeinFileInCouchdb"
      (void $ onCorrectCallAndResponse res.body \(a :: PutCouchdbDocument) -> setRevision (unsafePartial $ fromJust $ (unwrap a).rev) ev)
  where
    setRevision :: String -> (AVar DomeinFile) -> MonadPerspectives Unit
    setRevision s av = liftAff $ put (DomeinFile (dfr {_rev = (revision s)})) av

modifyDomeinFileInCouchdb :: DomeinFile -> (AVar DomeinFile) -> MonadPerspectives Unit
modifyDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) av = do
  (DomeinFile {_rev}) <- liftAff $ read av
  originalRevision <- pure $ unsafePartial $ fromJust _rev
  oldDf <- liftAff $ take av
  res <- liftAff $ AX.put
    ResponseFormat.string
    (modelsURL <> escapeCouchdbDocumentName _id <> "?_rev=" <> originalRevision)
    (RequestBody.string (encodeJSON (DomeinFile dfr {_rev = _rev})))
  if res.status == (StatusCode 409)
    then do
      rev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName _id)
      setRevision rev
      updatedDomeinFile <- liftAff $ read av
      modifyDomeinFileInCouchdb updatedDomeinFile av
    else onAccepted res.status [200, 201] "modifyDomeinFileInCouchdb"
      (void (onCorrectCallAndResponse res.body \(a :: PutCouchdbDocument)-> setRevision (unsafePartial $ fromJust $ (unwrap a).rev)))
  where
    setRevision :: String -> MonadPerspectives Unit
    setRevision s = liftAff $ put (DomeinFile (dfr {_rev = (revision s)})) av

-- | Remove the file from couchb. Removes the model from cache.
removeDomeinFileFromCouchdb :: Namespace -> MonadPerspectives Unit
removeDomeinFileFromCouchdb ns = do
  rev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName ns)
  (rq :: (AX.Request String)) <- defaultPerspectRequest
  res <- liftAff $ AX.request $ rq {method = Left DELETE, url = (modelsURL <> escapeCouchdbDocumentName ns <> "?rev=" <> rev)}
  onAccepted res.status [200, 202] "removeDomeinFileFromCouchdb" $ domeinCacheRemove ns *> pure unit

modelsURL :: URL
modelsURL = "http://localhost:5984/perspect_models/"

baseURL :: URL
baseURL = "http://localhost:5984/"

domeinRequest :: AX.Request String
domeinRequest =
  { method: Left GET
  , url: "http://localhost:5984/models2model_SysteemDomein_"
  , headers: []
  , content: Nothing
  , username: Just "cor"
  , password: Just "geheim"
  , withCredentials: true
  , responseFormat: ResponseFormat.string
  }
