module Perspectives.DomeinCache
-- ( retrieveDomeinResourceDefinition )

where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, readVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Foreign.Generic (encodeJSON)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.StrMap (lookup)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax, put)
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (DocReference(..), GetCouchdbAllDocs(..), PutCouchdbDocument, onAccepted)
import Perspectives.Couchdb.Databases (retrieveDocumentVersion)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache, AvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, RolID, ID)
import Perspectives.GlobalUnsafeStrMap (poke)
import Perspectives.Identifiers (Namespace, escapeCouchdbDocumentName)
import Perspectives.PerspectivesState (domeinCache, domeinCacheInsert, domeinCacheLookup)
import Perspectives.Syntax (PerspectContext, PerspectRol, revision)
import Prelude (Unit, bind, discard, pure, ($), (*>), (<$>), (<>), (>>=), (==))

type URL = String

storeDomeinFileInCache :: forall e. Namespace -> AVar DomeinFile -> MonadPerspectives (AvarCache e) (AVar DomeinFile)
storeDomeinFileInCache ns df= do
  dc <- domeinCache
  liftAff $ liftEff $ poke dc ns df *> pure df

-- | Change the domeinfile in cache. NOTA BENE: does not store the modified file in Couchdb!
modifyDomeinFileInCache :: forall e. Namespace -> (DomeinFile -> DomeinFile) -> MonadPerspectives (AvarCache e) Unit
modifyDomeinFileInCache ns modifier = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> throwError $ error $ "modifyDomeinFileInCache cannot find domeinfile in cache: " <> ns
    (Just avar) -> do
      df <- liftAff $ takeVar avar
      liftAff $ putVar (modifier df) avar

-- | Fetch a PerspectContext asynchronously from its Domein, loading the Domein file if necessary.
retrieveContextFromDomein :: forall e.
  ContextID
  -> Namespace
  -> (MonadPerspectives (AjaxAvarCache e) PerspectContext)
retrieveContextFromDomein id ns = do
  (DomeinFile {contexts}) <- retrieveDomeinFile ns
  case lookup id contexts of
    Nothing -> throwError $ error ("retrieveContextFromDomein: cannot find definition of " <> id <> " in retrieveContextFromDomein for " <> ns)
    (Just context) -> pure context

-- | Fetch a PerspectRol asynchronously from its Domein, loading the Domein file if necessary.
retrieveRolFromDomein :: forall e.
  RolID
  -> Namespace
  -> (MonadPerspectives (AjaxAvarCache e) PerspectRol)
retrieveRolFromDomein id ns = do
  (DomeinFile {roles}) <- retrieveDomeinFile ns
  case lookup id roles of
    Nothing -> throwError $ error ("retrieveRolFromDomein: cannot find definition of " <> id <> " in retrieveRolFromDomein for " <> ns)
    (Just rol) -> pure rol

retrieveDomeinFile :: forall e. Namespace -> MonadPerspectives (AjaxAvarCache e) DomeinFile
retrieveDomeinFile ns = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> do
      ev <- (liftAff makeEmptyVar) >>= domeinCacheInsert ns
      -- forkAff hinders catchError.
      -- _ <- forkAff do
      res <- liftAff $ affjax $ domeinRequest {url = modelsURL <> escapeCouchdbDocumentName ns}
      liftAff $ onAccepted res.status [200, 304] "retrieveDomeinFile" $ putVar res.response ev
      liftAff $ readVar ev
    (Just avar) -> liftAff $ readVar avar

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

documentsInDatabase :: forall e. DatabaseName -> Aff (ajax :: AJAX | e) GetCouchdbAllDocs
documentsInDatabase database = do
  res <- affjax $ domeinRequest {url = baseURL <> escapeCouchdbDocumentName database <> "/_all_docs"}
  onAccepted res.status [200] "documentsInDatabase"
    $ pure res.response

documentNamesInDatabase :: forall e. DatabaseName -> Aff (ajax :: AJAX | e) (Array String)
documentNamesInDatabase database = do
  (GetCouchdbAllDocs cad) <- documentsInDatabase database
  pure $ (\(DocReference{id}) -> id) <$> cad.rows

saveCachedDomeinFile :: forall e. ID -> MonadPerspectives (AjaxAvarCache e) Unit
saveCachedDomeinFile ns = do
  mAvar <- domeinCacheLookup ns
  case mAvar of
    Nothing -> throwError $ error $ "saveCachedDomeinFile: cannot find domeinfile in cache: " <> ns
    (Just avar) -> do
      df <- liftAff $ readVar avar
      modifyDomeinFileInCouchdb df avar

-- | Either create or modify the DomeinFile in couchdb. Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
storeDomeinFileInCouchdb :: forall e. DomeinFile -> MonadPerspectives (AjaxAvarCache e) Unit
storeDomeinFileInCouchdb df@(DomeinFile {_id}) = do
  mAvar <- domeinCacheLookup _id
  -- mAvar <- liftEff $ peek domeinCache _id
  case mAvar of
    Nothing -> createDomeinFileInCouchdb df
    (Just avar) -> modifyDomeinFileInCouchdb df avar

createDomeinFileInCouchdb :: forall e. DomeinFile -> MonadPerspectives (AjaxAvarCache e) Unit
createDomeinFileInCouchdb df@(DomeinFile dfr@{_id, contexts}) = do
  ev <- (liftAff makeEmptyVar) >>= domeinCacheInsert _id
  (res :: AffjaxResponse PutCouchdbDocument)  <- liftAff $ put (modelsURL <> escapeCouchdbDocumentName _id) (encodeJSON df)
  if res.status == (StatusCode 409)
    then do
      rev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName _id)
      setRevision rev ev
      updatedDomeinFile <- liftAff $ readVar ev
      modifyDomeinFileInCouchdb updatedDomeinFile ev
    else onAccepted res.status [200, 201] "createDomeinFileInCouchdb" $ setRevision (unsafePartial $ fromJust $ (unwrap res.response).rev) ev
  where
    setRevision :: String -> (AVar DomeinFile) -> MonadPerspectives (AjaxAvarCache e) Unit
    setRevision s av = liftAff $ putVar (DomeinFile (dfr {_rev = (revision s)})) av

modifyDomeinFileInCouchdb :: forall e. DomeinFile -> (AVar DomeinFile) -> MonadPerspectives (AjaxAvarCache e) Unit
modifyDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) av = do
  (DomeinFile {_rev}) <- liftAff $ readVar av
  originalRevision <- pure $ unsafePartial $ fromJust _rev
  oldDf <- liftAff $ takeVar av
  (res :: AffjaxResponse PutCouchdbDocument) <- liftAff $ put
    (modelsURL <> escapeCouchdbDocumentName _id <> "?_rev=" <> originalRevision)
    (encodeJSON (DomeinFile dfr {_rev = _rev}))
  if res.status == (StatusCode 409)
    then do
      rev <- retrieveDocumentVersion (modelsURL <> escapeCouchdbDocumentName _id)
      setRevision rev
      updatedDomeinFile <- liftAff $ readVar av
      modifyDomeinFileInCouchdb updatedDomeinFile av
    else onAccepted res.status [200, 201] "modifyDomeinFileInCouchdb" $ setRevision (unsafePartial $ fromJust $ (unwrap res.response).rev)
  where
    setRevision :: String -> MonadPerspectives (AjaxAvarCache e) Unit
    setRevision s = liftAff $ putVar (DomeinFile (dfr {_rev = (revision s)})) av

modelsURL :: URL
modelsURL = "http://localhost:5984/perspect_models/"

baseURL :: URL
baseURL = "http://localhost:5984/"

domeinRequest :: AffjaxRequest Unit
domeinRequest =
  { method: Left GET
  , url: "http://localhost:5984/models2model_SysteemDomein_"
  , headers: []
  , content: Nothing
  , username: Just "cor"
  , password: Just "geheim"
  , withCredentials: true
  }
