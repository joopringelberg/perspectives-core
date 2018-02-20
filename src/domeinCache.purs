module Perspectives.DomeinCache
-- ( retrieveDomeinResourceDefinition )

where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, readVar, takeVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.StrMap (StrMap, empty, lookup)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax, put)
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.Couchdb (DocReference(..), GetCouchdbAllDocs(..), PutCouchdbDocument, onAccepted)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache, AjaxAvar)
import Perspectives.EntiteitAndRDFAliases (ContextID, RolID)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)
import Perspectives.Identifiers (Namespace, escapeCouchdbDocumentName)
import Perspectives.Syntax (PerspectContext, PerspectRol, fromRevision, revision)
import Prelude (Unit, bind, pure, unit, ($), (*>), (<$>), (<>), discard)

-- | The global index of all cached Domein files, indexed by namespace name, is a mutable unsafe map.
type DomeinCache = GLStrMap (AVar DomeinFile)

type URL = String

domeinCache :: DomeinCache
domeinCache = new unit

storeDomeinFileInCache :: forall e. Namespace -> AVar DomeinFile -> Aff (gm :: GLOBALMAP | e) (AVar DomeinFile)
storeDomeinFileInCache ns df= liftEff $ poke domeinCache ns df *> pure df

-- | Fetch a PerspectContext asynchronously from its Domein, loading the Domein file if necessary.
retrieveContextFromDomein :: forall e.
  ContextID
  -> Namespace
  -> (Aff (AjaxAvarCache e) PerspectContext)
retrieveContextFromDomein id ns = do
  (DomeinFile {contexts}) <- retrieveDomeinFile ns
  case lookup id contexts of
    Nothing -> throwError $ error ("retrieveDomeinResourceDefinition: cannot find definition of " <> id <> " in DomeinFileContexts for " <> ns)
    (Just context) -> pure context

-- | Fetch a PerspectRol asynchronously from its Domein, loading the Domein file if necessary.
retrieveRolFromDomein :: forall e.
  RolID
  -> Namespace
  -> (Aff (AjaxAvarCache e) PerspectRol)
retrieveRolFromDomein id ns = do
  (DomeinFile {roles}) <- retrieveDomeinFile ns
  case lookup id roles of
    Nothing -> throwError $ error ("retrieveDomeinResourceDefinition: cannot find definition of " <> id <> " in DomeinFileContexts for " <> ns)
    (Just rol) -> pure rol

retrieveDomeinFile :: forall e. Namespace -> Aff (AjaxAvarCache e) DomeinFile
retrieveDomeinFile ns = do
  mAvar <- liftEff $ peek domeinCache ns
  case mAvar of
    Nothing -> do
      ev <- makeEmptyVar
      _ <- liftEff $ poke domeinCache ns ev
      -- forkAff hinders catchError.
      -- _ <- forkAff do
      res <- affjax $ domeinRequest {url = modelsURL <> escapeCouchdbDocumentName ns}
      onAccepted res.status [200, 304] "retrieveDomeinFile" $ putVar res.response ev
      readVar ev
    (Just avar) -> readVar avar

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

-- | Either create or modify the DomeinFile in couchdb. Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
storeDomeinFileInCouchdb :: forall e. DomeinFile -> Aff (AjaxAvarCache e) Unit
storeDomeinFileInCouchdb df@(DomeinFile {_id}) = do
  mAvar <- liftEff $ peek domeinCache _id
  case mAvar of
    Nothing -> createDomeinFileInCouchdb df
    (Just avar) -> modifyDomeinFileInCouchdb df avar

createDomeinFileInCouchdb :: forall e. DomeinFile -> Aff (AjaxAvarCache e) Unit
createDomeinFileInCouchdb df@(DomeinFile dfr@{_id, contexts}) = do
  ev <- makeEmptyVar
  _ <- liftEff $ poke domeinCache _id ev
  (res :: AffjaxResponse PutCouchdbDocument)  <- put (modelsURL <> escapeCouchdbDocumentName _id) (encodeJSON df)
  onAccepted res.status [200, 201] "createDomeinFileInCouchdb"
    $ putVar (DomeinFile (dfr {_rev = (revision (_.rev (unwrap res.response)))})) ev

modifyDomeinFileInCouchdb :: forall e. DomeinFile -> (AVar DomeinFile) -> Aff (AjaxAvar e) Unit
modifyDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) av = do
  (DomeinFile {_rev}) <- readVar av
  originalRevision <- pure $ unsafePartial $ fromJust $ fromRevision _rev
  oldDf <- takeVar av
  (res :: AffjaxResponse PutCouchdbDocument) <- put
    (modelsURL <> escapeCouchdbDocumentName _id <> "?_rev=" <> originalRevision)
    (encodeJSON (DomeinFile dfr {_rev = _rev}))
  onAccepted res.status [200, 201] "modifyDomeinFileInCouchdb"
    $ putVar (DomeinFile (dfr {_rev = (revision (_.rev (unwrap res.response)))})) av

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
