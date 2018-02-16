module Perspectives.DomeinCache
-- ( retrieveDomeinResourceDefinition )

where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, putVar, readVar, takeVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.StrMap (StrMap, empty, lookup)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax, put)
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.Effects (AjaxAvarCache, AjaxAvar)
import Perspectives.EntiteitAndRDFAliases (ContextID, RolID)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)
import Perspectives.Identifiers (Namespace, escapeCouchdbDocumentName)
import Perspectives.Syntax (PerspectContext, PerspectRol, Revision, fromRevision, noRevision, revision)
import Prelude (Unit, bind, pure, show, unit, ($), (*>), (<$>), (<>), (==), (||))

newtype DomeinFile = DomeinFile
  { _rev :: Revision
  , _id :: String
  , contexts :: DomeinFileContexts
  , roles ::DomeinFileRoles
  }

derive instance genericDomeinFile :: Generic DomeinFile _

instance encodeDomeinFile :: Encode DomeinFile where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance decodeDomeinFile :: Decode DomeinFile where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

defaultDomeinFile :: DomeinFile
defaultDomeinFile = DomeinFile{ _rev: noRevision, _id: "", contexts: empty, roles: empty}

newtype CouchdbPutResponse = CouchdbPutResponse
  { ok :: Boolean
  , id :: String
  , rev :: String}

derive instance genericCouchdbPutResponse :: Generic CouchdbPutResponse _

instance decodeCouchdbPutResponse :: Decode CouchdbPutResponse where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-- | DomeinFileContexts is an immutable map of resource type names to PerspectContexts.
type DomeinFileContexts = StrMap PerspectContext

type DomeinFileRoles = StrMap PerspectRol

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
      _ <- case res.status of
        StatusCode 200 ->
          case runExcept (decodeJSON res.response) of
            (Left m) -> throwError $ error ("retrieveDomeinFile cannot parse response from couchdb for " <> ns <> ": " <> show m)
            (Right df) -> putVar df ev
        otherwise -> throwError $ error ("retrieveDomeinFile " <> ns <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
      readVar ev
    (Just avar) -> readVar avar

newtype CouchdbAllDocs = CouchdbAllDocs
  { offset :: Int
  , rows :: Array DocReference
  , total_rows :: Int
  , update_seq :: NullOrUndefined Int
  }

derive instance genericCouchdbAllDocs :: Generic CouchdbAllDocs _

instance decodeCouchdbAllDocs :: Decode CouchdbAllDocs where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype DocReference = DocReference { id :: String, value :: Rev}

derive instance genericDocReference :: Generic DocReference _

instance decodeDocReference :: Decode DocReference where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype Rev = Rev { rev :: String}

derive instance genericRef :: Generic Rev _

instance decodeRev :: Decode Rev where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

documentsInDatabase :: forall e. DatabaseName -> Aff (ajax :: AJAX | e) CouchdbAllDocs
documentsInDatabase database = do
  res <- affjax $ domeinRequest {url = baseURL <> escapeCouchdbDocumentName database <> "/_all_docs"}
  case res.status of
    StatusCode 200 ->
      case runExcept (decodeJSON res.response) of
        (Left m) -> throwError $ error ("documentsInDatabase " <> database <> " cannot parse response from couchdb: " <> show m)
        (Right cad) -> pure cad
    otherwise -> throwError $ error ("documentsInDatabase " <> database <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")

documentNamesInDatabase :: forall e. DatabaseName -> Aff (ajax :: AJAX | e) (Array String)
documentNamesInDatabase database = do
  (CouchdbAllDocs cad) <- documentsInDatabase database
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
  -- TODO. Misschien een uitgebreidere analyse van de statuscodes? Zie http://127.0.0.1:5984/_utils/docs/api/document/common.html
  (res :: AffjaxResponse String)  <- put (modelsURL <> escapeCouchdbDocumentName _id) (encodeJSON df)
  (StatusCode n) <- pure res.status
  case n == 200 || n == 201 of
    true -> do
      case runExcept (decodeJSON res.response) of
        (Left m) -> throwError $ error ("createDomeinFileInCouchdb cannot parse response from couchdb for " <> _id <> ": " <> show m)
        (Right (CouchdbPutResponse {rev})) -> putVar (DomeinFile (dfr {_rev = (revision rev)})) ev
    false -> throwError $ error ("createDomeinFileInCouchdb " <> _id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")


modifyDomeinFileInCouchdb :: forall e. DomeinFile -> (AVar DomeinFile) -> Aff (AjaxAvar e) Unit
modifyDomeinFileInCouchdb df@(DomeinFile dfr@{_id}) av = do
  (DomeinFile {_rev}) <- readVar av
  originalRevision <- pure $ unsafePartial $ fromJust $ fromRevision _rev
  oldDf <- takeVar av
  -- TODO. Misschien een uitgebreidere analyse van de statuscodes? Zie http://127.0.0.1:5984/_utils/docs/api/document/common.html
  (res :: AffjaxResponse String) <- put (modelsURL <> escapeCouchdbDocumentName _id <> "?_rev=" <> originalRevision) (encodeJSON (DomeinFile dfr {_rev = _rev}))
  (StatusCode n) <- pure res.status
  case n == 200 || n == 201 of
    true -> do
      case runExcept (decodeJSON res.response) of
        (Left m) -> throwError $ error ("modifyDomeinFileInCouchdb cannot parse response from couchdb for " <> _id <> ": " <> show m)
        (Right (CouchdbPutResponse {rev})) -> putVar (DomeinFile (dfr {_rev = (revision rev)})) av
    false -> throwError $ error ("modifyDomeinFileInCouchdb " <> _id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")

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
