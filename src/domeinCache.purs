module Perspectives.DomeinCache
-- ( retrieveDomeinResourceDefinition )

where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, putVar, readVar, takeVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Argonaut (jsonParser, toObject)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, lookup)
import Data.String (null)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, put)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)
import Perspectives.Identifiers (Namespace, escapeCouchdbDocumentName)
import Perspectives.ResourceTypes (AsyncDomeinFile, CouchdbResource, Resource, recordToJson, stringToRecord)
import Perspectives.Syntax (ID)
import Prelude (Unit, bind, pure, show, unit, ($), (*>), (<$>), (<>), (==), (||))

type DomeinFile = { _id :: ID, _rev :: String, contexts :: DomeinFileContexts}

-- | DomeinFileContexts is an immutable map of resource type names to resource definitions in the form of PropDefs.
type DomeinFileContexts = StrMap CouchdbResource

-- | The global index of all cached Domein files, indexed by namespace name, is a mutable unsafe map.
type DomeinCache = GLStrMap (AVar DomeinFile)

type URL = String

domeinCache :: DomeinCache
domeinCache = new unit

storeDomeinFileInCache :: forall e. Namespace -> AVar DomeinFile -> Aff (gm :: GLOBALMAP | e) (AVar DomeinFile)
storeDomeinFileInCache ns df= liftEff $ poke domeinCache ns df *> pure df

-- | Fetch the definition of a resource asynchronously from its Domein.
retrieveDomeinResourceDefinition :: forall e.
  Resource
  -> Namespace
  -> (AsyncDomeinFile e CouchdbResource)
retrieveDomeinResourceDefinition id ns = do
  ({contexts}) <- retrieveDomeinFile ns
  case lookup id contexts of
    Nothing -> throwError $ error ("retrieveDomeinResourceDefinition: cannot find definition of " <> id <> " in DomeinFileContexts for " <> ns)
    (Just propDefs) -> pure propDefs

retrieveDomeinFile :: forall e. Namespace -> AsyncDomeinFile e DomeinFile
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
        StatusCode 200 -> putVar (stringToRecord res.response) ev
        otherwise -> throwError $ error ("retrieveDomeinFile " <> ns <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
      readVar ev
    (Just avar) -> readVar avar

-- | There can be two error scenarios here: either the returned string cannot be parsed
-- | by JSON.parse, or the resulting json is not an object. Neither is likely, because Couchdb
-- | will not store such documents.
stringToPropDefs :: String -> Either String CouchdbResource
stringToPropDefs s = case jsonParser s of
    (Left err) -> Left $ "stringToPropDefs: cannot parse: " <> s
    (Right json) ->
      case toObject json of
        Nothing -> Left $ "stringToPropDefs: parsed json is not an object!"
        (Just obj) -> Right obj

newtype CouchdbAllDocs = CouchdbAllDocs
  { offset :: Int
  , rows :: Array { id :: String, value :: { rev :: String}}
  , total_rows :: Int
  , update_seq :: Int
  }

-- | A name not preceded or followed by a forward slash.
type DatabaseName = String

documentsInDatabase :: forall e. DatabaseName -> Aff (ajax :: AJAX | e) CouchdbAllDocs
documentsInDatabase database = do
  res <- affjax $ domeinRequest {url = baseURL <> escapeCouchdbDocumentName database <> "/_all_docs"}
  case res.status of
    StatusCode 200 -> pure $ CouchdbAllDocs (stringToRecord res.response )
    otherwise -> throwError $ error ("documentsInDatabase " <> database <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")

documentNamesInDatabase :: forall e. DatabaseName -> Aff (ajax :: AJAX | e) (Array String)
documentNamesInDatabase database = do
  (CouchdbAllDocs cad) <- documentsInDatabase database
  pure $ (\({id}) -> id) <$> cad.rows

-- | Either create or modify the DomeinFile in couchdb. Do not use createDomeinFileInCouchdb or modifyDomeinFileInCouchdb directly.
storeDomeinFileInCouchdb :: forall e. DomeinFile -> Aff (ajax :: AJAX, avar :: AVAR, gm :: GLOBALMAP | e) Unit
storeDomeinFileInCouchdb df@{_id} = do
  mAvar <- liftEff $ peek domeinCache _id
  case mAvar of
    Nothing -> createDomeinFileInCouchdb df
    (Just avar) -> modifyDomeinFileInCouchdb df avar

createDomeinFileInCouchdb :: forall e. DomeinFile -> Aff (ajax :: AJAX, avar :: AVAR, gm :: GLOBALMAP | e) Unit
createDomeinFileInCouchdb df@{_id, contexts} = do
  ev <- makeEmptyVar
  _ <- liftEff $ poke domeinCache _id ev
  -- TODO. Misschien een uitgebreidere analyse van de statuscodes? Zie http://127.0.0.1:5984/_utils/docs/api/document/common.html
  res <- put (modelsURL <> escapeCouchdbDocumentName _id) (recordToJson {_id: _id, contexts: contexts})
  (StatusCode n) <- pure res.status
  case n == 200 || n == 201 of
    true -> putVar (df {_rev = (_.rev $ stringToRecord res.response)}) ev
    false -> throwError $ error ("createDomeinFileInCouchdb " <> _id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")

modifyDomeinFileInCouchdb :: forall e. DomeinFile -> (AVar DomeinFile) -> Aff (ajax :: AJAX, avar :: AVAR, gm :: GLOBALMAP | e) Unit
modifyDomeinFileInCouchdb df@{_id, _rev} av =
  if null _rev
    then do
      oldDf <- takeVar av
      -- TODO. Misschien een uitgebreidere analyse van de statuscodes? Zie http://127.0.0.1:5984/_utils/docs/api/document/common.html
      res <- put (modelsURL <> escapeCouchdbDocumentName _id <> "?_rev=" <> _rev) (recordToJson df)
      (StatusCode n) <- pure res.status
      case n == 200 || n == 201 of
        true -> do
          ({rev}) <- pure (stringToRecord res.response)
          putVar (df {_rev = rev}) av
        false -> throwError $ error ("createDomeinFileInCouchdb " <> _id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
    -- This may happen if a second create scenario overtakes the first one. The initial DomeinFile has Nothing as revision.
    else pure unit

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
