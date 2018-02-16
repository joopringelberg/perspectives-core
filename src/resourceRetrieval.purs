module Perspectives.ResourceRetrieval
( fetchPerspectEntiteitFromCouchdb
, saveUnversionedEntiteit
, saveVersionedEntiteit
, saveEntiteit
  )
where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, isEmptyVar, putVar, takeVar)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax, put)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.Effects (AjaxAvarCache, AjaxAvar)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.EntiteitCache (stringToRecord)
import Perspectives.Identifiers (deconstructNamespace, escapeCouchdbDocumentName, getStandardNamespace, isQualifiedWithDomein, isStandardNamespaceCURIE, isUserURI)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, cacheCachedEntiteit, decode, encode, getRevision', readEntiteitFromCache, representInternally, retrieveFromDomein, retrieveInternally, setRevision)


-- | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
-- TODO rename
fetchPerspectEntiteitFromCouchdb :: forall e a. PerspectEntiteit a => ID -> Aff (AjaxAvarCache e) a
fetchPerspectEntiteitFromCouchdb id = if isUserURI id
  then fetchEntiteit id
  else if isQualifiedWithDomein id
    then case deconstructNamespace id of
      Nothing -> throwError $ error ("fetchPerspectEntiteitFromCouchdb: Cannot construct namespace out of id " <> id)
      (Just ns) -> retrieveFromDomein id ns
    else if isStandardNamespaceCURIE id
      then case getStandardNamespace id of
        Nothing -> throwError $ error ("fetchPerspectEntiteitFromCouchdb: Cannot construct standard namespace out of id " <> id)
        (Just ns) -> retrieveFromDomein id ns
      else throwError $ error ("fetchPerspectEntiteitFromCouchdb: Unknown URI structure for " <> id)

-- | Fetch the definition of a resource asynchronously.
fetchEntiteit :: forall e a. PerspectEntiteit a => ID -> Aff (AjaxAvarCache e) a
fetchEntiteit id = do
  v <- representInternally id
  -- _ <- forkAff do
  res <- affjax $ userResourceRequest {url = baseURL <> id}
  case res.status of
    StatusCode 200 ->
      case decode res.response of
        (Left message) -> throwError $ error (show message <> " (" <> id <> ")")
        (Right pe) -> putVar pe v
    otherwise -> throwError $ error ("fetchEntiteit " <> id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  takeVar v


saveEntiteit :: forall e a. PerspectEntiteit a => ID -> Aff (AjaxAvarCache e) a
saveEntiteit id = do
  pe <- readEntiteitFromCache id
  case unNullOrUndefined $ getRevision' pe of
    Nothing -> saveUnversionedEntiteit id
    otherwise -> saveVersionedEntiteit id pe
{-
	- haal AVar op
	- indien leeg, breek af (want de operatie is kennelijk al in uitvoering)
	- anders: lees uit met takeVar
	- sla op in couchdb, ontvang revision
	- na afloop: vul AVar met coudhbresource met revision
-}
-- | A Resource may be created and stored locally, but not sent to the couchdb. Send such resources to
-- | couchdb with this function.
saveUnversionedEntiteit :: forall e a. PerspectEntiteit a => ID -> Aff (AjaxAvarCache e) a
saveUnversionedEntiteit id = do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("storeCachedButUnsafedPerspectEntiteitInCouchdb needs a locally stored resource for " <> id)
    (Just avar) -> do
      empty <- isEmptyVar avar
      if empty
        then throwError $ error ("storeCachedButUnsafedPerspectEntiteitInCouchdb needs a locally stored and filled resource for " <> id)
        else do
          pe <- takeVar avar
          (rev :: String) <- save' id (encode pe)
          putVar (setRevision rev pe) avar
          pure pe
  where

    -- | Create a document in couchdb that is not yet there (so, no revision available!).
    save' :: forall eff. ID -> String -> Aff (ajax :: AJAX | eff) String
    save' resId resource =
      do
        -- TODO. Misschien een uitgebreidere analyse van de statuscodes? Zie http://127.0.0.1:5984/_utils/docs/api/document/common.html
        res <- put (baseURL <> escapeCouchdbDocumentName resId) resource
        (StatusCode n) <- pure res.status
        case n == 200 || n == 201 of
          true ->
            pure $ _.rev $ stringToRecord res.response
          false -> throwError $ error ("save' " <> resId <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")

saveVersionedEntiteit :: forall e a. PerspectEntiteit a => ID -> a -> Aff (AjaxAvarCache e) a
saveVersionedEntiteit entId entiteit = do
  case (unNullOrUndefined (getRevision' entiteit)) of
    Nothing -> throwError $ error ("saveVersionedEntiteit: entiteit has no revision, deltas are impossible: " <> entId)
    (Just rev) -> do
      -- Store the changed entity in couchdb.
      newRev <- modifyResourceInCouchdb entId rev (encode entiteit)
      -- Set the new revision in the entity.
      cacheCachedEntiteit entId (setRevision newRev entiteit)
  where
    modifyResourceInCouchdb :: forall eff.ID -> String -> String -> Aff (AjaxAvar eff) String
    modifyResourceInCouchdb resId originalRevision resource = do
      -- TODO. Misschien een uitgebreidere analyse van de statuscodes? Zie http://127.0.0.1:5984/_utils/docs/api/document/common.html
      (res :: AffjaxResponse String) <- put (baseURL <> escapeCouchdbDocumentName resId <> "?_rev=" <> originalRevision) resource
      (StatusCode n) <- pure res.status
      case n == 200 || n == 201 of
        true -> do
          pure $ _.rev $ stringToRecord res.response
        false -> throwError $ error ("modifyResourceInCouchdb " <> resId <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")

-- TODO: gebruik de user.
baseURL :: String
baseURL = "http://localhost:5984/user_cor_contexts2/"

userResourceRequest :: AffjaxRequest Unit
userResourceRequest =
  { method: Left GET
  , url: "http://localhost:5984/user_cor_contexts2/user:xGebruiker"
  , headers: []
  , content: Nothing
  , username: Just "cor"
  , password: Just "geheim"
  , withCredentials: true
  }
