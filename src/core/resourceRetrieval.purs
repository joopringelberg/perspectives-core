module Perspectives.ResourceRetrieval
( fetchPerspectEntiteitFromCouchdb
, saveUnversionedEntiteit
, saveVersionedEntiteit
, saveEntiteit
, saveEntiteitPreservingVersion
, fetchEntiteit
  )
where

import Prelude

import Control.Monad.Aff.AVar (AVar, putVar, readVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), stripPrefix, stripSuffix)
import Network.HTTP.Affjax (AffjaxRequest, AffjaxResponse, affjax)
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeaderName, responseHeaderValue)
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Couchdb (PutCouchdbDocument, onAccepted)
import Perspectives.Couchdb.Databases (ensureAuthentication, defaultPerspectRequest)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (deconstructModelName, isQualifiedWithDomein, isUserURI)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, cacheCachedEntiteit, encode, getRevision', readEntiteitFromCache, representInternally, retrieveFromDomein, retrieveInternally, setRevision)
import Perspectives.User (entitiesDatabase)


-- | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
-- TODO rename
fetchPerspectEntiteitFromCouchdb :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) a
fetchPerspectEntiteitFromCouchdb id = if isUserURI id
  then fetchEntiteit id
  else if isQualifiedWithDomein id
    then case deconstructModelName id of
      Nothing -> throwError $ error ("fetchPerspectEntiteitFromCouchdb: Cannot construct namespace out of id " <> id)
      (Just ns) -> do
        ent <- retrieveFromDomein id ns
        v <- representInternally id
        liftAff $ putVar ent v
        pure ent
    else throwError $ error ("fetchPerspectEntiteitFromCouchdb: Unknown URI structure for " <> id)

-- | Fetch the definition of a resource asynchronously.
fetchEntiteit :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) a
fetchEntiteit id = ensureAuthentication $ do
  v <- representInternally id
  -- _ <- forkAff do
  ebase <- entitiesDatabase
  (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
  (res :: AffjaxResponse a) <- liftAff $ affjax $ rq {url = ebase <> id}
  liftAff $ onAccepted res.status [200] "fetchEntiteit" $ putVar res.response v
  liftAff $ readVar v

saveEntiteitPreservingVersion :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) a
-- saveEntiteitPreservingVersion id = catchError do
--     (_ :: a) <- fetchPerspectEntiteitFromCouchdb id
--     saveEntiteit id
--   \e -> saveEntiteit id
saveEntiteitPreservingVersion = saveEntiteit

saveEntiteit :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) a
saveEntiteit id = do
  pe <- readEntiteitFromCache id
  case unNullOrUndefined $ getRevision' pe of
    Nothing -> saveUnversionedEntiteit id
    otherwise -> saveVersionedEntiteit id pe

-- | A Resource may be created and stored locally, but not sent to the couchdb. Send such resources to
-- | couchdb with this function.
saveUnversionedEntiteit :: forall e a. PerspectEntiteit a => ID -> MonadPerspectives (AjaxAvarCache e) a
saveUnversionedEntiteit id = ensureAuthentication $ do
  (mAvar :: Maybe (AVar a)) <- retrieveInternally id
  case mAvar of
    Nothing -> throwError $ error ("saveUnversionedEntiteit needs a locally stored resource for " <> id)
    (Just avar) -> do
      pe <- liftAff $ takeVar avar
      ebase <- entitiesDatabase
      (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
      -- (res :: AffjaxResponse PutCouchdbDocument) <- liftAff $ put (ebase <> escapeCouchdbDocumentName id) (encode pe)
      (res :: AffjaxResponse PutCouchdbDocument) <- liftAff $ affjax $ rq {method = Left PUT, url = (ebase <> id), content = Just (encode pe)}
      if res.status == (StatusCode 409)
        then retrieveEntiteitVersion id >>= pure <<< (flip setRevision pe) >>= void <<< saveVersionedEntiteit id
        else liftAff $ onAccepted res.status [200, 201] "saveUnversionedEntiteit"
          $ putVar (setRevision (unsafePartial $ fromJust $ unNullOrUndefined (unwrap res.response).rev) pe) avar
      pure pe

retrieveEntiteitVersion :: forall e. ID -> MonadPerspectives (AjaxAvarCache e) String
retrieveEntiteitVersion id = do
  (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
  ebase <- entitiesDatabase
  (res :: AffjaxResponse Unit) <- liftAff $ affjax $ rq {method = Left HEAD, url = (ebase <> id)}
  vs <- version res.headers
  liftAff $ onAccepted res.status [200, 304] "retrieveEntiteitVersion" (pure vs)
  where
    version :: Array ResponseHeader -> MonadPerspectives (AjaxAvarCache e) String
    version headers =  case find (\rh -> (responseHeaderName rh) == "ETag") headers of
      Nothing -> throwError $ error ("retrieveEntiteitVersion: couchdb returns no ETag header holding a document version number for " <> id)
      (Just h) -> (pure $ responseHeaderValue h) >>= removeDoubleQuotes
      -- pure $ removeDoubleQuotes $ responseHeaderValue h -- ""53-46f66252f91f7b88ce23623de06eca77""


    removeDoubleQuotes :: String -> MonadPerspectives (AjaxAvarCache e) String
    removeDoubleQuotes s = do
      (ms1 :: Maybe String) <- pure $ stripSuffix (Pattern "\"") s
      case ms1 of
        Nothing -> throwError $ error ("retrieveEntiteitVersion: couchdb returns ETag value WITHOUT double quotes for " <> id)
        (Just s1) -> do
          ms2 <- pure $ stripPrefix (Pattern "\"") s1
          case ms2 of
            Nothing -> throwError $ error ("retrieveEntiteitVersion: couchdb returns ETag value WITHOUT double quotes for " <> id)
            (Just s2) -> pure s2

saveVersionedEntiteit :: forall e a. PerspectEntiteit a => ID -> a -> MonadPerspectives (AjaxAvarCache e) a
saveVersionedEntiteit entId entiteit = ensureAuthentication $ do
  case (unNullOrUndefined (getRevision' entiteit)) of
    Nothing -> throwError $ error ("saveVersionedEntiteit: entiteit has no revision, deltas are impossible: " <> entId)
    (Just rev) -> do
      ebase <- entitiesDatabase
      (rq :: (AffjaxRequest Unit)) <- defaultPerspectRequest
      -- (res :: AffjaxResponse PutCouchdbDocument) <- liftAff $ put (ebase <> escapeCouchdbDocumentName entId <> "?_rev=" <> rev) (encode entiteit)
      (res :: AffjaxResponse PutCouchdbDocument) <- liftAff $ affjax $ rq {method = Left PUT, url = (ebase <> entId <> "?_rev=" <> rev), content = Just (encode entiteit)}
      onAccepted res.status [200, 201] "saveVersionedEntiteit"
        $ cacheCachedEntiteit entId (setRevision (unwrap res.response).rev entiteit)
