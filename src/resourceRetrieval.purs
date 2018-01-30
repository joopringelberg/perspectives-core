module Perspectives.ResourceRetrieval
( fetchPerspectEntiteitFromCouchdb
, createResourceInCouchdb
  )
where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (putVar, takeVar)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, put, affjax)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.Identifiers (escapeCouchdbDocumentName, getNamespace, getStandardNamespace, isQualifiedWithDomein, isStandardNamespaceCURIE)
import Perspectives.PerspectEntiteit (class PerspectEntiteit, decode, representInternally, retrieveFromDomein)
import Perspectives.ResourceTypes (AsyncDomeinFile, stringToRecord)
import Perspectives.Syntax (ID)

-- | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
-- fetchPropDefs :: forall e. Resource -> (AsyncDomeinFile e PropDefs)
-- fetchPropDefs :: forall e. Resource -> (AsyncDomeinFile e PropDefs)
-- fetchPropDefs id = do
--   r <- fetchPerspectEntiteitFromCouchdb id
--   pure $ PropDefs r

-- | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
-- fetchPropDefs :: forall e. Resource -> (AsyncDomeinFile e PropDefs)
fetchPerspectEntiteitFromCouchdb :: forall e a. PerspectEntiteit a => ID -> AsyncDomeinFile e a
fetchPerspectEntiteitFromCouchdb id = if isQualifiedWithDomein id
  then case getNamespace id of
    Nothing -> throwError $ error ("Cannot construct namespace out of id " <> id)
    (Just ns) -> retrieveFromDomein id ns
  else if isStandardNamespaceCURIE id
    then case getStandardNamespace id of
      Nothing -> throwError $ error ("Cannot construct standard namespace out of id " <> id)
      (Just ns) -> retrieveFromDomein id ns
    else fetchIndividualCouchDbDefinition id

-- | Fetch the definition of a resource asynchronously.
fetchIndividualCouchDbDefinition :: forall e a. PerspectEntiteit a => ID -> AsyncDomeinFile e a
fetchIndividualCouchDbDefinition id = do
  v <- representInternally id
  -- _ <- forkAff do
  res <- affjax $ userResourceRequest {url = baseURL <> id}
  case res.status of
    StatusCode 200 ->
      case decode res.response of
        (Left message) -> throwError $ error (show message <> " (" <> id <> ")")
        (Right pe) -> putVar pe v
    otherwise -> throwError $ error ("fetchIndividualCouchDbDefinition " <> id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  takeVar v

-- | Create a document in couchdb that is not yet there (so, no revision available!).
createResourceInCouchdb :: forall e. ID -> String -> Aff (ajax :: AJAX | e) String
createResourceInCouchdb resId resource =
  do
    -- TODO. Misschien een uitgebreidere analyse van de statuscodes? Zie http://127.0.0.1:5984/_utils/docs/api/document/common.html
    res <- put (baseURL <> escapeCouchdbDocumentName resId) resource
    (StatusCode n) <- pure res.status
    case n == 200 || n == 201 of
      true ->
        pure $ _.rev $ stringToRecord res.response
      false -> throwError $ error ("createResourceInCouchdb " <> resId <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")

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
