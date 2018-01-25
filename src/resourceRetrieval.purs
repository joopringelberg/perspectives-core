module Perspectives.ResourceRetrieval
( fetchPropDefs
, fetchCouchdbResource
, storeCouchdbResource
  )
where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (makeEmptyVar, putVar, takeVar)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Argonaut (toString)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (lookup)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, put, affjax)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.DomeinCache (retrieveDomeinResourceDefinition, stringToPropDefs)
import Perspectives.Identifiers (escapeCouchdbDocumentName, getNamespace, getStandardNamespace, isDomeinURI, isStandardNamespaceCURIE)
import Perspectives.ResourceTypes (Resource, AsyncResource, AsyncDomeinFile, PropDefs(..), CouchdbResource, resource2json)

-- | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
-- fetchPropDefs :: forall e. Resource -> (AsyncDomeinFile e PropDefs)
fetchPropDefs :: forall e. Resource -> (AsyncDomeinFile e PropDefs)
fetchPropDefs id = do
  r <- fetchCouchdbResource id
  pure $ PropDefs r

-- | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
-- fetchPropDefs :: forall e. Resource -> (AsyncDomeinFile e PropDefs)
fetchCouchdbResource :: forall e. Resource -> (AsyncDomeinFile e CouchdbResource)
fetchCouchdbResource id = if isDomeinURI id
  then case getNamespace id of
    Nothing -> throwError $ error ("Cannot construct namespace out of id " <> id)
    (Just ns) -> retrieveDomeinResourceDefinition id ns
  else if isStandardNamespaceCURIE id
    then case getStandardNamespace id of
      Nothing -> throwError $ error ("Cannot construct standard namespace out of id " <> id)
      (Just ns) -> retrieveDomeinResourceDefinition id ns
    else fetchIndividualCouchDbDefinition id

-- | Fetch the definition of a resource asynchronously.
fetchIndividualCouchDbDefinition :: forall e. Resource -> AsyncResource e CouchdbResource
fetchIndividualCouchDbDefinition id = do
  v <- makeEmptyVar
  -- _ <- forkAff do
  res <- affjax $ userResourceRequest {url = baseURL <> id}
  case res.status of
    StatusCode 200 ->
      case stringToPropDefs res.response of
        (Left message) -> throwError $ error (message <> " (" <> id <> ")")
        Right pd -> putVar pd v
    otherwise -> throwError $ error ("fetchIndividualCouchDbDefinition " <> id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  takeVar v

storeCouchdbResource :: forall e. String -> CouchdbResource -> Aff (ajax :: AJAX | e) (Maybe String)
storeCouchdbResource resId resource =
  let
    revision =  maybe "" id (maybe Nothing toString (lookup "_rev" resource))
  in do
    -- TODO. Misschien een uitgebreidere analyse van de statuscodes? Zie http://127.0.0.1:5984/_utils/docs/api/document/common.html
    res <- put (baseURL <> escapeCouchdbDocumentName resId <> revision) (resource2json resource)
    (StatusCode n) <- pure res.status
    case n == 200 || n == 201 of
      true ->
        -- we **must** use res.response and do this with it, or else get an error...
        case stringToPropDefs res.response of
          (Left message) -> throwError $ error (message <> " (" <> resId <> ")")
          Right pd -> pure $ maybe Nothing toString (lookup "rev" pd)
      false -> throwError $ error ("storeCouchdbResource " <> resId <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")

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
