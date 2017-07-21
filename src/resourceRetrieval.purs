module Perspectives.ResourceRetrieval
(fetchPropDefs
  )
where

import Prelude
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (makeVar, putVar, takeVar)
import Data.Argonaut (jsonParser, toObject)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AffjaxRequest, affjax)
import Network.HTTP.StatusCode (StatusCode(..))

import Perspectives.Identifiers (getNamespace, getStandardNamespace, isDomeinURI, isStandardNamespaceCURIE)
import Perspectives.DomeinCache (retrieveDomeinResourceDefinition)
import Perspectives.ResourceTypes(ResourceId, AsyncResource, PropDefs(..))

fetchPropDefs :: forall e. ResourceId -> AsyncResource e (Either String PropDefs)
fetchPropDefs id =
  do
    defstring <- fetchDefinition id
    pure (either Left f defstring)
  where f ds = let x = stringToPropDefs ds
                in case x of
                    (Left message) -> Left $ message <> " (" <> id <> ")"
                    Right pd -> Right pd

-- | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
fetchDefinition :: forall e. ResourceId -> (AsyncResource e (Either String String))
fetchDefinition id = if isDomeinURI id
  then retrieveDomeinResourceDefinition (getNamespace id) id
  else if isStandardNamespaceCURIE id
    then retrieveDomeinResourceDefinition (getStandardNamespace id) id
    else fetchResourceDefinition id

-- | Fetch the definition of a resource asynchronously.
fetchResourceDefinition :: forall e. ResourceId -> (AsyncResource e (Either String String))
fetchResourceDefinition id = do
  v <- makeVar
  _ <- forkAff do
        res <- affjax $ userResourceRequest {url = baseURL <> id}
        case res.status of
          StatusCode 200 -> putVar v (Right res.response)
          otherwise -> putVar v (Left $ "fetchDefinition " <> id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  takeVar v

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

-- | There can be two error scenarios here: either the returned string cannot be parsed
-- | by JSON.parse, or the resulting json is not an object. Neither is likely, because Couchdb
-- | will not store such documents.
stringToPropDefs :: String -> Either String PropDefs
stringToPropDefs s = case jsonParser s of
    (Left err) -> Left $ "stringToPropDefs: cannot parse: " <> s
    (Right json) ->
      case toObject json of
        Nothing -> Left $ "stringToPropDefs: parsed json is not an object!"
        (Just obj) -> Right $ PropDefs obj
