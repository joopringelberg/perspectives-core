module Perspectives.ResourceRetrieval
(fetchPropDefs
  )
where

import Prelude
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (makeVar, putVar, takeVar)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Network.HTTP.Affjax (AffjaxRequest, affjax)
import Network.HTTP.StatusCode (StatusCode(..))

import Perspectives.Identifiers (getNamespace, getStandardNamespace, isDomeinURI, isStandardNamespaceCURIE)
import Perspectives.DomeinCache (retrieveDomeinResourceDefinition, stringToPropDefs)
import Perspectives.ResourceTypes(ResourceId, AsyncResource, AsyncDomeinFile, PropDefs)

-- | Fetch the definition of the resource asynchronously, either from a Domein file or from the user database.
fetchPropDefs :: forall e. ResourceId -> (AsyncDomeinFile e (Either String PropDefs))
fetchPropDefs id = if isDomeinURI id
  then maybe (pure (Left ("Cannot construct namespace out of id " <> id)))
        (retrieveDomeinResourceDefinition id)
        (getNamespace id)
  else if isStandardNamespaceCURIE id
    then maybe (pure (Left ("Cannot construct standard namespace out of id " <> id)))
          (retrieveDomeinResourceDefinition id)
          (getStandardNamespace id)
    else fetchResourceDefinition id

-- | Fetch the definition of a resource asynchronously.
fetchResourceDefinition :: forall e. ResourceId -> (AsyncResource e (Either String PropDefs))
fetchResourceDefinition id = do
  v <- makeVar
  _ <- forkAff do
        res <- affjax $ userResourceRequest {url = baseURL <> id}
        case res.status of
          StatusCode 200 -> putVar v (f res.response)
          otherwise -> putVar v (Left $ "fetchDefinition " <> id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  takeVar v
  where f ds = let x = stringToPropDefs ds
                in case x of
                    (Left message) -> Left $ message <> " (" <> id <> ")"
                    Right pd -> Right pd

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
