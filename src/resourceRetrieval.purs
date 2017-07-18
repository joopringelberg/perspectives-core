module Perspectives.ResourceRetrieval
( PropDefs
, ResourceId
, fetchDefinition
, stringToPropDefs
  )
where

import Prelude
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (makeVar, putVar, takeVar, AVAR)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, keys)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax)

type ResourceId = String

-- | A newtype for the property definitions so we can show them.
--newtype PropDefs = PropDefs (StrMap (Array Foreign))
newtype PropDefs = PropDefs (StrMap Foreign)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = show $ keys s

-- | Fetch the definition of a resource asynchronously.
fetchDefinition :: forall e. ResourceId -> Aff (avar :: AVAR, ajax :: AJAX | e) String
fetchDefinition id = do
  v <- makeVar
  _ <- forkAff do
        res <- affjax $ userResourceRequest {url = baseURL <> id}
        putVar v res.response
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

stringToPropDefs :: String -> PropDefs
stringToPropDefs s = PropDefs empty
