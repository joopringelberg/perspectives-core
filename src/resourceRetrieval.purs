module Perspectives.ResourceRetrieval
( PropDefs(..)
, ResourceId
, AsyncResource
, fetchPropDefs
  )
where

import Prelude
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (makeVar, putVar, takeVar, AVAR)
import Data.Argonaut (Json, jsonParser, toObject)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, keys)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax)
import Network.HTTP.StatusCode (StatusCode(..))

type ResourceId = String

-- | A newtype for the property definitions so we can show them.
--newtype PropDefs = PropDefs (StrMap (Array Foreign))
newtype PropDefs = PropDefs (StrMap Json)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = show $ keys s

type AsyncResource e a = Aff (avar :: AVAR, ajax :: AJAX | e) a

fetchPropDefs :: forall e. ResourceId -> AsyncResource e (Either String PropDefs)
fetchPropDefs id =
  do
    defstring <- fetchDefinition id
    pure (either Left f defstring)
  where f ds = let x = stringToPropDefs ds
                in case x of
                    (Left message) -> Left $ message <> " (" <> id <> ")"
                    Right pd -> Right pd

-- | Fetch the definition of a resource asynchronously.
-- | TODO: handle situations without response, or rather, when the response indicates an error.
fetchDefinition :: forall e. ResourceId -> (AsyncResource e (Either String String))
fetchDefinition id = do
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
