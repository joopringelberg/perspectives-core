module Perspectives.ResourceRetrieval
( PropDefs(..)
, ResourceId
, AsyncResource
, fetchDefinition
, stringToPropDefs
  )
where

import Prelude
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (makeVar, putVar, takeVar, AVAR)
import Data.Argonaut (Json, fromString, jsonParser, toObject)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, keys, singleton)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax)

type ResourceId = String

-- | A newtype for the property definitions so we can show them.
--newtype PropDefs = PropDefs (StrMap (Array Foreign))
newtype PropDefs = PropDefs (StrMap Json)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = show $ keys s

type AsyncResource e a = Aff (avar :: AVAR, ajax :: AJAX | e) a

-- | Fetch the definition of a resource asynchronously.
-- | TODO: handle situations without response, or rather, when the response indicates an error.
fetchDefinition :: forall e. ResourceId -> (AsyncResource e String)
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

-- | There can be two error scenarios here: either the returned string cannot be parsed
-- | by JSON.parse, or the resulting json is not an object. Neither is likely.
-- | What to do in such cases? We could return an empty PropDefs and fail silently.
-- | The causes of these problems are of no interest to the end user.
-- | TODO: construct a PropDefs representing a Resource that failed to load. In this way we
-- | lift the problem into the Domain that is modelled, leaving it to the application to handle
-- | this case (e.g. by representing such Resources in a distinghuished way to the user).
stringToPropDefs :: String -> PropDefs
stringToPropDefs s = case jsonParser s of
    (Left err) -> PropDefs (singleton "error" (fromString err))
    (Right json) ->
      case toObject json of
        Nothing -> PropDefs (singleton "error" (fromString "Definition is not an object!"))
        (Just obj) -> PropDefs obj
