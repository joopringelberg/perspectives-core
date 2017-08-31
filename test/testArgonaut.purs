module Test.Argonaut where

import Prelude
import Data.Argonaut(Json, toObject, jsonParser, toString)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax)

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

fetchContent :: forall e. Aff (ajax :: AJAX | e) String
fetchContent = do
  resp <- affjax $ userResourceRequest
  pure resp.response

getId :: forall e. Aff (ajax :: AJAX | e) (Either String String)
getId = do
  s <- fetchContent
  case jsonParser s of
    (Left err) -> pure (Left err)
    (Right json) ->
      case toObject json of
        Nothing -> pure (Left "Definition is not an object!")
        (Just obj) -> case lookup "id" obj of
          Nothing -> pure (Left "Definition has no id!")
          (Just id) -> case toString id of
            Nothing -> pure (Left "id is not a String!")
            (Just sid) -> pure (Right sid)

-- Dit is een model voor het ophalen van de PropDefs, als we PropDefs definieren als StrMap Json
getPropDefs' :: forall e. Aff (ajax :: AJAX | e) (Either String JObject)
getPropDefs' = do
  s <- fetchContent
  pure $ parseDefinitionString s

parseDefinitionString :: String -> Either String JObject
parseDefinitionString s = case jsonParser s of
    (Left err) -> Left err
    (Right json) ->
      case toObject json of
        Nothing -> Left "Definition is not an object!"
        (Just obj) -> Right obj


t =
  { "_id":"user:xGebruiker"
  , "_rev":"1-b82564d0cc4f2336a2dbbb13063ce7d2"
  , "id":"user:xGebruiker"
  , "rdf:type":["model:SysteemDomein#Gebruiker"]
  , "rdfs:label":["Ik als PERSPECT Gebruiker"]
  , "model:SysteemDomein#context_Responsible":["user:xGebruiker"]
  , "model:SysteemDomein#gebruiker_ActieLijst":["user:yMijnActieLijst"]
  , "model:SysteemDomein#rol_RolBinding":["user:yNatuurlijkPersoon"]}
