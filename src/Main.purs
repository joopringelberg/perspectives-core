module Main where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Argonaut.Core (foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (getField)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (ResponseType(..))


main :: forall r. Eff (ref :: REF, console :: CONSOLE | r ) Unit
main = do
  log "hello world"
{-
main = launchAff $ do
  res <- affjax $ userResourceRequest
  json <- case jsonParser res.response of
    Left e -> pure e
    Right r -> foldJsonObject jsonEmptyObject r
  liftEff $ log $ "GET /api response: " <> getField json

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

baseURL :: String
baseURL = "http://localhost:5984/user_cor_contexts2/"
-}
