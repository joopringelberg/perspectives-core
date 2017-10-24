module Test.LocationT where

import Prelude
import Perspectives.LocationT
import Control.Monad.Aff (Aff, Canceler(..), launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax)
import Perspectives.DomeinCache (retrieveDomeinResourceDefinition)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Location (Location, locationValue)
import Perspectives.ResourceRetrieval (fetchPropDefs)
import Perspectives.ResourceTypes (AsyncDomeinFile, PropDefs(..))

test :: forall t106.
  Eff
    ( exception :: EXCEPTION
    , console :: CONSOLE
    , ajax :: AJAX
    , avar :: AVAR
    , gm :: GLOBALMAP
    | t106
    )
    (Canceler
       ( console :: CONSOLE
       , ajax :: AJAX
       , avar :: AVAR
       , gm :: GLOBALMAP
       | t106
       )
    )
test = launchAff do
  -- sInA <- stringInAff
  -- log sInA
  -- locString <- stringInLocation "String in Location in Aff"
  -- log $ locationValue locString

  gebruiker <- getGebruiker
  log gebruiker
  -- locGebruiker <- runLocationT getGebruikerInLocation
  -- log $ locationValue locGebruiker
  --
  -- locPropDefs <- getPropDefsInLocation1
  -- log $ locPropDefs

f :: forall e. String -> LocationT (Aff e) String
f s = lift $ pure s

stringInLocation :: forall e. String -> Aff e (Location String)
stringInLocation s = runLocationT $ f s

stringInAff :: forall e. Aff e String
stringInAff = pure "String in Aff"

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

getGebruiker :: forall e. Aff (ajax :: AJAX | e) String
getGebruiker = do
  res <- affjax userResourceRequest
  pure res.response

getGebruikerInLocation :: forall e. LocationT (Aff (ajax :: AJAX|e)) String
getGebruikerInLocation = do
  res <- lift $ affjax userResourceRequest
  pure res.response

getPropDefsInLocation :: forall e. (AsyncDomeinFile e PropDefs)
getPropDefsInLocation = retrieveDomeinResourceDefinition "model:ExecutieKetenDomein#" "model:ExecutieKetenDomein#"

getPropDefsInLocation1 :: forall e. (AsyncDomeinFile e String)
getPropDefsInLocation1 = do
  pd <- fetchPropDefs "user:xGebruiker"
  _ <- pure (show pd)
  pure "Dit zouden propdefs moeten zijn"
