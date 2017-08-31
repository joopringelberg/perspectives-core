module Test.FetchPropDefs where

import Prelude
import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Network.HTTP.Affjax (affjax)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.DomeinCache (domeinRequest, modelsURL, retrieveDomeinFile, stringToDomeinFile)
import Perspectives.Identifiers (getNamespace, getStandardNamespace, isDomeinURI, isStandardNamespaceCURIE)
import Perspectives.ResourceRetrieval (fetchPropDefs)
import Perspectives.ResourceTypes (PropDefs(..))

test = launchAff do
  df <- attempt $ retrieveDomeinFile "model_ICTDomein_"
  case df of
    (Right f) -> log $ show f
    (Left err) -> throwError err

-- test = launchAff do
--   res <- attempt $ affjax $ (domeinRequest {url = modelsURL <> "model_ICTDomein_"} )
--   case res of
--     (Right response) ->
--       case response.status of
--         StatusCode 200 -> case stringToDomeinFile response.response of
--           (Left err) -> throwError $ error err
--           (Right df) -> log $ show df
--         otherwise -> throwError $ error "Iets fout"
--     (Left err) -> throwError err
