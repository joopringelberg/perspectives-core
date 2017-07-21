module Perspectives.DomeinCache
-- ( namespaceToDomeinFileName
-- , retrieveDomeinResourceDefinition )

where

import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (makeVar, putVar, takeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, lookup)
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Network.HTTP.Affjax (AffjaxRequest, affjax)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.Identifiers (Namespace)
import Perspectives.ResourceTypes (PropDefs, ResourceId, AsyncResource)
import Prelude (Unit, bind, pure, show, ($), (<>))

type DomeinFile = Array PropDefs
-- | The global index of all cached Domein files, indexed by namespace name.
type DomeinCache = StrMap DomeinFile

domeinCache :: DomeinCache
domeinCache = empty

-- storeDomeinFileInCache :: forall e. Namespace -> DomeinFile -> Eff (st :: ST DomeinCache | e) DomeinFile
-- storeDomeinFileInCache ns df=
--   do
--     dc <- thawST' domeinCache
--     poke dc ns df *> pure df

domeinFileRegex :: Regex
domeinFileRegex = unsafeRegex "[:#\\/]" global

namespaceToDomeinFileName :: String -> String
namespaceToDomeinFileName s = replace domeinFileRegex "_" s

-- | Fetch the definition of a resource asynchronously.
-- | TODO dit is de implementatie van fetchResourceDefinition, pas aan voor Domeinen!!
retrieveDomeinResourceDefinition :: forall e.
  Maybe Namespace
  -> ResourceId
  -> (AsyncResource e (Either String String))
retrieveDomeinResourceDefinition Nothing id = pure $ Left ("retrieveDomeinResourceDefinition: cannot find namespace for " <> id)
retrieveDomeinResourceDefinition (Just ns) id = do
  v <- makeVar
  _ <- forkAff do
        res <- affjax $ domeinRequest {url = modelsURL <> ns}
        case res.status of
          StatusCode 200 -> putVar v (Right res.response)
          otherwise -> putVar v (Left $ "fetchDefinition " <> id <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
  takeVar v

modelsURL :: String
modelsURL = "http://localhost:5984/models2/"

domeinRequest :: AffjaxRequest Unit
domeinRequest =
  { method: Left GET
  , url: "http://localhost:5984/models2model_SysteemDomein_"
  , headers: []
  , content: Nothing
  , username: Just "cor"
  , password: Just "geheim"
  , withCredentials: true
  }
