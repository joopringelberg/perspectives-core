module Perspectives.DomeinCache
-- ( namespaceToDomeinFileName
-- , retrieveDomeinResourceDefinition )

where

import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR, makeVar, putVar, takeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (jsonParser, toArray, toObject)
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax)
import Network.HTTP.StatusCode (StatusCode(..))
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)
import Perspectives.Identifiers (Namespace)
import Perspectives.ResourceTypes (PropDefs(..), ResourceId, AsyncResource)
import Prelude (Unit, bind, pure, show, ($), (*>), (<<<), (<>))

type DomeinFile = StrMap PropDefs
-- | The global index of all cached Domein files, indexed by namespace name.
type DomeinCache = GLStrMap DomeinFile

domeinCache :: forall e. Eff (gm :: GLOBALMAP | e) DomeinCache
domeinCache = new

storeDomeinFileInCache :: forall e. Namespace -> DomeinFile -> Eff (gm :: GLOBALMAP | e) DomeinFile
storeDomeinFileInCache ns df=
  do
    dc <- domeinCache
    poke dc ns df *> pure df

domeinFileRegex :: Regex
domeinFileRegex = unsafeRegex "[:#\\/]" global

namespaceToDomeinFileName :: String -> String
namespaceToDomeinFileName s = replace domeinFileRegex "_" s

-- | Fetch the definition of a resource asynchronously from its Domein.
retrieveDomeinResourceDefinition :: forall e.
  Maybe Namespace
  -> ResourceId
  -> (AsyncDomeinFile e (Either String PropDefs))
retrieveDomeinResourceDefinition Nothing id = pure $ Left ("retrieveDomeinResourceDefinition: cannot find namespace for " <> id)
retrieveDomeinResourceDefinition (Just ns) id = do
  df <- retrieveDomeinFile ns
  case df of
    e@(Left err) -> pure e
    (Right f) -> case lookup id f of
      Nothing -> pure $ Left ("retrieveDomeinResourceDefinition: cannot find definition of " <> id <> " in DomeinFile for " <> ns)

type AsyncDomeinFile e a = Aff (gm :: GLOBALMAP, avar :: AVAR, ajax :: AJAX | e) a

retrieveDomeinFile :: forall e. Namespace -> AsyncDomeinFile e (Either String DomeinFile)
retrieveDomeinFile ns = do
  dc <- liftEff $ domeinCache
  file <- liftEff $ peek dc ns
  case file of
    Nothing -> do
      v <- makeVar
      _ <- forkAff do
            res <- affjax $ domeinRequest {url = modelsURL <> ns}
            case res.status of
              StatusCode 200 -> case stringToDomeinFile res.response of
                e@(Left _) -> putVar v e
                df -> putVar v df
              otherwise -> putVar v (Left $ "retrieveDomeinFile " <> ns <> " fails: " <> (show res.status) <> "(" <> show res.response <> ")")
      takeVar v
    (Just f) -> pure $ Right f

stringToDomeinFile :: String -> Either String DomeinFile
stringToDomeinFile s = case jsonParser s of
  (Left err) -> Left $ "stringToDomeinFile: cannot parse: " <> s
  (Right json) ->
    case toArray json of
      Nothing -> Left $ "stringToDomeinFile: parsed json is not an array!"
      (Just arr) -> case traverse f arr of
        Nothing -> Left "stringToDomeinFile: not all array elements are objects!"
        (Just objArr) -> Right $ objArrayToDomeinFile objArr
        where f js = maybe Nothing (Just <<< PropDefs) (toObject js)

-- TODO Maak tuples van de id van een resource en de resource zelf, converteer dan naar een StrMap met fromFoldable.
-- objArrayToDomeinFile :: Array JObject -> DomeinFile
objArrayToDomeinFile arr = fromFoldable []

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
