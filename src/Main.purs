module Main where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Writer (runWriterT)
import Data.Argonaut.Core (foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (getField)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (many)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (ResponseType(..))
import Perspectives.PropertyComposition ((>->))
import Perspectives.Resource (resourceDefinitions)
import Perspectives.ResourceTypes (Resource(..))
import Perspectives.TripleAdministration (Triple(..), applyNamedFunction, constructTripleGetter, lookup, tripleIndex)

-- import Test.Properties

main = test


test = launchAff do
  -- log "=========================Test.Properties================================"
  -- ((Triple{object : gb}) :: Triple) <- applyNamedFunction (constructTripleGetter "rdfs:label") "user:xGebruiker"
  -- log $ show gb
  --
  -- log "=========================================================================="
  -- ((Triple{object : tp}) :: Triple) <- applyNamedFunction (constructTripleGetter "rdf:type") "user:xGebruiker"
  -- log $ show tp

  ((Triple{object}) :: Triple) <- applyNamedFunction ((constructTripleGetter "rdf:type") >-> (constructTripleGetter "rdfs:subClassOf")) "user:xGebruiker"

  ((Triple{object: obj2}) :: Triple) <- applyNamedFunction ((constructTripleGetter "rdf:type") >-> (constructTripleGetter "rdfs:subClassOf") >-> (constructTripleGetter "rdfs:label")) "user:xGebruiker"

  _ <- liftEff $ lookup tripleIndex "user:xGebruiker" "rdfs:label"
  log $ show object
