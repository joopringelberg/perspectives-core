module Main where


import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console
import Perspectives.SystemQueries
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Console (log)
import Data.Argonaut.Core (foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode (getField)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Response (ResponseType(..))
import Perspectives.Location (Location, locate, locationValue)
import Perspectives.LocationT (LocationT(..))
import Perspectives.Property (StackedLocation)
import Perspectives.PropertyComposition (lowerFromLocationT, (>->))
import Perspectives.Resource (representResource)
import Perspectives.ResourceTypes (Resource(..))

import Test.Properties

main = test

-- test = launchAff do
--   log "=========================Test.Properties================================"
--   (gbLoc :: Location (Maybe Resource)) <- (liftEff $ representResource "user:xGebruiker")
--   (l :: Location (Maybe String)) <- (rol_RolBinding >-> label) gbLoc
--   log ( "(rol_RolBinding >-> label) user:xGebruiker = " <> (show l))
--
--   log "========================================================="
--   (h :: Location (Maybe Resource)) <-  rdfType gbLoc
--   log ( "(rdfType) user:xGebruiker = " <> (show h))
--
--   log "========================================================="
--   h' <-  types gbLoc
--   log ( "(types user:xGebruiker = " <> (show h'))
