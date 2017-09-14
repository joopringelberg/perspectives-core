module Test.BindLocationT where

import Prelude
import Perspectives.PropertyComposition
import Perspectives.SystemQueries
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log) as Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Perspectives.Location (Location)
import Perspectives.LocationT (runLocationT)
import Perspectives.Property (SingleGetter)
import Perspectives.QueryCombinators (filter, hasValue) as QC
import Perspectives.Resource (representResource)
import Perspectives.ResourceTypes (Resource)

log = lift <<< Aff.log

{-
  Representeer de gebruiker.
  Pas de getter rol_RolBinding toe om een computation in Aff van een Location met een Maybe Resource te krijgen.
  Gebruik dan bind om deze Maybe Resource te verkrijgen en daarop een case analyse uit te voeren.
  Indien de rolbinding van de gebruiker gevonden werd, pas er dan de getter label op toe.
  De bedoeling is dat de locatie met de binding en de locatie met het Maybe String label met elkaar verbonden zijn.
-}

test = launchAff $ runLocationT do
  (gb :: Location (Maybe Resource)) <- liftEff $ representResource "user:xGebruiker"
  log (show gb)
  log (show gb)

-- test = launchAff $ runLocationT do
--   -- log "=========================Test.BindLocationT================================"
--   (gb :: Resource) <- liftEff $ representResource "user:xGebruiker"
--   (r :: (Maybe Resource)) <-  rol_RolBinding gb
--   l <-
--   case r of
--       Nothing -> pure (Just "Geen rol_RolBinding van Gebruiker gevonden.")
--       (Just b) -> label b
--   log ( "label rol_RolBinding user:xGebruiker = " <> (show l))
