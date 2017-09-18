module Test.Properties where

import Prelude
import Perspectives.PropertyComposition
import Perspectives.SystemQueries
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log) as Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Perspectives.Location (Location, locate, locationValue)
import Perspectives.LocationT (runLocationT)
import Perspectives.Property (SingleGetter, MemoizingSingleGetter)
import Perspectives.QueryCombinators (filter, hasValue) as QC
import Perspectives.Resource (representResource)
import Perspectives.ResourceTypes (Resource)

test = launchAff do
  log "=========================Test.Properties================================"
  (gbLoc :: Location (Maybe Resource)) <- liftEff $ representResource "user:xGebruiker"
  (l :: Location (Maybe String)) <-  (rol_RolBinding >-> label) gbLoc
  log ( "(rol_RolBinding >-> label) user:xGebruiker = " <> (show (locationValue l)))

  log "========================================================="
  h <-  (rdfType) gbLoc
  log ( "(rdfType) user:xGebruiker = " <> (show h))

  log "========================================================="
  h' <-  types gbLoc
  log ( "(types user:xGebruiker = " <> (show h'))

  log "========================================================="
  ekdLoc <- liftEff $ representResource "model:ExecutieKetenDomein#ExecutieKetenDomein"
  m <-  (label) ekdLoc
  log ( "label model:ExecutieKetenDomein#ExecutieKetenDomein = " <> (show m))


  log "========================================================="
  n <-  (subClassOf >>-> label) ekdLoc
  log ( "(subClassOf >>-> label) model:ExecutieKetenDomein#ExecutieKetenDomein = " <> (show n))

  log "========================================================="
  o <-  (rdfType >-> label) gbLoc
  log ( "(rdfType >-> label) user:xGebruiker = " <> (show o))

  log "========================================================="
  p <-  (types >>-> label) gbLoc
  log ( "(types >>-> label user:xGebruiker) = " <> (show p))

  log "========================================================="
  p' <-  (rol_RolBinding >-> rdfType >->> types >>->> subClassOf >>-> label) gbLoc
  log ( "(( rol_RolBinding >-> rdfType >->> types >>->> subClassOf >>-> label) user:xGebruiker) = " <> (show p'))

  log "========================================================="
  q <-  (rdfType >->> superClasses) gbLoc
  log ( "(rdfType >->> superClasses) user:xGebruiker = " <> (show q))

  -- log "========================================================="
  -- q <-  (typeSuperClasses >>-> identifier) gbLoc
  -- log ( "(typeSuperClasses >>-> identifier) user:xGebruiker = " <> (show q))

  -- log "========================================================="
  -- r <-  (typeSuperClasses >>-> hasLabel) gbLoc
  -- log ( "(typeSuperClasses >>-> hasLabel) user:xGebruiker = " <> (show r))
  --
  -- log "========================================================="
  -- s <-  (QC.filter hasLabel typeSuperClasses  >>-> identifier) gbLoc
  -- log ( "(filter hasLabel typeSuperClasses) user:xGebruiker = " <> (show s))

log = Aff.log
