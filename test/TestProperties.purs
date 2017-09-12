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
import Perspectives.Property (SingleGetter)
import Perspectives.QueryCombinators (filter, hasValue) as QC
import Perspectives.Resource (representResource)
import Perspectives.ResourceTypes (Resource)

test = launchAff do
  log "=========================Test.Properties================================"
  (gb :: Resource) <- liftEff $ representResource "user:xGebruiker"
  gbLoc <- pure (locate (Just gb))
  (l :: Location (Maybe String)) <-  (query rol_RolBinding >-> label) gbLoc
  log ( "(rol_RolBinding >-> label) user:xGebruiker = " <> (show (locationValue l)))
  (m :: Location (Maybe String)) <-  (query rol_RolBinding >-> label) gbLoc
  log ( "(rol_RolBinding >-> label) user:xGebruiker = " <> (show (locationValue m)))

  -- log "========================================================="
  -- ekd <- liftEff $ representResource "model:ExecutieKetenDomein#ExecutieKetenDomein"
  -- m <-  label ekd
  -- log ( "label model:ExecutieKetenDomein#ExecutieKetenDomein = " <> (show m))
  --
  --
  -- log "========================================================="
  -- n <-  (subClassOf >>-> label) ekd
  -- log ( "(subClassOf >-> label) model:ExecutieKetenDomein#ExecutieKetenDomein = " <> (show n))
  --
  -- log "========================================================="
  -- o <-  (rdfType >-> label) gb
  -- log ( "(rdfType >-> label) user:xGebruiker = " <> (show o))
  --
  -- log "========================================================="
  -- p <-  (types >>-> label) gb
  -- log ( "(types >>-> label user:xGebruiker) = " <> (show p))
  --
  -- log "========================================================="
  -- q <-  (typeSuperClasses >>-> identifier) gb
  -- log ( "(typeSuperClasses >>-> identifier) user:xGebruiker = " <> (show q))
  --
  -- log "========================================================="
  -- r <-  (typeSuperClasses >>-> hasLabel) gb
  -- log ( "(typeSuperClasses >>-> hasLabel) user:xGebruiker = " <> (show r))
  --
  -- log "========================================================="
  -- s <-  (QC.filter hasLabel typeSuperClasses  >>-> identifier) gb
  -- log ( "(filter hasLabel typeSuperClasses) user:xGebruiker = " <> (show s))

hasLabel :: SingleGetter Boolean
hasLabel = QC.hasValue label

log = Aff.log
