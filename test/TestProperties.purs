module Test.Properties where

import Prelude
import Perspectives.PropertyComposition
import Perspectives.SystemQueries
import Control.Monad.Aff (launchAff, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console (log) as Eff
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Perspectives.Location (Location, saveInLocation, locationValue)
import Perspectives.LocationT (runLocationT)
import Perspectives.Property (MemorizingSingleGetter, SingleGetter)
import Perspectives.QueryCombinators (filter) as QC
import Perspectives.Resource (representResource, representResource')
import Perspectives.ResourceTypes (Resource)
import Perspectives.TripleAdministration (runTripleGetter)


test = runAff handleError handleSuccess do
  log "=========================Test.Properties================================"
  (gbLoc :: Maybe Resource) <- liftEff $ representResource' "user:xGebruiker"
  (l :: Maybe String) <-  runTripleGetter label gbLoc
  log ( "label user:xGebruiker = " <> (show l))

  log "========================================================="
  (l :: Maybe String) <-  runTripleGetter (rol_RolBinding >-> label) gbLoc
  log ( "(rol_RolBinding >-> label) user:xGebruiker = " <> (show l))
  --
  -- log "========================================================="
  -- h <-  rdfType gbLoc
  -- log ( "rdfType user:xGebruiker = " <> (show h))
  --
  -- log "========================================================="
  -- h' <-  types gbLoc
  -- log ( "types user:xGebruiker = " <> (show h'))
  --
  -- log "========================================================="
  -- ekdLoc <- liftEff $ representResource "model:ExecutieKetenDomein#ExecutieKetenDomein"
  -- m <-  label ekdLoc
  -- log ( "label model:ExecutieKetenDomein#ExecutieKetenDomein = " <> (show m))
  --
  --
  -- log "========================================================="
  -- n <-  subClassOf >>-> label $ ekdLoc
  -- log ( "subClassOf >>-> label $ model:ExecutieKetenDomein#ExecutieKetenDomein = " <> (show n))
  --
  -- log "========================================================="
  -- o <-  rdfType >-> label $ gbLoc
  -- log ( "rdfType >-> label $ user:xGebruiker = " <> (show o))
  --
  -- log "========================================================="
  -- p <-  types >>-> label $ gbLoc
  -- log ( "types >>-> label user:xGebruiker = " <> (show p))
  --
  -- log "========================================================="
  -- p' <-  rol_RolBinding >-> rdfType >->> types >>->> subClassOf >>-> label $ gbLoc
  -- log ( "rol_RolBinding >-> rdfType >->> types >>->> subClassOf >>-> label $ user:xGebruiker = " <> (show p'))
  --
  -- log "========================================================="
  -- q <-  rdfType >->> superClasses $ gbLoc
  -- log ( "rdfType >->> superClasses $ user:xGebruiker = " <> (show q))
  --
  -- log "========================================================="
  -- q' <-  typeSuperClasses >>-> identifier $ gbLoc
  -- log ( "typeSuperClasses >>-> identifier $ user:xGebruiker = " <> (show q'))
  -- --
  -- log "========================================================="
  -- r <-  typeSuperClasses >>-> hasLabel $ gbLoc
  -- log ( "typeSuperClasses >>-> hasLabel $ user:xGebruiker = " <> (show r))
  --
  -- log "========================================================="
  -- -- s <-  (QC.filter hasLabel typeSuperClasses) gbLoc
  -- -- log ( "(QC.filter hasLabel typeSuperClasses) user:xGebruiker = " <> (show s))
  -- s' <-  (QC.filter hasLabel "onlyWithLabel" typeSuperClasses) >>-> identifier $ gbLoc
  -- log ( "(QC.filter hasLabel typeSuperClasses) >>-> identifier $ user:xGebruiker = " <> (show s'))

handleError :: forall e. (Error -> Eff (console :: CONSOLE | e) Unit)
handleError e = Eff.log (show e)

handleSuccess :: forall a e. Show a => (a -> Eff (console :: CONSOLE | e) Unit)
handleSuccess a = Eff.log (show a)
