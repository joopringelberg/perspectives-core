module Test.Properties where

import Prelude
import Perspectives.PropertyComposition
import Perspectives.SystemQueries
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Perspectives.QueryCombinators (filter) as QC
import Perspectives.TripleAdministration ((##))
import Test.TestEffects (CancelerEffects)

gebruiker :: String
gebruiker = "user:xGebruiker"

test :: forall e. Aff (CancelerEffects e) Unit
test = do
  log "=========================Test.Properties================================"
  l <-  gebruiker ## label
  log ( "gebruiker ## label = " <> (show l))

  log "========================================================="
  l1 <-  gebruiker ## (rol_RolBinding >-> label)
  log ( "gebruiker ## (rol_RolBinding >-> label) = " <> (show l1))

  log "========================================================="
  h <-  gebruiker ## types
  log ( "gebruiker ## types = " <> (show h))

  log "========================================================="
  m <-  "model:ExecutieKetenDomein#ExecutieKetenDomein" ## label
  log ( "\"model:ExecutieKetenDomein#ExecutieKetenDomein\" ## label = " <> (show m))

  log "========================================================="
  n <-  "model:ExecutieKetenDomein#ExecutieKetenDomein" ## subClassOf >-> label
  log ( "\"model:ExecutieKetenDomein#ExecutieKetenDomein\" ## subClassOf >-> label = " <> (show n))

  log "========================================================="
  o <-  gebruiker ## rdfType >-> label
  log ( "gebruiker ## rdfType >-> label = " <> (show o))

  log "========================================================="
  p <-  gebruiker ## types >-> label
  log ( "gebruiker ## types >-> label = " <> (show p))

  log "========================================================="
  p' <-  gebruiker ## rol_RolBinding >-> rdfType >-> types >-> subClassOf >-> label
  log ( "gebruiker ## rol_RolBinding >-> rdfType >-> types >-> subClassOf >-> label = " <> (show p'))

  log "========================================================="
  q <-  gebruiker ## rdfType >-> superClasses
  log ( "gebruiker ## rdfType >-> superClasses = " <> (show q))

  log "========================================================="
  q' <- gebruiker ## typeSuperClasses >-> identity
  log ( "gebruiker ## typeSuperClasses >-> identity = " <> (show q'))

  log "========================================================="
  r <-  gebruiker ## typeSuperClasses >-> hasLabel
  log ( "gebruiker ## typeSuperClasses >-> hasLabel = " <> (show r))

  log "========================================================="
  s <-  gebruiker ## (QC.filter hasLabel typeSuperClasses)
  log ( "gebruiker ## (QC.filter hasLabel typeSuperClasses) = " <> (show s))
