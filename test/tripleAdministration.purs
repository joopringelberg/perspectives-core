module Test.TripleAdministration where


import Prelude
import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Perspectives.SystemQueries
import Perspectives.PropertyComposition
import Perspectives.TripleAdministration
import Test.TestEffects


test :: forall e. Eff (TestEffects e) (Canceler (CancelerEffects e))
test = launchAff do
  log "=========================Test.Properties================================"
  ((Triple{object : r0}) :: Triple) <- applyNamedFunction (constructTripleGetter "rdfs:label") "user:xGebruiker"
  log $ show r0

  log "=========================================================================="
  ((Triple{object: r1}) :: Triple) <- applyNamedFunction (rdfType >-> subClassOf) "user:xGebruiker"
  log $ show r1

  log "=========================================================================="
  ((Triple{object: r2}) :: Triple) <-  "user:xGebruiker" ## rdfType >-> subClassOf >-> label
  log $ show r2

  liftEff $ lookup tripleIndex "user:xGebruiker" "rdfs:label"
