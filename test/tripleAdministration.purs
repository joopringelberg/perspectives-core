module Test.TripleAdministration where


import Prelude
import Perspectives.SystemQueries
import Perspectives.PropertyComposition
import Perspectives.TripleAdministration
import Test.TestEffects
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Perspectives.QueryCombinators (closure, filter, hasValue)


test :: forall e. Aff (CancelerEffects e) Unit
test = do
  log "=========================Test.Properties================================"
  ((Triple{object : r0}) :: Triple) <- applyNamedFunction (constructTripleGetter "rdfs:label") "user:xGebruiker"
  log $ show r0

  log "=========================================================================="
  ((Triple{object: r1}) :: Triple) <- applyNamedFunction (rdfType >-> subClassOf) "user:xGebruiker"
  log $ show r1

  log "=========================================================================="
  ((Triple{object: r2}) :: Triple) <-  "user:xGebruiker" ## rdfType >-> subClassOf >-> label
  log $ show r2

  log "=========================================================================="
  ((Triple{object: r3}) :: Triple) <-  "user:xGebruiker" ## rdfType >-> closure subClassOf
  log $ show r3

  log "=========================================================================="
  ((Triple{object: r4}) :: Triple) <-  "user:xGebruiker" ## closure rdfType
  log $ show r4

  log "=========================================================================="
  ((Triple{object: r5}) :: Triple) <-  "user:xGebruiker" ## hasValue rdfType
  log $ show r5

  log "=========================================================================="
  ((Triple{object: r5}) :: Triple) <-  "user:xGebruiker" ## filter (hasValue label) (rdfType >-> closure subClassOf)
  log $ show r5

  _ <- liftEff $ lookupTriple "user:xGebruiker" "rdfs:label"

  pure unit
