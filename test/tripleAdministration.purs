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
import Perspectives.TripleGetter (applyNamedFunction, constructTripleGetter, (##))


test :: forall eff. Aff (CancelerEffects eff) Unit
test = do
  log "=========================Test.Properties================================"
  ((Triple{object : r0})) <- applyNamedFunction (constructTripleGetter "rdfs:label") "user:xGebruiker"
  log $ show r0

  log "=========================================================================="
  ((Triple{object: r1})) <- applyNamedFunction (rdfType >-> subClassOf) "user:xGebruiker"
  log $ show r1

  log "=========================================================================="
  ((Triple{object: r2})) <-  "user:xGebruiker" ## rdfType >-> subClassOf >-> label
  log $ show r2

  log "=========================================================================="
  ((Triple{object: r3})) <-  "user:xGebruiker" ## rdfType >-> closure subClassOf
  log $ show r3

  log "=========================================================================="
  ((Triple{object: r4})) <-  "user:xGebruiker" ## closure rdfType
  log $ show r4

  log "=========================================================================="
  ((Triple{object: r5})) <-  "user:xGebruiker" ## hasValue rdfType
  log $ show r5

  log "=========================================================================="
  ((Triple{object: r6})) <-  "user:xGebruiker" ## filter (hasValue label) (rdfType >-> closure subClassOf)
  log $ show r6

  _ <- liftEff $ lookupInTripleIndex "user:xGebruiker" "rdfs:label"

  pure unit
