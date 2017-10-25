module Test.TripleAdministration where


import Prelude
import Perspectives.SystemQueries
import Perspectives.PropertyComposition
import Perspectives.TripleAdministration
import Test.TestEffects
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)


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

  _ <- liftEff $ lookup tripleIndex "user:xGebruiker" "rdfs:label"

  pure unit
