module Test.TheoryChange where


import Prelude
import Perspectives.SystemQueries
import Perspectives.PropertyComposition
import Perspectives.TripleAdministration
import Test.TestEffects
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Perspectives.TheoryChange (propagateTheoryDeltas)
import Perspectives.TripleGetter (applyNamedFunction, constructTripleGetter)


test :: forall e. Aff (CancelerEffects e) Unit
test = do
  log "=========================Test.Properties================================"
  (t@(Triple{object : r0})) <- applyNamedFunction (constructTripleGetter "rdfs:label") "user:xGebruiker"
  log $ show r0

  r1 <- propagateTheoryDeltas [t]
  log $ show r1

  _ <- liftEff $ lookupInTripleIndex "user:xGebruiker" "rdfs:label"

  pure unit
