module Test.QueryEffects where

import Perspectives.SystemQueries
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryEffect ((~>))
import Perspectives.TheoryChange (setProperty, updateFromSeeds)
import Perspectives.TripleAdministration (lookupInTripleIndex)
import Perspectives.TripleGetter (NamedFunction(..), NamedTripleGetter, (##))
import Prelude (class Show, Unit, discard, void, bind)
import Test.TestEffects (CancelerEffects)

test :: forall e. Aff (CancelerEffects e) Unit
test = do
  void ("user:xGebruiker" ## label ~> showOnConsole)
  void ("user:xGebruiker" ## rol_RolBinding >-> label ~> showOnConsole)
  void ("user:xGebruiker" ## rol_RolBinding ~> showOnConsole)
  void ("user:xGebruiker" ## rol_RolBinding >-> rdfType >-> label ~> showOnConsole)
  t <- setProperty "user:xGebruiker" "rdfs:label" ["Nieuw label voor gebruiker!"]
  t' <- setProperty "user:yNatuurlijkPersoon" "rdfs:label" ["Nieuw label voor NatuurlijkPersoon"]
  void (updateFromSeeds [t, t'])
  void (liftEff (lookupInTripleIndex "user:xGebruiker" "rdfs:label"))

showGebruikerLabel :: forall e. NamedTripleGetter (console :: CONSOLE | e)
showGebruikerLabel = label ~> (NamedFunction "log" logShow)

showOnConsole :: forall a e. Show a => NamedFunction (a -> Eff (console :: CONSOLE | e) Unit)
showOnConsole = NamedFunction "log" logShow
