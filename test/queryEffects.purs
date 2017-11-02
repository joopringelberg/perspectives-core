module Test.QueryEffects where

import Perspectives.SystemQueries
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryEffect ((~>))
import Perspectives.TheoryChange (saveChangedObject, setObject, setProperty, updateFromSeeds)
import Perspectives.TripleAdministration (NamedFunction(..), lookupInTripleIndex)
import Perspectives.TripleGetter (NamedTripleGetter, (##))
import Prelude (class Show, Unit, bind, discard, flip, void, (>>=))
import Test.TestEffects (CancelerEffects)

test :: forall e. Aff (CancelerEffects e) Unit
test = do
  void ("user:xGebruiker" ## label ~> showOnConsole)
  void ("user:xGebruiker" ## rol_RolBinding >-> label ~> showOnConsole)
  void (liftEff (logShow "======================"))
  t <- setProperty "user:xGebruiker" "rdfs:label" ["Nieuw label voor gebruiker!"]
  t' <- setProperty "user:yNatuurlijkPersoon" "rdfs:label" ["Nieuw label voor NatuurlijkPersoon"]
  x <- setProperty "user:xGebruiker" "model:SysteemDomein#rol_RolBinding" ["model:SysteemDomein#Gebruiker"]
  void (updateFromSeeds [t, t', x])
  void (liftEff (logShow "======================"))
  y <- ("user:xGebruiker" ## rol_RolBinding >-> label) >>= (flip saveChangedObject ["Hello Pirate!"])
  y' <- setObject ("user:xGebruiker" ## rol_RolBinding >-> label) ["Hello Pirate!"]
  void (updateFromSeeds [y])

  void (liftEff (lookupInTripleIndex "user:xGebruiker" "rdfs:label"))

showGebruikerLabel :: forall e. NamedTripleGetter (console :: CONSOLE | e)
showGebruikerLabel = label ~> (NamedFunction "log" logShow)

showOnConsole :: forall a e. Show a => NamedFunction (a -> Eff (console :: CONSOLE | e) Unit)
showOnConsole = NamedFunction "log" logShow
