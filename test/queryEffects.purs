module Test.QueryEffects where

import Perspectives.SystemQueries

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Perspectives.CoreTypes (MonadPerspectives, NamedFunction(..), TypedTripleGetter, (##))
import Perspectives.Deltas (runInTransactie, setContextDisplayName)
import Perspectives.Effects (TransactieEffects)
import Perspectives.QueryEffect ((~>))
import Prelude (class Show, Unit, discard, void, ($))

rolDef :: String
rolDef = "model:Perspectives$Rol"

test :: forall e. MonadPerspectives (TransactieEffects (console :: CONSOLE | e)) Unit
test = do
  void (rolDef ## (label ~> showOnConsole))
  void (liftEff (logShow "======================"))
  runInTransactie $ setContextDisplayName rolDef "Rol"
  void (liftEff (logShow "======================"))

  -- void (liftEff (lookupInTripleIndex "user:xGebruiker" "rdfs:label"))

showGebruikerLabel :: forall e. TypedTripleGetter (console :: CONSOLE | e)
showGebruikerLabel = label ~> (NamedFunction "log" logShow)

showOnConsole :: forall a e. Show a => NamedFunction (a -> Eff (console :: CONSOLE | e) Unit)
showOnConsole = NamedFunction "log" logShow
