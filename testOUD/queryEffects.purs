module Test.QueryEffects where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Perspectives.Actions (setContextDisplayName)
import Perspectives.CoreTypes (MonadPerspectives, NamedFunction(..), TypedTripleGetter)
import Perspectives.DataTypeTripleGetters (labelM)
import Perspectives.Deltas (runTransactie)
import Perspectives.Effects (TransactieEffects)
import Perspectives.QueryEffect ((~>))
import Perspectives.RunMonadPerspectivesQuery ((##))
import Prelude (class Show, Unit, discard, void, (<<<), bind)

rolDef :: String
rolDef = "model:Perspectives$Rol"

test :: forall e. MonadPerspectives (TransactieEffects (console :: CONSOLE | e)) Unit
test = do
  void (rolDef ## (labelM ~> showOnConsole))
  void (liftEff (logShow "======================"))
  _ <- setContextDisplayName "Rol" rolDef
  runTransactie
  void (liftEff (logShow "======================"))

  -- void (liftEff (lookupInTripleIndex "user:xGebruiker" "rdfs:labelM"))

showGebruikerLabel :: forall e. TypedTripleGetter (console :: CONSOLE | e)
showGebruikerLabel = labelM ~> (NamedFunction "log" (liftEff <<< logShow))

showOnConsole :: forall a e. Show a => NamedFunction (a -> Eff (console :: CONSOLE | e) Unit)
showOnConsole = NamedFunction "log" logShow
