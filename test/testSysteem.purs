module Test.Systeem where

import Prelude

import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.RunMonadPerspectivesQuery ((##=))
import Perspectives.TripleGetterConstructors (constructRolGetter)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  lift $ log "=========================Test.Systeem================================"
  l0 <-  "model:User$MijnSysteem" ##= constructRolGetter "model:Systeem$Systeem$gebruiker"
  lift $ logShow l0
