module Test.BoundContexts where

import Control.Monad.Aff.Console (log) as AC
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.QueryCombinators (filter)
import Perspectives.DataTypeTripleGetters (bindingM, iedereRolInContextM)
import Perspectives.ModelBasedTripleGetters (boundContextsM, hasBindingM)
import Prelude (Unit, bind, discard, show, (<<<), (<>))

rolDef :: String
-- rolDef = "model:Perspectives$externalProperty"
rolDef = "model:Perspectives$Property"

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  log "=========================Test.BoundContexts================================"
  l0 <-  rolDef ## iedereRolInContextM
  log ( "rolDef ## iedereRolInContextM = " <> (show l0))

  l1 <-  rolDef ## iedereRolInContextM >-> bindingM
  log ( "rolDef ## iedereRolInContextM >-> bindingM = " <> (show l1))
  --
  l2 <- "model:Perspectives$externalProperty" ## iedereRolInContextM
  log ("model:Perspectives$externalProperty ## iedereRolInContextM) = " <> (show l2))
  --
  l3 <-  rolDef ## filter hasBindingM iedereRolInContextM
  log ( "rolDef ## filter iedereRolInContextM hasBindingM = " <> (show l3))

  l4 <- rolDef ## boundContextsM
  log ( "rolDef ## boundContextsM = " <> (show l4))

log :: forall e. String -> MonadPerspectives (console :: CONSOLE | e) Unit
log = lift <<< AC.log
