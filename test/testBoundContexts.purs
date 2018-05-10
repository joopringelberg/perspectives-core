module Test.BoundContexts where

import Control.Monad.Aff.Console (log) as AC
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (filter)
import Perspectives.SystemQueries (binding, boundContexts, hasBinding, iedereRolInContext)
import Prelude (Unit, bind, discard, show, (<<<), (<>))

rolDef :: String
-- rolDef = "model:Perspectives$externalProperty"
rolDef = "model:Perspectives$Property"

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  log "=========================Test.BoundContexts================================"
  l0 <-  rolDef ## iedereRolInContext
  log ( "rolDef ## iedereRolInContext = " <> (show l0))

  l1 <-  rolDef ## iedereRolInContext >-> binding
  log ( "rolDef ## iedereRolInContext >-> binding = " <> (show l1))
  --
  l2 <- "model:Perspectives$externalProperty" ## iedereRolInContext
  log ("model:Perspectives$externalProperty ## iedereRolInContext) = " <> (show l2))
  --
  l3 <-  rolDef ## filter hasBinding iedereRolInContext
  log ( "rolDef ## filter iedereRolInContext hasBinding = " <> (show l3))

  l4 <- rolDef ## boundContexts
  log ( "rolDef ## boundContexts = " <> (show l4))

log :: forall e. String -> MonadPerspectives (console :: CONSOLE | e) Unit
log = lift <<< AC.log
