module Test.DomeinCache where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.DomeinCache (retrieveContextFromDomein)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.SystemQueries (iedereRolInContext)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  lift $ log "=========================Test.BoundContexts================================"
  l0 <-  "model:Perspectives$Property" ## iedereRolInContext
  lift $ log ( "model:Perspectives$Property ## iedereRolInContext = " <> (show l0))

  lift $ log "=========================Test.BoundContexts================================"
  l1 <-  "model:Perspectives$externalProperty" ## iedereRolInContext
  lift $ log ( "model:Perspectives$externalProperty ## iedereRolInContext = " <> (show l1))



  -- l1 <-  retrieveContextFromDomein "model:Perspectives$internalProperty" "model:Perspectives"
  -- lift $ log ( "retrieveContextFromDomein internalProperty = " <> (show l1))
  --
  -- l2 <-  retrieveContextFromDomein "model:Perspectives$externalProperty" "model:Perspectives"
  -- lift $ log ( "retrieveContextFromDomein externalProperty = " <> (show l2))
  --
  -- l2 <-  retrieveContextFromDomein "model:Perspectives$Property" "model:Perspectives"
  -- lift $ log ( "retrieveContextFromDomein Property = " <> (show l2))
