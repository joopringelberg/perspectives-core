module Test.DomeinCache where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.DataTypeTripleGetters (iedereRolInContextM)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE | e)) Unit
test = do
  lift $ log "=========================Test.BoundContexts================================"
  l0 <-  "model:Perspectives$Property" ## iedereRolInContextM
  lift $ log ( "model:Perspectives$Property ## iedereRolInContextM = " <> (show l0))

  lift $ log "=========================Test.BoundContexts================================"
  l1 <-  "model:Perspectives$externalProperty" ## iedereRolInContextM
  lift $ log ( "model:Perspectives$externalProperty ## iedereRolInContextM = " <> (show l1))



  -- l1 <-  retrieveContextFromDomein "model:Perspectives$internalProperty" "model:Perspectives"
  -- lift $ log ( "retrieveContextFromDomein internalProperty = " <> (show l1))
  --
  -- l2 <-  retrieveContextFromDomein "model:Perspectives$externalProperty" "model:Perspectives"
  -- lift $ log ( "retrieveContextFromDomein externalProperty = " <> (show l2))
  --
  -- l2 <-  retrieveContextFromDomein "model:Perspectives$Property" "model:Perspectives"
  -- lift $ log ( "retrieveContextFromDomein Property = " <> (show l2))
