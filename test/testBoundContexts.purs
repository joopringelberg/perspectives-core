module Test.BoundContexts where

import Control.Monad.Aff.Console (log) as AC
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Perspectives.CoreTypes (MonadPerspectives, (##))
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (ignoreCache, filter)
import Perspectives.SystemQueries (binding, boundContexts, buitenRol, contextType, hasBinding, hasLabel, identity, iedereRolInContext, isVerplicht, label, range, rolContext, rolHasType, rolType, rolTypen)
import Perspectives.TripleGetter (constructRolGetter)
import Prelude (Unit, bind, discard, show, (<<<), (<>))
import Test.TestEffects (CancelerEffects)

rolDef :: String
rolDef = "model:Perspectives$Rol"

test :: forall e. MonadPerspectives (CancelerEffects e) Unit
test = do
  log "=========================Test.BoundContexts================================"
  l1 <-  rolDef ## iedereRolInContext >-> binding
  log ( "rolDef ## iedereRolInContext >-> binding = " <> (show l1))

  l2 <- "model:Perspectives$mogelijkeBinding_buitenRol" ## (rolHasType "model:Perspectives$BuitenRol")
  log ("model:Perspectives$mogelijkeBinding_buitenRol ## (rolHasType model:Perspectives$BuitenRol) = " <> (show l2))

  l3 <-  rolDef ## filter hasBinding iedereRolInContext
  log ( "rolDef ## filter iedereRolInContext hasBinding = " <> (show l3))

log :: forall e. String -> MonadPerspectives (console :: CONSOLE | e) Unit
log = lift <<< AC.log
