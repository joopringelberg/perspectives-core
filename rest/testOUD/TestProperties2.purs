module Test.Properties where

import Control.Monad.Aff.Console (log) as AC
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.RunMonadPerspectivesQuery ((##))
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.QueryCombinators (ignoreCache)
import Perspectives.DataTypeTripleGetters (bindingM, buitenRolM, contextTypeM, labelM, contextM, iedereRolInContextM, rolTypeM, typeVanIedereRolInContextM, identityM)
import Perspectives.ModelBasedTripleGetters (hasBindingM, hasLabelM, rolIsVerplichtM, rangeDefM)
import Perspectives.TripleGetterConstructors (constructRolGetter)
import Prelude (Unit, bind, discard, show, (<<<), (<>))
import Test.TestEffects (CancelerEffects)

rolDef :: String
rolDef = "model:Perspectives$Rol"

rolIsVerplichtDef :: String
rolIsVerplichtDef = "model:Perspectives$Rol$isVerplicht"

viewDef :: String
viewDef = "model:Perspectives$view"

psp :: String
psp = "model:Perspectives"

test :: forall e. MonadPerspectives (CancelerEffects e) Unit
test = do
  log "=========================Test.Properties================================"
  l1 <-  rolDef ## iedereRolInContextM
  log ( "rolDef ## iedereRolInContextM = " <> (show l1))

  l2 <-  rolDef ## iedereRolInContextM >-> bindingM
  log ( " rolDef ## iedereRolInContextM >-> bindingM = " <> (show l2))

  l3 <-  rolDef ## iedereRolInContextM >-> bindingM >-> contextM
  log ( " rolDef ## iedereRolInContextM >-> bindingM >-> contextM = " <> (show l3))

  l4 <-  rolDef ## iedereRolInContextM >-> identityM
  log ( " rolDef ## iedereRolInContextM >-> identity = " <> (show l4))

  l5 <-  rolDef ## iedereRolInContextM >-> bindingM >-> contextM >-> contextTypeM
  log ( " rolDef ## iedereRolInContextM >-> identity >-> contextTypeM = " <> (show l5))

  l6 <-  rolDef ## buitenRolM
  log ( " rolDef ## buitenRolM = " <> (show l6))

  l7 <-  rolDef ## typeVanIedereRolInContextM
  log ( " rolDef ## typeVanIedereRolInContextM = " <> (show l7))

  l8 <-  rolDef ## iedereRolInContextM >-> rolTypeM
  log ( " rolDef ## iedereRolInContextM >-> rolTypeM = " <> (show l8))

  l9 <-  rolIsVerplichtDef ## rolIsVerplichtM
  log ( " rolIsVerplichtDef ## rolIsVerplichtM = " <> (show l9))

  l10 <-  rolDef ## labelM
  log ( " rolDef ## labelM = " <> (show l10))

  l11 <-  rolIsVerplichtDef ## rangeDefM
  log ( " rolIsVerplichtDef ## rangeDefM = " <> (show l11))

  l12 <-  rolDef ## hasLabelM
  log ( " rolDef ## hasLabelM = " <> (show l12))

  l13 <-  viewDef ## iedereRolInContextM >-> hasBindingM
  log ( " viewDef ## iedereRolInContextM >-> hasBindingM = " <> (show l13))

  log "========================= TESTING TO DEBUG MODEL LOADING ================================"
  l14 <- psp ## typeVanIedereRolInContextM
  log ( "psp ## typeVanIedereRolInContextM = " <> show l14)

  l15 <- psp ## ignoreCache (constructRolGetter "model:Perspectives$rolInContext")
  log ( "psp ## (constructRolGetter 'model:Perspectives$rolInContext') = " <> show l15)

  l16 <- psp ## ignoreCache (constructRolGetter "model:Perspectives$rolInContext") >-> bindingM
  log ( "psp ## (constructRolGetter 'model:Perspectives$rolInContext') >-> bindingM = " <> show l16)

  l17 <-  rolDef ## (constructRolGetter "model:Perspectives$rolInContext")
  log ( "rolDef ## (constructRolGetter 'model:Perspectives$rolInContext') = " <> (show l17))

  l18 <- psp ## ignoreCache iedereRolInContextM
  log ( "psp ## iedereRolInContextM = " <> show l18)

  l19 <- psp ## ignoreCache iedereRolInContextM >-> bindingM
  log ( "psp ## iedereRolInContextM >-> bindingM = " <> show l19)

  -- Dit levert een lege lijst!
  l20 <- "model:Perspectives$rolInContext1" ## bindingM
  log ( "'model:Perspectives$rolInContext1' ## bindingM = " <> show l20)

  l21 <- "model:Perspectives$rolInContext1" ## rolTypeM
  log ( "'model:Perspectives$rolInContext1' ## rolTypeM = " <> show l21)

log :: forall e. String -> MonadPerspectives (console :: CONSOLE | e) Unit
log = lift <<< AC.log
