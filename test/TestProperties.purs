module Test.Properties where

import Control.Monad.Aff.Console (log) as AC
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Trans.Class (lift)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (ignoreCache)
import Perspectives.SystemQueries (binding, buitenRol, contextType, hasBinding, hasLabel, identity, isVerplicht, label, range, rolContext, iedereRolInContext, rolType, rolTypen)
import Perspectives.TripleGetter (constructRolGetter, (##))
import Prelude (Unit, bind, discard, show, (<<<), (<>))
import Test.TestEffects (CancelerEffects)

rolDef :: String
rolDef = "model:Perspectives$Rol"

isVerplichtDef :: String
isVerplichtDef = "model:Perspectives$isVerplicht"

viewDef :: String
viewDef = "model:Perspectives$view"

psp :: String
psp = "model:Perspectives"

test :: forall e. MonadPerspectives (CancelerEffects e) Unit
test = do
  log "=========================Test.Properties================================"
  l1 <-  rolDef ## iedereRolInContext
  log ( "rolDef ## iedereRolInContext = " <> (show l1))

  l2 <-  rolDef ## iedereRolInContext >-> binding
  log ( " rolDef ## iedereRolInContext >-> binding = " <> (show l2))

  l3 <-  rolDef ## iedereRolInContext >-> binding >-> rolContext
  log ( " rolDef ## iedereRolInContext >-> binding >-> rolContext = " <> (show l3))

  l4 <-  rolDef ## iedereRolInContext >-> identity
  log ( " rolDef ## iedereRolInContext >-> identity = " <> (show l4))

  l5 <-  rolDef ## iedereRolInContext >-> binding >-> rolContext >-> contextType
  log ( " rolDef ## iedereRolInContext >-> identity >-> contextType = " <> (show l5))

  l6 <-  rolDef ## buitenRol
  log ( " rolDef ## buitenRol = " <> (show l6))

  l7 <-  rolDef ## rolTypen
  log ( " rolDef ## rolTypen = " <> (show l7))

  l8 <-  rolDef ## iedereRolInContext >-> rolType
  log ( " rolDef ## iedereRolInContext >-> rolType = " <> (show l8))

  l9 <-  isVerplichtDef ## isVerplicht
  log ( " isVerplichtDef ## isVerplicht = " <> (show l9))

  l10 <-  rolDef ## label
  log ( " rolDef ## label = " <> (show l10))

  l11 <-  isVerplichtDef ## range
  log ( " isVerplichtDef ## range = " <> (show l11))

  l12 <-  rolDef ## hasLabel
  log ( " rolDef ## hasLabel = " <> (show l12))

  l13 <-  viewDef ## iedereRolInContext >-> hasBinding
  log ( " viewDef ## iedereRolInContext >-> hasBinding = " <> (show l13))

  log "========================= TESTING TO DEBUG MODEL LOADING ================================"
  l14 <- psp ## rolTypen
  log ( "psp ## rolTypen = " <> show l14)

  l15 <- psp ## ignoreCache (constructRolGetter "model:Perspectives$rolInContext")
  log ( "psp ## (constructRolGetter 'model:Perspectives$rolInContext') = " <> show l15)

  l16 <- psp ## ignoreCache (constructRolGetter "model:Perspectives$rolInContext") >-> binding
  log ( "psp ## (constructRolGetter 'model:Perspectives$rolInContext') >-> binding = " <> show l16)

  l17 <-  rolDef ## (constructRolGetter "model:Perspectives$rolInContext")
  log ( "rolDef ## (constructRolGetter 'model:Perspectives$rolInContext') = " <> (show l17))

  l18 <- psp ## ignoreCache iedereRolInContext
  log ( "psp ## iedereRolInContext = " <> show l18)

  l19 <- psp ## ignoreCache iedereRolInContext >-> binding
  log ( "psp ## iedereRolInContext >-> binding = " <> show l19)

  -- Dit levert een lege lijst!
  l20 <- "model:Perspectives$rolInContext1" ## binding
  log ( "'model:Perspectives$rolInContext1' ## binding = " <> show l20)

  l21 <- "model:Perspectives$rolInContext1" ## rolType
  log ( "'model:Perspectives$rolInContext1' ## rolType = " <> show l21)

log :: forall e. String -> MonadPerspectives (console :: CONSOLE | e) Unit
log = lift <<< AC.log
