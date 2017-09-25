module Test.Memoization where

import Prelude
import Perspectives.PropertyComposition
import Perspectives.SystemQueries
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log) as Aff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Perspectives.Location (Location, locate, locationValue)
import Perspectives.LocationT (runLocationT)
import Perspectives.Property (SingleGetter, MemorizingSingleGetter)
import Perspectives.Resource (representResource)
import Perspectives.ResourceTypes (Resource)

{-
Use this test to check in the Chrome debugger that memoizing functions do indeed retrieve a value.
-}
test = launchAff do
  log "=========================Test.Memoization================================"
  log "=========================MemoizingSingleGetter================================"
  (gbLoc :: Location (Maybe Resource)) <- liftEff $ representResource "user:xGebruiker"
  (l :: Location (Maybe String)) <-  label gbLoc
  -- Check in traverseLoc.
  (l' :: Location (Maybe String)) <-  label gbLoc
  log ( "label user:xGebruiker = " <> (show l))

  log "=========================MemoizingPluralGetter================================"
  (m :: Location (Array String)) <-  (subClassOf >>-> label) gbLoc
  -- Check in traverseLoc.
  (m' :: Location (Array String)) <-  (subClassOf >>-> label) gbLoc
  log ( "label user:xGebruiker = " <> (show m))

  log "=========================memoizeMonadicFunction================================"
  (n :: Location (Array Resource)) <-  types gbLoc
  -- Check in memoizeMonadicFunction.
  (n' :: Location (Array Resource)) <-  types gbLoc
  log ( "label user:xGebruiker = " <> (show n))

  log "=========================memoizeMonadicFunction================================"
  (o :: Location (Array Resource)) <-  typeSuperClasses gbLoc
  -- Check in memoizeMonadicFunction.
  (o' :: Location (Array Resource)) <-  typeSuperClasses gbLoc
  log ( "label user:xGebruiker = " <> (show o))

  log "=========================hasValue================================"
  (p :: Location (Maybe Boolean)) <-  hasLabel gbLoc
  -- Check in memoizeMonadicFunction.
  (p' :: Location (Maybe Boolean)) <-  hasLabel gbLoc
  (p'' :: Location (Maybe Boolean)) <-  hasBinding gbLoc
  log ( "label user:xGebruiker = " <> (show p))

log = Aff.log
