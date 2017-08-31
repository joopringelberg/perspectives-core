module Test.GlobalUnsafeStrMap where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)

myMap :: forall r. Eff (gm :: GLOBALMAP | r) (GLStrMap Int)
myMap = new


test :: forall e. Eff (console :: CONSOLE, gm :: GLOBALMAP | e) Unit
test = do
  log "========================================================="
  m <- myMap
  log "poke myMap \"een\" 1'"
  _ <- poke m "een" 1
  v <- peek m "een"
  log ("lookup myMap \"een\"'= " <> show v)
  log "========================================================="
