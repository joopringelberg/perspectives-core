module Test.GlobalUnsafeStrMap where


import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new, poke, peek)

myMap :: GLStrMap Int
myMap = new unit


test :: forall e. Eff (console :: CONSOLE, gm :: GLOBALMAP | e) Unit
test = do
  log "========================================================="
  log "poke myMap \"een\" 1'"
  _ <- poke myMap "een" 1
  v <- peek myMap "een"
  log ("lookup myMap \"een\"'= " <> show v)
  log "========================================================="
