module Main where

import Prelude
import Perspectives.DestructiveMap
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show m1)
  where
    m1 = (createMap unit)
    m2 = setInMap m1 "a" 1
    m3 = setInMap m2 "a" 2
