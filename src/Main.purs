module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Perspectives.Location
-- test
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show l3)
  log (show (setLocation l1 20))
  log (show (recomputeLocation l3))
