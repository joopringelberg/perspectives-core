module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Perspectives.Location
-- test
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show l1)
  log (show (setLocation l1 10))
  log (show (recomputeLocation l2))
