module Main where

import Test.TestEffects
import Test.TripleAdministration
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Canceler, runAff)

main :: forall e. Eff (CancelerEffects e) (Canceler (CancelerEffects e))
main = runAff handleError handleSuccess test
