module Main where


import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Perspectives.Resource (resourceLocation, j)

main :: forall eff. Eff ( console :: CONSOLE | eff) Unit
main = do
        log ( show (resourceLocation j))
