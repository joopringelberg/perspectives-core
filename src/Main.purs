module Main where


import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Perspectives.Location (locate, THEORYDELTA, runLocation, setLocationValue)

main :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
main = myProgram

myProgram :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
myProgram = let c = locate "This content will be ignored." in
  do
    runLocation $ map log c
    setLocationValue c "Hello world!"
    setLocationValue c "This goes on and on."
