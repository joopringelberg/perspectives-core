module Main where


import Control.Monad.Eff.Console
import Control.Monad.Eff
import Prelude
import Signal
import Signal.Channel

main :: forall eff. Eff (channel :: CHANNEL, console :: CONSOLE | eff) Unit
main = myProgram

myProgram :: forall eff. Eff (channel :: CHANNEL, console :: CONSOLE | eff) Unit
myProgram = do
  c <- channel "This content will be ignored."
  runSignal $ map log (subscribe c)
  send c "Hello world!"
  send c "This goes on and on."
