module Main where


import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Perspectives.Location (locate, THEORYDELTA, runLocation, setLocationValue)

main :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
main = myProgram

myProgram :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
myProgram = let
  m = locate "This content will be ignored."
  n = locate 1
  p = map ((+) 10) n
  s = (+) <$> n <*> p
  in
    do
      runLocation $ map log m
      setLocationValue m "Hello world!"
      setLocationValue m "This goes on and on."
      runLocation $ map (log <<< ((<>) "n = ") <<< show) n
      runLocation $ map (log <<< ((<>) "p = ") <<< show) p
      runLocation $ map (log <<< ((<>) "s = ") <<< show) s
      setLocationValue n 10
{-
myProgram
This content will be ignored.
Hello world!
This goes on and on.
n = 1
p = 11
s = 12
s = 21
p = 20
s = 30
n = 10
unit

  Hier zien we het depth-first effect. Zodra n de waarde 10 krijgt, wordt die het netwerk ingestuurd en wordt s 21 (als som van 10 en 11).
  Pas daarna wordt p 20 en dan wordt s uiteindelijk 30.
  Wat we willen is dat de deltas breadth-first verspreid worden, zodat éérst p 20 wordt en dàn pas s berekend wordt als 30.
-}
