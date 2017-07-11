module Main where


import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Perspectives.Location (locate, THEORYDELTA, runLocation, setLocationValue, setLocationValue', pureTHEORYDELTA, runTHEORYDELTA)

main :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
main = myProgram4

myProgram :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
myProgram = let
  m = locate "This content will be ignored."
  n = locate 1
  p = map ((+) 10) n
  s = (+) <$> n <*> p
  in
    do                                                        -- 'do' runs each computation below, that respectively:
      runLocation $ map log m                                 -- runs the log function on the value in the location
      setLocationValue m "Hello world!"                       -- changes the theory by setting the value in the location
      setLocationValue m "This goes on and on."
      runLocation $ map (log <<< ((<>) "n = ") <<< show) n    -- runs the log function on the labeled value
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
{-
Elke regel in de do-expressie van myProgram levert een computation op (Een unit waarde in de Eff Monad).
Main executeert elk van deze computations.
-}

myProgram2 :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
myProgram2 = let
  m = locate "This content will be ignored."
  n = locate 1
  p = map ((+) 10) n
  s = (+) <$> n <*> p
  locationsWithEffectfulComputations = map (map (log <<< show)) [n, p, s]
  in
    do                                                        -- 'do' runs each computation below, that respectively:
      runLocation $ map log m                                 -- runs the log function on the value in the location
      setLocationValue m "Hello world!"                       -- changes the theory by setting the value in the location
      setLocationValue m "This goes on and on."
      foreachE locationsWithEffectfulComputations runLocation
      setLocationValue n 10

myProgram3 :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
myProgram3 = let
  m = locate "This content will be ignored."
  ignore = pureTHEORYDELTA (setLocationValue' m "Hello world")
  in
    do
      runLocation $ map log m

myProgram4 :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
myProgram4 = let
    m = locate "This content will be ignored."
    p :: forall e. Eff (td :: THEORYDELTA, console :: CONSOLE | e) Unit
    p = do
          runLocation $ map log m
          (setLocationValue m "Hello world")
  in runTHEORYDELTA p

myProgram5 :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
myProgram5 = let
    m = locate "This content will be ignored."
    in runTHEORYDELTA do
          runLocation $ map log m
          (setLocationValue m "Hello world")

myProgram6 :: forall eff. Eff (td :: THEORYDELTA, console :: CONSOLE | eff) Unit
myProgram6 = let
  n = locate 1
  p = map ((+) 10) n
  s = (+) <$> n <*> p
  locationsWithEffectfulComputations = map (map (log <<< show)) [n, p, s]
  in runTHEORYDELTA do
      foreachE locationsWithEffectfulComputations runLocation
      setLocationValue n 10
{-
10
20
30
unit
En dit is een demonstratie van breadth-first!
-}
